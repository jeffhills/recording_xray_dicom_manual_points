ORTHANC_URL <- Sys.getenv("ORTHANC_URL", "https://means.onefit-medical.com:4343")
ORTHANC_USER <- Sys.getenv("ORTHANC_USER", "eos")
ORTHANC_PASS <- Sys.getenv("ORTHANC_PASS", "demo456**")

# Insecure on purpose (matches your earlier curl -k); replace with valid certs in prod
insecure_tls <- config(ssl_verifypeer = 0L, ssl_verifyhost = 0L)

# Helpers ----
orthanc_get <- function(path, ...) {
  GET(paste0(ORTHANC_URL, path),
      authenticate(ORTHANC_USER, ORTHANC_PASS),
      insecure_tls,
      ...)
}

orthanc_post <- function(path, body_list, ...) {
  POST(paste0(ORTHANC_URL, path),
       authenticate(ORTHANC_USER, ORTHANC_PASS),
       insecure_tls,
       encode = "json",
       body = body_list,
       ...)
}

# Find all DX instances for a given PatientID
find_dx_instances <- function(patient_id) {
  # Ask Orthanc: return instance IDs for this PatientID and DX modality
  body <- list(
    Level = "Instance",
    Query = list(
      PatientID = patient_id,
      Modality  = "DX"
    )
  )
  r <- orthanc_post("/tools/find", body)
  stop_for_status(r)
  ids <- content(r, as = "parsed", simplifyVector = TRUE)
  # If Orthanc returns objects (rare if server is customized), coerce to IDs
  if (is.list(ids) && length(ids) && !is.character(ids[[1]])) {
    ids <- vapply(ids, function(x) x$ID %||% NA_character_, character(1))
    ids <- ids[!is.na(ids)]
  }
  ids
}


# Pull instance metadata (for label inference)
get_instance_meta <- function(instance_id) {
  r <- orthanc_get(paste0("/instances/", instance_id))
  stop_for_status(r)
  content(r, as = "parsed", simplifyVector = TRUE)
}

# Infer AP vs LAT from tags we’ve seen on your server
infer_label <- function(meta) {
  mt <- meta$MainDicomTags
  vals <- tolower(paste(
    mt$ImageComments %||% "",
    mt$SeriesDescription %||% "",
    mt$ViewPosition %||% ""
  ))
  if (grepl("\\blat(eral)?\\b", vals)) return("LAT")
  if (grepl("\\bfrontal\\b",  vals))   return("AP")   # "EOS Frontal" -> AP
  if (grepl("\\bap\\b",       vals))   return("AP")
  if (grepl("\\bpa\\b",       vals))   return("AP")   # tweak if you prefer PA distinct
  return("instance")
}

# Unique filename like AP.dcm, LAT.dcm, AP_1.dcm ...
unique_dcm_path <- function(folder, base) {
  i <- 0L
  repeat {
    suffix <- if (i == 0L) "" else paste0("_", i)
    candidate <- file.path(folder, paste0(base, suffix, ".dcm"))
    if (!file.exists(candidate)) return(candidate)
    i <- i + 1L
  }
}

# Download one instance to dest path
download_instance <- function(instance_id, dest_path) {
  r <- orthanc_get(paste0("/instances/", instance_id, "/file"),
                   write_disk(dest_path, overwrite = FALSE))
  stop_for_status(r)
  dest_path
}

fetch_one_lat_instance <- function(patient_id, fallback_to_ap = FALSE) {
  # Ask Orthanc for *expanded* instance results + key tags to sort on
  body <- list(
    Level = "Instance",
    Expand = TRUE,
    Query  = list(PatientID = patient_id, Modality = "DX"),
    RequestedTags = c("StudyDate","SeriesDescription","ImageComments","ViewPosition","InstanceNumber")
  )
  r <- orthanc_post("/tools/find", body)
  stop_for_status(r)
  res <- content(r, as = "parsed", simplifyVector = FALSE)
  
  # If server didn't expand, fetch per-instance
  if (length(res) && is.character(res[[1]])) {
    ids <- unlist(res, use.names = FALSE)
    res <- lapply(ids, function(id) {
      m <- get_instance_meta(id)
      list(ID = id, MainDicomTags = m$MainDicomTags)
    })
  }
  
  # Label each candidate and split LAT vs others
  lab <- function(x) infer_label(list(MainDicomTags = x$MainDicomTags))
  candidates_lat <- Filter(function(x) identical(lab(x), "LAT"), res)
  candidates_ap  <- Filter(function(x) !identical(lab(x), "LAT"), res)
  
  # Sorting helpers
  as_int <- function(x) {
    if (is.null(x) || !nzchar(x)) return(NA_integer_)
    suppressWarnings(as.integer(x))
  }
  rank_key <- function(x) {
    mt <- x$MainDicomTags
    c(as_int(mt$StudyDate), as_int(mt$InstanceNumber))  # StudyDate desc, then InstanceNumber desc
  }
  
  pick_best <- function(lst) {
    if (!length(lst)) return(NULL)
    ord <- order(
      vapply(lst, function(x) rank_key(x)[1], integer(1)), decreasing = TRUE, na.last = TRUE
    )
    # break ties with InstanceNumber
    tie_sorted <- lst[ord]
    if (length(tie_sorted) > 1) {
      ord2 <- order(
        vapply(tie_sorted, function(x) rank_key(x)[2], integer(1)), decreasing = TRUE, na.last = TRUE
      )
      tie_sorted <- tie_sorted[ord2]
    }
    tie_sorted[[1]]
  }
  
  best <- pick_best(candidates_lat)
  if (is.null(best) && fallback_to_ap) best <- pick_best(candidates_ap)
  if (is.null(best)) return(NULL)
  list(id = best$ID, meta = list(MainDicomTags = best$MainDicomTags))
}

# Save to a deterministic file name (overwrite allowed)
save_one_lat <- function(instance_id, patient_id) {
  patient_dir <- file.path("dicoms", patient_id)
  dir.create(patient_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(patient_dir, "LAT.dcm")
  r <- orthanc_get(paste0("/instances/", instance_id, "/file"),
                   write_disk(dest, overwrite = TRUE))
  stop_for_status(r)
  normalizePath(dest)
}

get_hdr_df <- function(hdr) if (is.list(hdr) && !is.data.frame(hdr)) hdr[[1]] else hdr
get_tag <- function(hdr, name) {
  df <- get_hdr_df(hdr); v <- df$value[df$name == name]
  if (length(v)) as.character(v[[1]]) else NA_character_
}



`%||%` <- function(x, y) if (is.null(x)) y else x

parse_nums <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[[1]])) return(numeric())
  as.numeric(strsplit(as.character(x[[1]]), "\\\\|,|\\s+")[[1]])
}

# robust first-number getter
first_num <- function(x) {
  v <- parse_nums(x)
  if (length(v) >= 1 && is.finite(v[1])) v[1] else NA_real_
}

# this clears your existing overlay + recorded points
clear_click_state <- function() {
  click_coord_reactive_list$coords <- list()
  plot_points_coordinates_reactiveval(list())
  session$sendCustomMessage("plot-coordinates", list(coords = list()))
}

to_data_url_png <- function(raw_png) paste0("data:image/png;base64,", base64enc::base64encode(raw_png))

# Single entry point to process a LOCAL DICOM path as if uploaded
process_local_dicom <- function(path,
                                pixel_spacing_reactive_values,
                                spine_orientation,
                                patient_id_reactive_val,
                                session,
                                long_edge_height = 1650) {
  
  dcm <- tryCatch(oro.dicom::readDICOMFile(path),
                  error = function(e) { showNotification(paste("DICOM read failed:", conditionMessage(e)),
                                                         type = "error"); NULL })
  if (is.null(dcm)) return(invisible(NULL))   # ← no validate() here
  
  img <- dcm$img
  hdr <- dcm$hdr
  
  # rescale + window
  rs <- suppressWarnings(parse_nums(get_tag(hdr, "RescaleSlope"))[1]);      if (is.finite(rs)) img <- img * rs
  ri <- suppressWarnings(parse_nums(get_tag(hdr, "RescaleIntercept"))[1]);  if (is.finite(ri)) img <- img + ri
  wc <- suppressWarnings(parse_nums(get_tag(hdr, "WindowCenter"))[1])
  ww <- suppressWarnings(parse_nums(get_tag(hdr, "WindowWidth"))[1])
  if (is.finite(wc) && is.finite(ww) && ww > 0) {
    low <- wc - ww/2; high <- wc + ww/2
    img[img < low] <- low; img[img > high] <- high
  }
  
  # normalize to [0,1]
  rng <- range(img, finite = TRUE)
  img01 <- if (diff(rng) > 0) (img - rng[1]) / diff(rng) else matrix(0, nrow(img), ncol(img))
  
  # invert if MONOCHROME1
  pm <- tolower(get_tag(hdr, "PhotometricInterpretation"))
  if (identical(pm, "monochrome1")) img01 <- 1 - img01
  
  # PixelSpacing (mm per ORIGINAL pixel)
  ps <- parse_nums(get_tag(hdr, "PixelSpacing"))
  if (length(ps) < 2 || any(!is.finite(ps[1:2]))) ps <- parse_nums(get_tag(hdr, "ImagerPixelSpacing"))
  
  # compress uniformly to a target long edge (no upscaling)
  old_rows <- nrow(img01); old_cols <- ncol(img01)
  target_long_edge <- long_edge_height
  scale <- min(1, target_long_edge / max(old_cols, old_rows))
  
  rc <- rawConnection(raw(), "wb"); png::writePNG(img01, target = rc); raw_png <- rawConnectionValue(rc); close(rc)
  im <- magick::image_read(raw_png)
  if (scale < 1) im <- magick::image_resize(im, sprintf("%d%%", as.integer(round(scale * 100))))
  im <- magick::image_flip(im)  # keep superior at top for browser display
  out <- magick::image_write(im, format = "png")
  session$sendCustomMessage("load-image", list(src = to_data_url_png(out)))
  
  
  # store adjusted spacing (mm per DISPLAY pixel)
  if (length(ps) >= 2 && all(is.finite(ps[1:2]))) {
    pixel_spacing_reactive_values$spacing <- c(ps[1] / scale, ps[2] / scale)
  } else {
    pixel_spacing_reactive_values$spacing <- NULL
    showNotification("No PixelSpacing present; distances will be in pixels.", type = "warning", duration = 6)
  }
  
  # orientation heuristic
  if (str_detect(get_tag(hdr, "PatientOrientation"), "P")) {
    spine_orientation("left")
  } else {
    spine_orientation("right")
  }
  
  # capture PatientID if available
  pid <- get_tag(hdr, "PatientID")
  if (!is.null(pid) && !is.na(pid) && nzchar(pid)) {
    patient_id_reactive_val(trimws(as.character(pid)))
  }
  
  # reset your points store
  pixel_spacing_reactive_values$pts <- data.frame(point = integer(), x_px = numeric(), y_px = numeric(),
                                                  x_mm = numeric(), y_mm = numeric(), stringsAsFactors = FALSE)
  updateCheckboxInput(session, "xray_file_uploaded", value = TRUE)
}



process_image_path <- function(path, name = NULL, mime = NULL,
                               session,
                               pixel_spacing_reactive_values,
                               spine_orientation,
                               patient_id_reactive_val,
                               clear_click_state) {
  name <- tolower(name %||% basename(path))
  mime <- mime %||% ""
  
  is_dicom <- grepl("\\.(dcm|dicom)$", name) ||
    mime %in% c("application/dicom", "application/dicom+json", "application/octet-stream") ||
    looks_like_dicom(path)
  
  to_data_url_png <- function(raw_png) paste0("data:image/png;base64,", base64enc::base64encode(raw_png))
  
  if (is_dicom) {
    dcm <- tryCatch(oro.dicom::readDICOMFile(path),
                    error = function(e) { showNotification(paste("DICOM read failed:", conditionMessage(e)), type="error"); NULL })
    if (is.null(dcm)) return(invisible(NULL))
    
    img <- dcm$img; hdr <- dcm$hdr
    
    rs <- suppressWarnings(first_num(get_tag(hdr,"RescaleSlope")));     if (is.finite(rs)) img <- img * rs
    ri <- suppressWarnings(first_num(get_tag(hdr,"RescaleIntercept"))); if (is.finite(ri)) img <- img + ri
    wc <- suppressWarnings(first_num(get_tag(hdr,"WindowCenter")))
    ww <- suppressWarnings(first_num(get_tag(hdr,"WindowWidth")))
    if (is.finite(wc) && is.finite(ww) && ww > 0) { low <- wc - ww/2; high <- wc + ww/2; img[img<low] <- low; img[img>high] <- high }
    
    rng <- range(img, finite = TRUE)
    img01 <- if (diff(rng) > 0) (img - rng[1]) / diff(rng) else matrix(0, nrow(img), ncol(img))
    
    if (identical(tolower(get_tag(hdr,"PhotometricInterpretation")), "monochrome1")) img01 <- 1 - img01
    
    ps <- parse_nums(get_tag(hdr,"PixelSpacing"))
    if (length(ps)<2 || any(!is.finite(ps[1:2]))) ps <- parse_nums(get_tag(hdr,"ImagerPixelSpacing"))
    
    fov_h <- suppressWarnings(first_num(get_tag(hdr,"FieldOfViewDimensions")))
    target_long_edge <- if (is.finite(fov_h)) fov_h else 1650
    
    old_rows <- nrow(img01); old_cols <- ncol(img01)
    scale <- min(1, target_long_edge / max(old_cols, old_rows))
    
    rc <- rawConnection(raw(), "wb"); png::writePNG(img01, target = rc); raw_png <- rawConnectionValue(rc); close(rc)
    im <- magick::image_read(raw_png)
    if (scale < 1) im <- magick::image_resize(im, sprintf("%d%%", as.integer(round(scale*100))))
    im <- magick::image_flip(im)
    out <- magick::image_write(im, format = "png")
    session$sendCustomMessage("load-image", list(src = to_data_url_png(out)))
    
    # set spacing in the provided reactiveValues
    if (length(ps) >= 2 && all(is.finite(ps[1:2]))) {
      pixel_spacing_reactive_values$spacing <- c(ps[1] / scale, ps[2] / scale)
    } else {
      pixel_spacing_reactive_values$spacing <- NULL
      showNotification("No PixelSpacing present; distances will be in pixels.", type="warning", duration=6)
    }
    
    if (stringr::str_detect(get_tag(hdr,"PatientOrientation"), "P")) spine_orientation("left") else spine_orientation("right")
    
    pid <- get_tag(hdr, "PatientID")
    if (!is.null(pid) && !is.na(pid) && nzchar(pid)) patient_id_reactive_val(trimws(as.character(pid)))
    
    pixel_spacing_reactive_values$pts <- pixel_spacing_reactive_values$pts[0, , drop = FALSE]
    clear_click_state()
    updateCheckboxInput(session, "xray_file_uploaded", value = TRUE)
    
  } else {
    b64 <- base64enc::base64encode(path)
    mime_out <- if (grepl("^image/", mime)) mime else "image/*"
    session$sendCustomMessage("load-image", list(src = paste0("data:", mime_out, ";base64,", b64)))
    pixel_spacing_reactive_values$spacing <- NULL
    pixel_spacing_reactive_values$pts <- pixel_spacing_reactive_values$pts[0, , drop = FALSE]
    clear_click_state()
    updateCheckboxInput(session, "xray_file_uploaded", value = TRUE)
  }
}
