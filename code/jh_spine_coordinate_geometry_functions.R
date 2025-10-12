
# predict_postop_pt_function <- function(preop_pt = 25.203136,
#                                        preop_c2_tilt = 1.5912083,
#                                        postop_c2pa = 19.231308) {
#   -0.49752004+0.10970428* preop_pt+0.00056298497*pmax(preop_pt-10.719671,0)^3-0.0038851919*pmax(preop_pt-20.0131,0)^3+0.0065365808*pmax(preop_pt-25.203136,0)^3-0.0038450855*pmax(preop_pt-31.171827,0)^3+0.00063071159*pmax(preop_pt-42.548329,0)^3-0.44307495* preop_c2_tilt+0.0017666557*pmax(preop_c2_tilt+5.8137583,0)^3-0.0035561278*pmax(preop_c2_tilt+1.7091905,0)^3+0.0016668741*pmax(preop_c2_tilt-1.5912083,0)^3+2.5888476e-05*pmax(preop_c2_tilt-5.5568369,0)^3+9.6709482e-05*pmax(preop_c2_tilt-14.441265,0)^3+0.92251864* postop_c2pa-0.00030340652*pmax(postop_c2pa-3.9065663,0)^3+0.0011766656*pmax(postop_c2pa-13.597867,0)^3-0.0014909534*pmax(postop_c2pa-19.231308,0)^3+0.00079454989*pmax(postop_c2pa-25.269772,0)^3-0.0001768555*pmax(postop_c2pa-35.169804,0)^3 
# }

jh_compute_pelvic_incidence_from_3_coord_function <- function(fem_head_coord = "fem_head_center",
                                                              s1_sa_coord   = "sacrum_superior_anterior",
                                                              s1_sp_coord  = "sacrum_superior_posterior") {
  # Pull points
  fh <- fem_head_coord
  sa <- s1_sa_coord
  sp <- s1_sp_coord
  stopifnot(length(fh)==2, length(sa)==2, length(sp)==2)
  
  # Midpoint of S1 superior endplate
  s1_mid <- (sa + sp) / 2
  
  # Endplate direction (posterior->anterior or anterior->posterior works;
  # we'll take the absolute angle later so direction won't matter)
  v_end <- sa - sp
  len_end <- sqrt(sum(v_end^2))
  if (len_end == 0) stop("S1 endplate points are identical; cannot define endplate.")
  u_end <- v_end / len_end
  
  # One perpendicular unit vector to the endplate (the other is its negative)
  u_perp <- c(-u_end[2], u_end[1])
  
  # Vector from midpoint to femoral head center
  v_mid_fh <- fh - s1_mid
  len_fh <- sqrt(sum(v_mid_fh^2))
  if (len_fh == 0) stop("Femoral head center coincides with S1 midpoint; PI undefined.")
  u_mid_fh <- v_mid_fh / len_fh
  
  # Angle between perpendicular and midpoint→FH line (0–90° typical)
  angle_rad <- acos( abs( sum(u_perp * u_mid_fh) ) )
  angle_deg <- angle_rad * 180 / pi
  
  angle_deg
  
}

jh_return_coord_vector_from_coord_tibble_function <- function(df, level_input, spine_marker_input){
  
  unname(c(df %>%
             filter(level == level_input, spine_marker == spine_marker_input) %>%
             pull(x), 
           df %>%
             filter(level == level_input, spine_marker == spine_marker_input) %>%
             pull(y)) )
}

jh_add_missing_inferior_corners <- function(df,
                                            percent = 0.2,
                                            calc_fun = jh_calculate_point_along_line_function) {
  stopifnot(all(c("level","spine_marker","x","y") %in% names(df)))
  stopifnot(is.factor(df$level))
  
  levs <- levels(df$level)
  
  # helper: previous level name in factor ordering (immediately inferior)
  prev_level_name <- function(lvl_char) {
    i <- match(lvl_char, levs)
    if (is.na(i) || i <= 1) return(NA_character_)
    levs[i - 1]
  }
  
  # mark which levels have which markers
  by_level <- df %>%
    mutate(marker_class = case_when(
      str_starts(spine_marker, "inferior_") ~ "has_inferior",
      str_starts(spine_marker, "superior_") ~ "has_superior",
      TRUE ~ "other"
    )) %>%
    group_by(level) %>%
    summarize(
      has_inf = any(marker_class == "has_inferior"),
      has_sup_post = any(spine_marker == "superior_posterior"),
      has_sup_ant  = any(spine_marker == "superior_anterior"),
      .groups = "drop"
    )
  
  # target levels: have superior (both corners) but no inferior
  targets <- by_level %>%
    filter(!has_inf, has_sup_post, has_sup_ant) %>%
    pull(level) %>%
    as.character()
  
  # Build a quick accessor for a given (level, marker) -> c(x,y)
  get_xy <- function(level_chr, marker_chr) {
    r <- df %>% filter(as.character(level) == level_chr,
                       spine_marker == marker_chr)
    if (nrow(r) != 1) return(NULL)
    c(r$x, r$y)
  }
  
  new_rows <- map_dfr(targets, function(lvl_chr) {
    prev_chr <- prev_level_name(lvl_chr)
    # need previous level’s superior corners
    if (is.na(prev_chr)) return(tibble())  # nothing to do
    
    prev_sup_post <- get_xy(prev_chr, "superior_posterior")
    prev_sup_ant  <- get_xy(prev_chr, "superior_anterior")
    this_sup_post <- get_xy(lvl_chr,  "superior_posterior")
    this_sup_ant  <- get_xy(lvl_chr,  "superior_anterior")
    
    # if any needed corner missing, skip safely
    if (is.null(prev_sup_post) || is.null(prev_sup_ant) ||
        is.null(this_sup_post) || is.null(this_sup_ant)) {
      return(tibble())
    }
    
    # compute inferior points at 10% along prev_sup -> this_sup
    inf_post <- calc_fun(coord_a = prev_sup_post,
                         coord_b = this_sup_post,
                         percent_a_to_b = percent)
    inf_ant  <- calc_fun(coord_a = prev_sup_ant,
                         coord_b = this_sup_ant,
                         percent_a_to_b = percent)
    
    tibble(
      level = factor(c(lvl_chr, lvl_chr), levels = levs),
      spine_marker = c("inferior_posterior", "inferior_anterior"),
      x = c(inf_post[1], inf_ant[1]),
      y = c(inf_post[2], inf_ant[2]),
      .computed = TRUE
    )
  })
  
  # bind and arrange in a sensible row order
  out <- bind_rows(
    df %>% mutate(.computed = FALSE),
    new_rows
  ) %>%
    arrange(
      level,
      factor(spine_marker,
             levels = c("inferior_anterior", "inferior_posterior",
                        "superior_posterior","superior_anterior"))
    ) %>%
    select(-.computed)
  
  out
}



####### COMPUTE CENTROIDS ##########

# Expected markers (one each per level)
.corner_order <- c("superior_anterior", "superior_posterior",
                   "inferior_posterior", "inferior_anterior")

# Shoelace centroid for a closed polygon (x, y as numeric vectors of equal length)
.poly_centroid <- function(x, y) {
  # close polygon
  x2 <- c(x, x[1]); y2 <- c(y, y[1])
  cross <- x2[-length(x2)] * y2[-1] - x2[-1] * y2[-length(y2)]
  A <- 0.5 * sum(cross)
  if (abs(A) < .Machine$double.eps) {
    # degenerate polygon; fall back to simple mean of vertices
    return(c(mean(x), mean(y), area = 0))
  }
  Cx <- (1 / (6 * A)) * sum((x2[-length(x2)] + x2[-1]) * cross)
  Cy <- (1 / (6 * A)) * sum((y2[-length(y2)] + y2[-1]) * cross)
  c(Cx, Cy, area = A)
}



jh_compute_vertebra_centroids <- function(df, spine_orientation = c("right","left")) {
  stopifnot(all(c("level","spine_marker","x","y") %in% names(df)))
  spine_orientation <- match.arg(spine_orientation)
  
  fem_head_center <- jh_return_coord_vector_from_coord_tibble_function(df = df, level_input = "pelvis", spine_marker_input = "fem_head_center")
  
  stopifnot(is.numeric(fem_head_center), length(fem_head_center) == 2)
  
  levs <- levels(df$level)
  fhx  <- fem_head_center[1]
  fhy  <- fem_head_center[2]
  
  s1_sup_post <- jh_return_coord_vector_from_coord_tibble_function(df = df, level_input = "sacrum", spine_marker_input = "superior_posterior")
  s1_sup_ant <- jh_return_coord_vector_from_coord_tibble_function(df = df, level_input = "sacrum", spine_marker_input = "superior_anterior")
  
  s1_mid <- jh_calculate_point_along_line_function(coord_a = s1_sup_ant, coord_b = s1_sup_post, percent_a_to_b = 0.5)
  
  centroids_df <- df %>%
    dplyr::filter(spine_marker %in% .corner_order) %>%
    dplyr::group_by(level) %>%
    # ensure we have all 4 corners
    dplyr::filter(dplyr::n_distinct(spine_marker) == 4) %>%
    dplyr::arrange(match(spine_marker, .corner_order), .by_group = TRUE) %>%
    dplyr::summarise(
      {
        cxy <- .poly_centroid(x, y)
        tibble::tibble(
          centroid_x = cxy[1],
          centroid_y = cxy[2]
        )
      },
      .groups = "drop"
    ) %>%
    dplyr::mutate(level = factor(level, levels = levs)) 
  
  centroids_tilts_df <- centroids_df %>%
    mutate(centroid_x = if_else(level == "sacrum", s1_mid[1], centroid_x), 
           centroid_y = if_else(level == "sacrum", s1_mid[2], centroid_y)) %>%
    # compute signed vertebral tilt vs vertical line through femoral head
    dplyr::mutate(
      vx = centroid_x - fhx,
      vy = centroid_y - fhy,
      # angle from vertical (0..pi). Using atan2(|vx|, vy) is robust when vy ≤ 0.
      tilt_deg_abs = atan2(abs(vx), vy) * 180 / pi,
      # sign rule:
      #  - if orientation == "right": negative when centroid_x < fhx
      #  - if orientation == "left":  negative when centroid_x > fhx
      sign_mult = dplyr::case_when(
        spine_orientation == "right" & centroid_x < fhx ~ -1,
        spine_orientation == "right" & centroid_x >= fhx ~  1,
        spine_orientation == "left"  & centroid_x >  fhx ~ -1,
        TRUE ~ 1
      ),
      vertebral_tilt = sign_mult * tilt_deg_abs
    ) %>%
    dplyr::select(level, centroid_x, centroid_y, vertebral_tilt) %>%
    mutate(vertebral_tilt = if_else(level == "sacrum", vertebral_tilt*-1, vertebral_tilt))
  
  pelvic_tilt_computed <- (centroids_tilts_df %>%
                    filter(level == "sacrum"))$vertebral_tilt 
  
  centroids_tilts_df %>%
    mutate(pelvic_tilt = pelvic_tilt_computed) %>%
    mutate(vertebral_pelvic_angle = pelvic_tilt + vertebral_tilt) %>%
    filter(level != "sacrum")
}



###### SMOOTH SPINE COORDINATES ######

jh_smooth_spine_vertebral_coordinates_function <- function(dat,
                               spar = 0.4,
                               df_spline = NULL,
                               jitter_y = 0,
                               aggregate_ties = FALSE) {
  
  stopifnot(all(c("level","spine_marker","x","y") %in% names(dat)))
  
  # ---- 1) Build fitting set: your exact filter ----
  fit_mask <- (!dat$level %in% c("pelvis", "skull", "c1", "sacrum")) &
    (dat$level != "sacrum" | !str_detect(dat$spine_marker, "inferior")) 
  ap_group <- case_when(
    str_detect(dat$spine_marker, "anterior$") ~ "anterior",
    str_detect(dat$spine_marker, "posterior$") ~ "posterior",
    TRUE ~ NA_character_
  )
  
  df_fit <- dat[fit_mask & !is.na(ap_group), , drop = FALSE] %>%
    mutate(ap_group = ap_group[fit_mask & !is.na(ap_group)])
  
  if (!nrow(df_fit)) return(dat)  # nothing to do
  
  # Optional: stabilize by averaging duplicates of y within each A/P group
  if (isTRUE(aggregate_ties)) {
    df_fit <- df_fit %>%
      group_by(ap_group, y) %>%
      summarise(x = mean(x), .groups = "drop")
  }
  
  if (jitter_y > 0) {
    set.seed(1L)
    df_fit$y <- df_fit$y + runif(nrow(df_fit), -jitter_y, jitter_y)
  }
  
  # ---- 2) Fit one spline per A/P group (x ~ y) ----
  fit_group <- function(dat_grp) {
    dat_grp <- arrange(dat_grp, y)
    if (!is.null(df_spline)) {
      stats::smooth.spline(x = dat_grp$y, y = dat_grp$x, df = df_spline, all.knots = TRUE)
    } else {
      stats::smooth.spline(x = dat_grp$y, y = dat_grp$x, spar = spar, all.knots = TRUE)
    }
  }
  
  fit_ant  <- df_fit %>% filter(ap_group == "anterior")  %>% { if (nrow(.)>=3) fit_group(.) else NULL }
  fit_post <- df_fit %>% filter(ap_group == "posterior") %>% { if (nrow(.)>=3) fit_group(.) else NULL }
  
  # ---- 3) Predict x̂ for ALL rows used to fit; snap x to x̂ ----
  out <- dat
  
  idx_ant  <- which(fit_mask & ap_group == "anterior")
  idx_post <- which(fit_mask & ap_group == "posterior")
  
  if (length(idx_ant) && !is.null(fit_ant)) {
    out$x[idx_ant] <- stats::predict(fit_ant,  x = out$y[idx_ant])$y
  }
  if (length(idx_post) && !is.null(fit_post)) {
    out$x[idx_post] <- stats::predict(fit_post, x = out$y[idx_post])$y
  }
  
  out
}








predict_postop_pt_function <-  function(preop_c2_tilt = 1.5912083,
                                        preop_pt = 25.203136,
                                        postop_c2pa = 19.231308) {
  -0.018071724-0.26959853*preop_c2_tilt+0.17554658*preop_pt+0.83058112*postop_c2pa 
}

jh_find_sacrum_inf_point_function <- function(s1_posterior_sup = c(0,0), 
                                              s1_anterior_sup = c(1, 1), 
                                              spine_facing = "right", 
                                              inf_length_multiplier = 2.5) {
  
  if(s1_posterior_sup[[1]]> s1_anterior_sup[[1]]){
    spine_orientation <- "left"
  }else{
    spine_orientation <- "right"
  }
  
  
  inf_s1_line_length <- jh_calculate_distance_between_2_points_function(s1_posterior_sup, s1_anterior_sup)*inf_length_multiplier
  
  s1_center <- jh_get_point_along_line_function(coord_a = s1_posterior_sup,
                                                coord_b = s1_anterior_sup, percent_a_to_b = 0.5)
  
  
  # Extract coordinates for points A and B
  x1 <- s1_posterior_sup[1]
  y1 <- s1_posterior_sup[2]
  x2 <- s1_anterior_sup[1]
  y2 <- s1_anterior_sup[2]
  
  # Calculate the slope of sacrum
  dx <- x2 - x1
  dy <- y2 - y1
  
  # Check for vertical line case (when dx = 0)
  if (dx == 0) {
    # Line is vertical, so perpendicular line is horizontal
    # Determine the direction based on spine_facing
    if (spine_orientation == "right") {
      inferior_sacrum <- c(s1_center[1] + inf_s1_line_length, s1_center[2])
    } else {
      inferior_sacrum <- c(s1_center[1] - inf_s1_line_length, s1_center[2])
    }
  } else {
    # Calculate the perpendicular slope
    perp_slope <- -dx / dy
    
    # Calculate the angle of the perpendicular line
    angle_perp <- atan(perp_slope)
    
    # Determine direction based on spine_facing
    if (spine_facing == "right") {
      inferior_sacrum_x <- s1_center[1] - inf_s1_line_length * cos(angle_perp)
      inferior_sacrum_y <- s1_center[2] - abs(inf_s1_line_length * sin(angle_perp))
    } else {
      inferior_sacrum_x <- s1_center[1] + inf_s1_line_length * cos(angle_perp)
      inferior_sacrum_y <- s1_center[2] - abs(inf_s1_line_length * sin(angle_perp))
    }
    
    # Set the coordinates for inferior_sacrum
    inferior_sacrum <- c(x = inferior_sacrum_x, y = inferior_sacrum_y)
  }
  
  # Return the coordinates for inferior_sacrum
  return(inferior_sacrum)
  
  
}

target_l1pa_function <- function(pelvic_incidence = 51.813768) {-19 + 0.5*pelvic_incidence}


# Function to create vertebra as an sf polygon
jh_create_vert_coord_list_slope_0_from_centroid_function <- function(centroid_x = 0, centroid_y = 0, width = 5, height = 4, spine_orientation = "right") {
  half_height <- height / 2
  half_width <- width / 2
  
  if(spine_orientation == "right"){
    # Define the four corners of the vertebra before rotation
    sp <- c(centroid_x - half_width, centroid_y + half_height)
    sa <- c(centroid_x + half_width, centroid_y + half_height)
    ia <- c(centroid_x + half_width, centroid_y - half_height)
    ip <- c(centroid_x - half_width, centroid_y - half_height)  
  }else{
    # Define the four corners of the vertebra before rotation
    sa <- c(centroid_x - half_width, centroid_y + half_height)
    sp <- c(centroid_x + half_width, centroid_y + half_height)
    ip <- c(centroid_x + half_width, centroid_y - half_height)
    ia <- c(centroid_x - half_width, centroid_y - half_height)
  }
  
  # coord_list <- list()
  # coord_list$sp <- sp
  # coord_list$sa <- sa
  # coord_list$ia <- ia
  # coord_list$ip <- ip
  # return(coord_list)
  
  coord_matrix <- rbind(sa, sp, ip, ia) 
  
  return(coord_matrix)
}


# Function to create vertebra as an sf polygon
jh_create_vertebra_from_coordinates_function <- function(centroid_x = 0, centroid_y = 0, width = 5, height = 4, spine_orientation = "right") {
  half_height <- height / 2
  half_width <- width / 2
  
  if(spine_orientation == "right"){
    # Define the four corners of the vertebra before rotation
    sp <- c(centroid_x - half_width, centroid_y + half_height)
    sa <- c(centroid_x + half_width, centroid_y + half_height)
    ia <- c(centroid_x + half_width, centroid_y - half_height)
    ip <- c(centroid_x - half_width, centroid_y - half_height)  
  }else{
    # Define the four corners of the vertebra before rotation
    sa <- c(centroid_x - half_width, centroid_y + half_height)
    sp <- c(centroid_x + half_width, centroid_y + half_height)
    ip <- c(centroid_x + half_width, centroid_y - half_height)
    ia <- c(centroid_x - half_width, centroid_y - half_height)
  }
  
  
  vert_body <- rbind(sp, sa, ia, ip, sp) ## binds the corners to make a square
  
  return(st_polygon(list(vert_body)))
}



# jh_rotate_polygon_around_centroid <- function(polygon, angle_degrees) {
#   # Step 1: Get the centroid of the polygon
#   centroid <- st_centroid(polygon)
#   centroid_coords <- st_coordinates(centroid)[1, 1:2]  # Ensure this is a 2D vector (x, y)
#   
#   # Step 2: Translate the polygon to have the centroid at the origin
#   coords <- st_coordinates(polygon)[, 1:2]  # Extract the polygon's coordinates
#   translated_coords <- sweep(coords, 2, centroid_coords)  # Subtract centroid from coordinates
#   
#   # Step 3: Create a rotation matrix
#   angle_radians <- angle_degrees * pi / 180  # Convert angle to radians
#   rotation_matrix <- matrix(c(cos(angle_radians), -sin(angle_radians),
#                               sin(angle_radians),  cos(angle_radians)),
#                             ncol = 2, byrow = TRUE)
#   
#   # Step 4: Apply the rotation matrix to the translated coordinates
#   rotated_coords <- translated_coords %*% rotation_matrix
#   
#   # Step 5: Translate the polygon back to its original position
#   final_coords <- sweep(rotated_coords, 2, centroid_coords, "+")
#   
#   # Step 6: Rebuild the rotated polygon
#   rotated_polygon <- st_polygon(list(final_coords))
#   
#   return(st_sfc(rotated_polygon, crs = st_crs(polygon)))
# }



jh_rotate_polygon_around_centroid <- function(polygon, angle_degrees) {
  # Step 1: Get the centroid of the polygon
  centroid <- st_centroid(polygon)
  centroid_coords <- st_coordinates(centroid)[1, 1:2]  # Ensure this is a 2D vector (x, y)
  
  # Step 2: Translate the polygon to have the centroid at the origin
  coords <- st_coordinates(polygon)[, 1:2]  # Extract the polygon's coordinates
  translated_coords <- sweep(coords, 2, centroid_coords)  # Subtract centroid from coordinates
  
  # Step 3: Create a rotation matrix
  angle_radians <- angle_degrees * pi / 180  # Convert angle to radians
  rotation_matrix <- matrix(c(cos(angle_radians), -sin(angle_radians),
                              sin(angle_radians),  cos(angle_radians)),
                            ncol = 2, byrow = TRUE)
  
  # Step 4: Apply the rotation matrix to the translated coordinates
  rotated_coords <- translated_coords %*% rotation_matrix
  
  # Step 5: Translate the polygon back to its original position
  final_coords <- sweep(rotated_coords, 2, centroid_coords, "+")
  
  # Step 6: Rebuild the rotated polygon
  rotated_polygon <- st_polygon(list(final_coords))
  
  return(st_sfc(rotated_polygon, crs = st_crs(polygon)))
}


# rotate_spine_function <- function(spine_df, angle_degrees, point_of_rotation = c(0, 0)) {
#   # Convert angle to radians
#   angle_rad <- angle_degrees * pi / 180
#   
#   # Extract rotation center
#   x_center <- point_of_rotation[1]
#   y_center <- point_of_rotation[2]
#   
#   # Rotation matrix components
#   cos_theta <- cos(angle_rad)
#   sin_theta <- sin(angle_rad)
#   
#   # Apply rotation to x, y coordinates relative to the rotation point
#   spine_df <- spine_df %>%
#     mutate(
#       x_shifted = x - x_center,
#       y_shifted = y - y_center,
#       x_rot = x_shifted * cos_theta - y_shifted * sin_theta + x_center,
#       y_rot = x_shifted * sin_theta + y_shifted * cos_theta + y_center
#     ) %>%
#     select(spine_level, vert_point, x_rot, y_rot) %>%
#     rename(x = x_rot, y = y_rot)
#   
#   return(spine_df)
# }

jh_rotate_spine_for_pt_adjustment_function_function <- function(spine_df, angle_degrees, point_of_rotation = c(0, 0)) {
  # Convert angle to radians
  angle_rad <- angle_degrees * pi / 180
  
  # Extract rotation center
  x_center <- point_of_rotation[1]
  y_center <- point_of_rotation[2]
  
  # Rotation matrix components
  cos_theta <- cos(angle_rad)
  sin_theta <- sin(angle_rad)
  
  # Apply rotation to x, y coordinates relative to the rotation point
  spine_df <- spine_df %>%
    mutate(
      x_shifted = x - x_center,
      y_shifted = y - y_center,
      x_rot = x_shifted * cos_theta - y_shifted * sin_theta + x_center,
      y_rot = x_shifted * sin_theta + y_shifted * cos_theta + y_center
    ) %>%
    select(spine_level, vert_point, x_rot, y_rot) %>%
    rename(x = x_rot, y = y_rot)
  
  return(spine_df)
}

jh_rotate_proximal_spine_at_segment_function <- function(spine_df, 
                                                         interspace, 
                                                         angle_degrees, 
                                                         point_of_rotation = c(0, 0),
                                                         spine_orientation = "left"){
  # Convert angle to radians
  orientation_modifier <- if_else(spine_orientation == "left", -1, 1)
  angle_rad <- (angle_degrees * pi / 180)*orientation_modifier
  
  
  
  # Extract rotation center
  x_center <- point_of_rotation[1]
  y_center <- point_of_rotation[2]
  
  # Rotation matrix components
  cos_theta <- cos(angle_rad)
  sin_theta <- sin(angle_rad)
  
  # Apply rotation to x, y coordinates relative to the rotation point
  spine_df <- spine_df %>%
    mutate(
      x_shifted = x - x_center,
      y_shifted = y - y_center,
      x_rot = x_shifted * cos_theta - y_shifted * sin_theta + x_center,
      y_rot = x_shifted * sin_theta + y_shifted * cos_theta + y_center
    ) %>%
    select(spine_level, vert_point, x_rot, y_rot) %>%
    rename(x = x_rot, y = y_rot)
  
  return(spine_df)
}

# jh_construct_geoms_after_planning_function <- function(vertebral_level_tibble, buffer_amount = 0){
#   coord_list_df <- vertebral_level_tibble %>%
#     mutate(vert_point_coord = map2(.x = x, .y = y, .f = ~ c(.x, .y))) %>%
#     select(vert_point, vert_point_coord)
# 
#   vert_coord_point_list <- coord_list_df$vert_point_coord
# 
#   names(vert_coord_point_list) <- coord_list_df$vert_point
# 
#   if(unique(vertebral_level_tibble$spine_level) == "sacrum"){
#     vertebral_geom <- st_polygon(list(rbind(vert_coord_point_list$s1_anterior_superior,
#                                             vert_coord_point_list$sac_inf_1,
#                                             vert_coord_point_list$s1_posterior_superior,
#                                             vert_coord_point_list$s1_anterior_superior)))
#   }else{
#     vertebral_geom <- st_polygon(list(rbind(vert_coord_point_list$sp,
#                                             vert_coord_point_list$sa,
#                                             vert_coord_point_list$ia,
#                                             vert_coord_point_list$ip,
#                                             vert_coord_point_list$sp)))
#   }
# 
#   if(buffer_amount > 0){
#     geom <- jh_safely_buffer_vert_function(vert_geom = vertebral_geom,
#                                            buffer_amount = buffer_amount)
#     return(geom)
#   }else{
#     return(vertebral_geom)
#   }
# 
# }

jh_construct_geoms_after_planning_function <- function(vertebral_level_tibble, buffer_amount = 0){
  coord_list_df <- vertebral_level_tibble %>%
    mutate(vert_point_coord = map2(.x = x, .y = y, .f = ~ c(.x, .y))) %>%
    select(vert_point, vert_point_coord)
  
  vert_coord_point_list <- coord_list_df$vert_point_coord
  
  names(vert_coord_point_list) <- coord_list_df$vert_point
  
  if(unique(vertebral_level_tibble$spine_level) == "sacrum"){
    vertebral_geom <- st_polygon(list(rbind(vert_coord_point_list$s1_anterior_superior,
                                            vert_coord_point_list$sac_inf_1,
                                            vert_coord_point_list$s1_posterior_superior,
                                            vert_coord_point_list$s1_anterior_superior)))
  }else{
    vert_coord_point_list <- jh_calculate_points_for_square_vertebra_function(vert_coord_list = vert_coord_point_list)
    
    vertebral_geom <- st_polygon(list(rbind(vert_coord_point_list$sp, 
                                            vert_coord_point_list$sa,
                                            vert_coord_point_list$ia, 
                                            vert_coord_point_list$ip, 
                                            vert_coord_point_list$sp)))
  }
  
  if(buffer_amount > 0){
    geom <- jh_safely_buffer_vert_function(vert_geom = vertebral_geom,
                                           buffer_amount = buffer_amount)
    return(geom)
  }else{
    return(vertebral_geom)
  }
  
}


jh_calculate_coord_at_intersection_function <- function(line_1_point_a, line_1_point_b, line_2_point_a, line_2_point_b) {
  x1 <- line_1_point_a[[1]];      y1 <- line_1_point_a[[2]]
  x2 <- line_1_point_b[[1]];  y2 <- line_1_point_b[[2]]
  x3 <- line_2_point_a[[1]];  y3 <- line_2_point_a[[2]]
  x4 <- line_2_point_b[[1]];  y4 <- line_2_point_b[[2]]
  
  dx1 <- x2 - x1
  dy1 <- y2 - y1
  dx2 <- x4 - x3
  dy2 <- y4 - y3
  
  denom <- dx1 * dy2 - dy1 * dx2
  if (abs(denom) < .Machine$double.eps) return(NA)
  
  t <- ((x3 - x1) * dy2 - (y3 - y1) * dx2) / denom
  c(x1 + t * dx1, y1 + t * dy1)
}



jh_calculate_points_for_square_vertebra_function <- function(vert_coord_list) {
  
  coord_list_return <- list()
  
  posterior_body_height <- jh_calculate_distance_between_2_points_function(point_1 = vert_coord_list$sp, point_2 = vert_coord_list$ip)
  
  anterior_body_height <- jh_calculate_distance_between_2_points_function(point_1 = vert_coord_list$sa, point_2 = vert_coord_list$ia)
  
  
  if(posterior_body_height > anterior_body_height){
    coord_list_return$sp <- vert_coord_list$sp
    coord_list_return$sa <- vert_coord_list$sa
    coord_list_return$ia <- vert_coord_list$ia
    coord_list_return$ip <- vert_coord_list$ip
    
    return(coord_list_return)
  }else{
    sp <- as.list(vert_coord_list$sp)
    ip <- as.list(vert_coord_list$ip)
    
    base_length <- posterior_body_height*2
    
    names(sp) <- c("x", "y")
    names(ip) <- c("x", "y")
    dx <- ip$x - sp$x
    dy <- ip$y - sp$y
    
    # Compute unit perpendicular vector (rotated 90 degrees counterclockwise)
    perp_x <- -dy
    perp_y <- dx
    
    # Normalize the perpendicular vector to unit length
    norm_factor <- sqrt(perp_x^2 + perp_y^2)
    perp_x <- perp_x / norm_factor
    perp_y <- perp_y / norm_factor
    
    # Scale to base_length
    perp_x <- perp_x * base_length
    perp_y <- perp_y * base_length
    
    if(vert_coord_list$sa[[1]] < vert_coord_list$sp[[1]]){
      sup_point <- list(x = sp$x - perp_x, y = sp$y - perp_y)
      inf_point <- list(x = ip$x - perp_x, y = ip$y - perp_y)
    }else{
      sup_point <- list(x = sp$x + perp_x, y = sp$y + perp_y)
      inf_point <- list(x = ip$x + perp_x, y = ip$y + perp_y)
    }
    
    
    
    coord_list_return$sp <- vert_coord_list$sp
    coord_list_return$sa <- jh_calculate_coord_at_intersection_function(line_1_point_a = vert_coord_list$sp,
                                                                        line_1_point_b = as.vector(sup_point),
                                                                        line_2_point_a = vert_coord_list$sa, 
                                                                        line_2_point_b = vert_coord_list$ia)
    
    coord_list_return$ia <- jh_calculate_coord_at_intersection_function(line_1_point_a = vert_coord_list$ip,
                                                                        line_1_point_b = as.vector(inf_point),
                                                                        line_2_point_a = vert_coord_list$sa, 
                                                                        line_2_point_b = vert_coord_list$ia)
    coord_list_return$ip <- vert_coord_list$ip
    
    
    return(coord_list_return)
    
  }
}



compute_l5_centroid_x_y_vector_function <- function(s1_centroid_x = NA,
                                                    s1_centroid_y = NA,
                                                    l4_centroid_x = NA,
                                                    l4_centroid_y = NA,
                                                    l1_centroid_x = NA,
                                                    l1_centroid_y = NA, 
                                                    fem_head_center_x = 0, 
                                                    fem_head_center_y = 0){
  
  s1_centroid_x <- s1_centroid_x - fem_head_center_x
  l4_centroid_x <- l4_centroid_x - fem_head_center_x
  l1_centroid_x <- l1_centroid_x - fem_head_center_x
  
  s1_centroid_y <- s1_centroid_y - fem_head_center_y
  l4_centroid_y <- l4_centroid_y - fem_head_center_y
  l1_centroid_y <- l1_centroid_y - fem_head_center_y
  
  x <-  -2.3065713+0.56815108*s1_centroid_x-0.052470422*s1_centroid_y+0.4914063*l4_centroid_x+0.057601093*l4_centroid_y-0.048191777*l1_centroid_x+0.0027333273*l1_centroid_y 
  
  y <- 2.601413+0.10427372*s1_centroid_x+0.52610784*s1_centroid_y-0.12734358*l4_centroid_x+0.52839458*l4_centroid_y+0.039031162*l1_centroid_x-0.056739585*l1_centroid_y
  
  return(c(x+fem_head_center_x, y+fem_head_center_y))
}
