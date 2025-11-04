################################ V2 ############################################


library(shiny)
library(sf)
library(tidyverse)
library(ggplot2)
library(shinyWidgets)
library(shinyBS)
library(svglite)
library(glue)
library(cowplot)
library(janitor)
# library(rms)
# library(Hmisc)
library(cowplot)
# library(assertr)
library(lubridate)
library(shinydashboard)
library(magick)
# library(ggforce)
# library(plotly)
library(redcapAPI)
library(gt)
library(blastula)
library(officer)
library(rvg)

library(httr)
library(jsonlite)

library(oro.dicom)

# install.packages("oro.dicom")

library(base64enc)
library(png)

options(shiny.maxRequestSize = 35*1024^2)

# --- app startup (runs once) ---
BASE_DATA_DIR <- Sys.getenv("APP_DATA_DIR", "/var/shiny-data")
DICOM_DIR     <- file.path(BASE_DATA_DIR, "dicoms")
dir.create(DICOM_DIR, showWarnings = FALSE, recursive = TRUE)

message(sprintf("DICOM_DIR resolved to: %s", DICOM_DIR))
stopifnot(file.access(DICOM_DIR, 2) == 0)  # ensure writable

source("code/shiny_functions.R", local = TRUE)
source("code/jh_calculation_functions.R", local = TRUE)
source("code/jh_spine_coordinate_geometry_functions.R", local = TRUE)

# source("code/jh_build_spine_from_coordinates_functions.R", local = TRUE)

# source("code/jh_constructing_spine_from_coordinates_new.R", local = TRUE)

source("code/loading_dicoms.R", local = TRUE)


im <- magick::image_read(path = "images/pelvis.png")

# Read original, precompute rasters once
im_left  <- im                                      # your already-read magick image
im_right <- magick::image_flop(im_left)             # horizontally mirrored

info <- magick::image_info(im_left)
w <- info$width; h <- info$height

im_raster_left  <- as.raster(im_left)
im_raster_right <- as.raster(im_right)

##vertebra
im_v_left <- magick::image_read(path = "images/vertebra.png")
# Read original, precompute rasters once
# im_left  <- im_v_left                                      # your already-read magick image
im_v_right <- magick::image_flop(im_v_left)             # horizontally mirrored

info_v <- magick::image_info(im_v_left)
w_v <- info_v$width; h_v <- info_v$height

im_v_raster_left  <- as.raster(im_v_left)
im_v_raster_right <- as.raster(im_v_right)



spine_instruction_points_norm <- tribble(
  ~id,                       ~x,    ~y,
  "fem_head_center",         190,  230,
  "asis_1",                  45, 410,
  "asis_2",                  105, 415,
  "pubic_symphysis",         60,  175,
  "anterior_acetabulum",     150,  280,
  "posterior_acetabulum",    265, 215,
  "sacrum_inferior_anterior",  410,  345,
  "sacrum_inferior_posterior", 440,  390
  # ... add the rest of the ids you show in the guide
)

# 4) Plot: image as background + points on top


# all_possible_lumbar_segments_angles_with_lpa_df <- read_csv("all_possible_lumbar_segment_angles_for_lpa.csv")

# reactlog_enable()


ui <- dashboardPage(
  dashboardHeader(title = "SolaSpine"
  ),
  dashboardSidebar(
    tags$style(HTML("
  .segment-input {
    display: flex;
    align-items: center; /* Align items to the center vertically */
    justify-content: end; /* Ensure space between label and input */
    margin-bottom: 0px; /* Adjusts the spacing between the inputs */
    font-size: 14px;
    color: black;
  }
  .segment-label {
    margin-right: 5px; /* Slightly increase space for the label */
    white-space: nowrap; /* Prevent labels from wrapping */
    font-size: 12px;
    color: black;
  }
  .segment-input .form-group {
    margin-bottom: 1px; /* Reduce default margin-bottom of form-group */
    color: black;
  }
  .custom-numeric-input {
    padding: 0; /* Remove padding from the numeric input container */
    margin: 0; /* Remove margin from the numeric input container */
    text-align: -webkit-left;
  }
  .custom-numeric-input .form-control {
    padding: 2px 5px; /* Adjust padding inside the numeric input */
    margin-bottom: 0px; /* Ensure no extra margin below the input */
    text-align: -webkit-left;
    width: 50px;
  }
")),
    br(), 
    # conditionalPanel(
    #   condition = "input.xray_file_uploaded == true",
    textOutput(outputId = "user_last_name"),
    br(),
    textOutput(outputId = "patient_id"),
    br(),
    textOutput(outputId = "number_of_records_remaining"),
    # ),
    conditionalPanel(
      condition = "input.xray_file_uploaded == true",
      h4("Xray Orientation:"),
      actionBttn(
        inputId = "spine_orientation_button",
        label = "Facing LEFT",
        style = "material-flat",
        color = "primary",
        icon = icon("arrow-left")
      )
    ),
    br(),
    plotOutput(outputId = "xray_instruction_plot"),
    br(),
    tableOutput(outputId = "redcap_means_measures"),
    br(),
    textOutput("calibration_status_text"),
    div(
      style = "display: none;",  # Hide the entire div, including the switch
      switchInput(
        inputId = "xray_file_uploaded",
        size = "mini", label = NULL,
        value = FALSE, 
        onLabel = "Y", 
        offLabel = "N",
      ),
      switchInput(
        inputId = "all_points_recorded",
        size = "mini", label = NULL,
        value = FALSE, 
        onLabel = "Y", 
        offLabel = "N",
      )
    )
  ),
  dashboardBody(
    tags$div(
      id = "loading-overlay",
      style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; 
           background-color: #fff; z-index: 9999; display: flex; 
           justify-content: center; align-items: center;",
      tags$div(
        tags$p("Loading...")
      )
    ),
    tags$script("
  $(document).on('shiny:idle', function() {
    $('#loading-overlay').fadeOut(500);
  });
"),
    tags$head(tags$style(HTML("
    .file-upload-container {
      border: 4px dashed #c5c5c5;
      border-radius: 10px;
      width: 80%;
      margin: 50px auto;
      padding: 50px;
      text-align: center;
      background-color: #f7f9fb;
      cursor: pointer;
      font-size: 18px;
      color: #333;
      transition: background-color 0.3s ease;
      position: relative;
    }
    .file-upload-container.dragover {
      background-color: #edf2f7;
    }
    .browse-btn {
      color: #007bff;
      font-weight: bold;
      cursor: pointer;
      display: inline-block;
      font-size: 20px;
      margin-top: 10px;
    }
    .upload-icon {
      font-size: 50px;
      color: #007bff;
      margin-bottom: 10px;
    }
    #image {
      position: absolute;
      width: 1px;
      height: 1px;
      padding: 0;
      margin: -1px;
      overflow: hidden;
      clip: rect(0, 0, 0, 0);
      border: 0;
      opacity: 0; /* Make it transparent */
    }
  "))),
    tags$style(HTML("
  #alignment_adjustment_box {
    padding: 0px !important;
  }
    /* Upload status bar */
    #upload-status { 
    display: none; 
    margin-top: 16px; 
    font-size: 14px; 
    color: #333; 
  }
  .upload-progress {
    position: relative;
    height: 12px;
    background: #e9ecef;
    border-radius: 6px;
    overflow: hidden;
    margin-top: 6px;
  }
  .upload-progress .bar {
    position: absolute;
    left: 0; top: 0; bottom: 0;
    width: 0%;
    background: #4f8fe9;
    transition: width 0.2s ease;
    border-radius: 6px;
  }

  @keyframes indeterminate {
    0%   { transform: translateX(-100%); }
    50%  { transform: translateX(25%); }
    100% { transform: translateX(200%); }
  }
/* container is a vertical stack; gap controls row spacing */
    .inline-sliders {
      display: flex;
      flex-direction: column;
      align-items: flex-start;      /* was 'left' */
      gap: 6px;                     /* tighten vertical spacing */
    }
    /* each slider row: label + slider inline */
    .inline-slider {
      display: flex;
      align-items: center;
      gap: 8px;
    }
    .inline-label {
      min-width: 78px;
      font-size: 12px;
      color: #444;
      white-space: nowrap;
    }
    /* kill extra Shiny spacing on all controls inside */
    .inline-sliders .form-group { margin: 0; }
    .inline-sliders .checkbox     { margin: 0; }
    .inline-sliders .btn { padding: 2px 8px; }  /* compact reset button */

    /* Ion.RangeSlider tweaks (Shiny's sliderInput) */
    .inline-slider .irs { width: 200px; }       /* slider width per row */
    .irs--shiny .irs-grid { display: none; }    /* no ticks/grid */
    .irs--shiny .irs-line,
    .irs--shiny .irs-bar  { height: 4px; }      /* thin track */
    .irs--shiny .irs-handle {
      top: 16px; width: 12px; height: 12px; border-width: 1px;
    }
    .irs--shiny .irs-min, .irs--shiny .irs-max, .irs--shiny .irs-single {
      font-size: 10px;                          /* small min/max/value bubble */
    }

    /* row for invert + reset on its own line */
    .inline-row {
      display: flex;
      align-items: center;
      justify-content: space-evenly;
      gap: 12px;
    }
")),
    tags$script(HTML("
  $(document).on('shiny:connected', function() {
    var container     = $('.file-upload-container');
    var fileInput     = $('#image');
    var uploadStatus  = $('#upload-status');
    var uploadPercent = $('#upload-percent');
    var uploadBar     = $('#upload-status .bar');

    // ---- drag & drop (unchanged) ----
    container.on('dragover', function(e){ e.preventDefault(); e.stopPropagation(); container.addClass('dragover'); });
    container.on('dragleave', function(e){ e.preventDefault(); e.stopPropagation(); container.removeClass('dragover'); });
    container.on('drop', function(e){
      e.preventDefault(); e.stopPropagation(); container.removeClass('dragover');
      var files = e.originalEvent.dataTransfer.files;
      fileInput[0].files = files;
      fileInput.trigger('change');
    });

    // hidden input click-through (unchanged)
    fileInput.css({ position:'absolute', top:'-9999px', opacity:0, visibility:'hidden', 'pointer-events':'none' });
    container.on('click', function(){ fileInput.off('click'); setTimeout(function(){ fileInput.trigger('click'); }, 50); });

    // show 0% immediately when a file is chosen
    fileInput.on('change', function(){
      if (this.files && this.files.length > 0) {
        uploadStatus.show();
        uploadBar.css('width','0%');
        uploadPercent.text('0%');
      }
    });

    // ---- robust XHR progress hook (no Blueimp dependency) ----
    (function(){
      var origSend = XMLHttpRequest.prototype.send;

      function bodyHasImageField(formData){
        try {
          if (!(formData instanceof FormData)) return false;
          for (var pair of formData.entries()) {
            var key = pair[0] || '';
            // match 'image', 'image[]', 'image[0]' etc.
            if (key === 'image' || key.indexOf('image[') === 0) return true;
          }
        } catch(e) {}
        return false;
      }

      XMLHttpRequest.prototype.send = function(body){
        try {
          if (bodyHasImageField(body)) {
            var xhr = this;

            // ensure visible at start
            uploadStatus.show();
            uploadBar.css('width','0%');
            uploadPercent.text('0%');

            if (xhr.upload) {
              xhr.upload.addEventListener('progress', function(e){
                if (e.lengthComputable) {
                  var pct = Math.round((e.loaded / e.total) * 100);
                  uploadBar.css('width', pct + '%');
                  uploadPercent.text(pct + '%');
                }
              });
            }

            var finalize = function(){
              uploadBar.css('width','100%');
              uploadPercent.text('100%');
              setTimeout(function(){
                uploadStatus.fadeOut();
                uploadBar.css('width','0%');
                uploadPercent.text('0%');
              }, 800);
            };
            xhr.addEventListener('loadend', finalize);
            xhr.addEventListener('error', finalize);
            xhr.addEventListener('abort', finalize);
          }
        } catch(e) {
          // swallow errors so uploads aren't blocked
          console && console.warn && console.warn('Upload progress hook error:', e);
        }
        return origSend.apply(this, arguments);
      };
    })();
  });
")),
    conditionalPanel(
      condition = "input.xray_file_uploaded == false",
      fluidRow(
        column(width = 12, align = "center",
               div(class = "file-upload-container",
                   fluidRow(align = "center",
                            tags$span(icon("upload", class = "upload-icon", 
                                           style = "font-size: 60px; color: #007bff; cursor: pointer;")),
                            br(),
                            span("Drag & Drop or ", span("Choose an X-ray", class = "browse-btn")),
                   )
               ),
               div(
                 style = "display: none;",  # Hide the entire div, including the switch
                 # fileInput("image", label = NULL, accept = 'image/', width = "100%")
                 fileInput(
                   "image",
                   label = NULL,
                   multiple = TRUE,
                   accept = c("image/*", ".dcm", ".dicom", "application/dicom", "application/dicom+json", "application/octet-stream"),
                   width = "100%"
                 )
               ),
               div(
                 id = "upload-status",
                 span("Uploading... "),
                 span(id = "upload-percent", "0%"),
                 div(class = "upload-progress",
                     div(class = "bar", style = "width: 0%;"))
               ),
        )
      )
    ),
    conditionalPanel(
      condition = "input.xray_file_uploaded == false",
      fluidRow(
        column(
          width = 12,
          textInput(inputId = "next_record_id_input", label = "Enter Record to Import:", placeholder = "or press button below to pull next record"),
          div(class = "center-cta",
              actionButton("pull_next_record_button", "Pull Next Record",
                           class = "btn btn-primary btn-lg btn-cta")
          )
        )
      )
    ),
    tags$head(
      tags$style(HTML("
    .nav-tabs-custom > .nav-tabs {
      background-color: #0073e6; /* Set your preferred color */
    }
    .nav-tabs-custom > .nav-tabs > li.active > a, 
    .nav-tabs-custom > .nav-tabs > li.active > a:hover {
      background-color: #005bb5; /* Set a darker color for active tab */
      color: white;
      font-size: 18px; /* Make tab titles larger */
      font-weight: bold; /* Make tab titles bold */
    }
    .nav-tabs-custom > .nav-tabs > li > a {
      color: white;
    }
    .center-cta {
    display:flex; justify-content:center; align-items:flext-start;
    min-height: 80px;              /* gives the row some breathing room */
  }
  .btn-cta {
    font-size: xx-large; padding: 16px 16px; border-radius: 12px;
  }
  "))
    ),
    conditionalPanel(
      condition = "input.xray_file_uploaded == true", 
      # Boxes need to be put in a row (or column)
      fluidRow(
        ########## MAIN PAGE COLUMN 1 STARTS HERE: ##############
        ########## MAIN PAGE COLUMN 1 STARTS HERE: ##############
        ########## MAIN PAGE COLUMN 1 STARTS HERE: ##############
        
        column(width = 6, 
               box(width = 12,
                   fluidRow(
                     class = "d-flex justify-content-center",  # Center content horizontally
                     tags$div(
                       style = "font-size: 20px; 
                 font-weight: bold; 
                 color: yellow; 
                 font-family: arial; 
                 font-style: italic; 
                 text-align: center; 
                 background-color: black; 
                 padding: 3px; 
                 border-radius: 12px;  /* Rounded corners */
                 display: block;
                 margin-left: 10px;
                 margin-right: 10px;
                     box-sizing: border-box;  /* Include padding and border in the element's width */",
                       htmlOutput(outputId = "xray_click_instructions")
                     ),
                     conditionalPanel(
                       condition = "input.xray_file_uploaded == true",
                       fluidRow(
                         tags$div(
                           "Zoom with scroll wheel; Pan with right click",
                           style = "font-size: 10pt; font-style: italic; color: #555; text-align: center;"
                         )
                       )
                     ),
                     # br()
                   ),
                   conditionalPanel(
                     # condition = "input.xray_file_uploaded == true & input.all_points_recorded == false",
                     condition = "input.xray_file_uploaded == true",
                     fluidRow(
                       column(width = 12, 
                              tags$div(
                                id = "image-container",
                                style = "position: relative; width: auto; height: 700px; overflow: hidden; border: 0px solid #ccc;",
                                tags$img(
                                  id = "uploadedImage",
                                  src = "",
                                  style = "position: absolute; top: 0; left: 0; cursor: crosshair"
                                )
                              ),
                              # tags$script(src = "https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js"),
                              tags$script(HTML("
       $(document).ready(function() {
  let scale = 1;
  let panX = 0, panY = 0;
  let isPanning = false;
  let startX, startY;

  function updateImageTransform() {
    $('#uploadedImage').css({
      'transform-origin': 'top left',
      'transform': `translate(${panX}px, ${panY}px) scale(${scale})`
    });

    // Update positions of all dots to match the transformation
    $('.dot').each(function() {
      const originalX = $(this).data('orig-x');
      const originalY = $(this).data('orig-y');
      const adjustedX = (originalX * scale) + panX;
      const adjustedY = ((imageHeight - originalY) * scale) + panY;

      $(this).css({
        left: adjustedX + 'px',
        top: adjustedY + 'px'
      });
    });
  }

  let imageHeight = null; // We'll determine the height once the image is loaded

  Shiny.addCustomMessageHandler('load-image', function(data) {
    var img = document.getElementById('uploadedImage');
    img.src = data.src;

    // Once the image loads, set the natural height
    img.onload = function() {
      imageHeight = img.naturalHeight;
      // Reset scaling and position when new image is loaded
      scale = 1;
      panX = 0;
      panY = 0;
      updateImageTransform();

      // Remove existing dots when a new image is loaded
      $('.dot').remove();
    };
  });

  Shiny.addCustomMessageHandler('plot-coordinates', function(data) {
    // Remove existing dots
    $('.dot').remove();

    if (!imageHeight) {
      console.error('Image height not set yet.');
      return;
    }

    // Plot all coordinates
    data.coords.forEach(function(coord, index) {
      console.log(`Plotting point ${index + 1}:`, coord);  // Debugging log for each coordinate

      // Create a new dot element
      const dot = $('<div class=\"dot\"></div>');

      // Adjust Y-coordinate for the Cartesian system
      const adjustedX = (coord.x * scale) + panX;
      const adjustedY = ((imageHeight - coord.y) * scale) + panY;  // Adjusting Y-coordinate

      // Debugging log for adjusted positions
      console.log('Adjusted position for dot:', { adjustedX, adjustedY });
      
      // Adjust the dot's CSS to center it on the point clicked
    const dotSize = 10; // This is the width and height of the dot (in pixels)
    const correctionOffset = 0.5; // Small adjustment if there’s still an offset issue

    dot.css({
      position: 'absolute',
      top: (adjustedY - (dotSize / 2)) + correctionOffset + 'px',
      left: (adjustedX - (dotSize / 2)) + correctionOffset + 'px',
      width: dotSize + 'px',
      height: dotSize + 'px',
      'background-color': 'red',
      'border-radius': '50%',
      'pointer-events': 'none', // Ensures dots don't interfere with panning/zooming
      'z-index': 10 // Ensures the dots are layered above the image
    });

      // Store original coordinates for reference during zoom and pan
      dot.data('orig-x', coord.x);
      dot.data('orig-y', coord.y);

      // Append dot to the image container
      $('#image-container').append(dot);
    });

    // Immediately update dot positions to reflect the current zoom and pan state
    updateImageTransform();
  });

  // Handle zoom with the mouse wheel
  $('#image-container').on('wheel', function(e) {
    e.preventDefault();
    const zoomIntensity = 0.1;
    const delta = e.originalEvent.deltaY > 0 ? -1 : 1;
    const previousScale = scale;

    // Update scale
    scale *= (1 + delta * zoomIntensity);
    scale = Math.min(Math.max(0.5, scale), 5);

    // Calculate new pan to keep the zoom centered at mouse position
    const mouseX = e.pageX - $(this).offset().left;
    const mouseY = e.pageY - $(this).offset().top;

    panX = mouseX - (mouseX - panX) * (scale / previousScale);
    panY = mouseY - (mouseY - panY) * (scale / previousScale);

    updateImageTransform();
  });

  // Handle panning with right-click only
  $('#image-container').on('mousedown', function(e) {
    if (e.which === 3) { // Right-click
      isPanning = true;
      startX = e.pageX - panX;
      startY = e.pageY - panY;
      $(this).css('cursor', 'grabbing');
      return false; // Prevent context menu
    }
  });

  $(document).on('mouseup', function() {
    isPanning = false;
    $('#image-container').css('cursor', 'crosshair');
  });

  $(document).on('mousemove', function(e) {
    if (!isPanning) return;
    panX = e.pageX - startX;
    panY = e.pageY - startY;

    updateImageTransform();
  });

  // Prevent the default context menu from appearing on right-click
  $('#image-container').on('contextmenu', function(e) {
    return false;
  });

  // Record click coordinates on left-click
  $('#image-container').on('click', function(e) {
    if (e.which === 1) { // Left-click
      var img = document.getElementById('uploadedImage');
      const rect = img.getBoundingClientRect(); // Get the image's bounding box relative to the viewport

      // Get the click coordinates relative to the image
      const clickX = e.clientX - rect.left;
      const clickY = e.clientY - rect.top;

      // Adjust the coordinates for the current pan and zoom level to get the original image reference frame
      const adjustedX = clickX / scale;
      const adjustedY = clickY / scale;

      // Correcting Y-coordinate (flipping the y-axis based on the height of the image)
      const correctedY = imageHeight - adjustedY;

      // Debugging log for adjusted click positions
      console.log('Adjusted click position:', { adjustedX, correctedY });

      // Send the corrected click coordinates to the Shiny server
      Shiny.setInputValue('xray_click', {x: adjustedX - 1, y: correctedY + 1}, {priority: 'event'});
    }
  });
});

Shiny.addCustomMessageHandler('apply-filters', function(cfg){
  // cfg = {brightness: <num>, contrast: <num>, gamma: <num>, invert: <bool>}
  const img = document.getElementById('uploadedImage');
  if(!img) return;

  // Normalize percent sliders to CSS-friendly multipliers
  const b = (cfg.brightness || 100) / 100.0;
  const c = (cfg.contrast   || 100) / 100.0;
  // Approximate gamma by adjusting brightness around mid-gray.
  // For gamma >1 (slider >100), increase contrast toward midtones.
  // This is not true WL/WW, but feels similar for quick viewing.
  const g = (cfg.gamma || 100) / 100.0;

  // Build CSS filter string
  // Order matters: contrast then brightness then invert works well visually.
  // We'll blend a subtle extra contrast based on gamma around midtones.
  const gammaBoost = Math.max(0.5, Math.min(2.0, g));
  const extraContrast = 0.5 * (gammaBoost - 1) + 1; // 1 at 100%, up to ~1.5 at 200%

  const invert = cfg.invert ? ' invert(1)' : '';

  img.style.filter =
    `contrast(${(c * extraContrast)}) brightness(${b})` + invert;

  // Keep the red dots crisp (ensure they are NOT affected by filters)
  // Our dots are separate absolutely-positioned divs, so no change is needed.
});

      "))
                       )
                     )
                   ),
                   ############################## COMPLETED COORDINATE COLLECTION ################################
                   
                   br(),
                   fluidRow(
                     div(class = "inline-row",
                         switchInput(
                           inputId = "img_invert",
                           label = tagList(icon("circle-half-stroke"), "Invert"),
                           value = FALSE,
                           size = 'small',
                           labelWidth = "80px"
                         ),
                         actionButton("img_reset_filters", "Reset", class = "btn btn-default btn-sm")
                     ),
                     # column(
                     #   width = 12,
                     box(title = "Adjust Image", collapsible = TRUE, collapsed = TRUE, width = 12,
                         div(class = "inline-sliders",
                             div(class = "inline-slider",
                                 span(class = "inline-label", "Brightness"),
                                 sliderInput("img_brightness", label = NULL, min = 50, max = 200, value = 100, post = "%", width = "220px")
                             ),
                             div(class = "inline-slider",
                                 span(class = "inline-label", "Contrast"),
                                 sliderInput("img_contrast", label = NULL, min = 50, max = 200, value = 100, post = "%", width = "220px")
                             ),
                             div(class = "inline-slider",
                                 span(class = "inline-label", "Gamma"),
                                 sliderInput("img_gamma", label = NULL, min = 50, max = 200, value = 100, post = "%", width = "220px")
                             )
                         )
                     )
                   ),
                   fluidRow(
                     column(
                       width = 6,
                       actionBttn(
                         inputId = "xray_delete_last_point",
                         block = TRUE,
                         size = "md",
                         label = "Delete Last",
                         style = "jelly",
                         color = "success",
                         icon = icon("delete-left")
                       )
                     ),
                     column(
                       width = 6,
                       actionBttn(
                         size = "md",
                         inputId = "xray_reset_points",
                         block = TRUE,
                         label = "Reset",
                         style = "unite",
                         color = "danger",
                         icon = icon("trash-can")
                       )
                     )
                   ),
                   fluidRow(
                     textOutput(outputId = "l1_s1_distance")
                   )
               ),
               fluidRow(
                 column(12, 
                        switchInput(inputId = "flag_xray", label = "Flag Xray for Abnormality or Question", value = FALSE, size = "normal"),
                        br(),
                        tags$head(
                          tags$style(HTML("
    #redcap_upload_completion {
      font-size: 28px;        /* large */
      font-weight: 700;       /* bold */
      font-style: italic;     /* italics */
      color: #0B6623;         /* dark green */
    }
  "))
                        ),
                        textOutput("redcap_upload_completion")
                 )
               )
               
        ),
        
        ########## MAIN PAGE COLUMN 2 STARTS HERE: ##############
        ########## MAIN PAGE COLUMN 2 STARTS HERE: ##############
        ########## MAIN PAGE COLUMN 2 STARTS HERE: ##############
        column(width = 6,
               fluidRow(
                 conditionalPanel(
                   "input.all_points_recorded == true",
                   actionBttn(inputId = "upload_to_redcap", 
                              label = "Upload Coordinates to Redcap", 
                              icon = icon("upload"), style = "jelly", color = "primary", size = "md")
                 )
               ),
               h3("Coordinates Plot:"),
               plotOutput(outputId = "spine_coord_plot"), 
               br(),
               hr(), 
               br(),
               tableOutput(outputId = "click_coordinates_df"), 
               box(title = "Coordinate Data:",
                   collapsible = TRUE, collapsed = TRUE,
                   # tableOutput("alignment_parameters_df"),
                   hr(),
                   br(),
                   verbatimTextOutput(outputId = "click_coordinates_text")
               )
        )
        
        ########## MAIN PAGE COLUMN 3 STARTS HERE: ##############
        ########## MAIN PAGE COLUMN 3 STARTS HERE: ##############
        ########## MAIN PAGE COLUMN 3 STARTS HERE: ##############
        
      )
    )
  )
)


############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################

# Server logic
server <- function(input, output, session) {
  
  pixel_spacing_reactive_values <- reactiveValues(
    spacing = NULL,
    pts = data.frame(point = integer(), x_px = numeric(), y_px = numeric(),
                     x_mm = numeric(), y_mm = numeric(), stringsAsFactors = FALSE)
  )
  spine_orientation <- reactiveVal("right")
  click_coord_reactive_list <- reactiveValues(coords = list())
  plot_points_coordinates_reactiveval <- reactiveVal(list())
  
  clear_click_state <- function() {
    click_coord_reactive_list$coords <- list()
    plot_points_coordinates_reactiveval(list())
    session$sendCustomMessage("plot-coordinates", list(coords = list()))
  }
  
  looks_like_dicom <- function(path) {
    info <- tryCatch(file.info(path), error = function(e) NULL)
    if (is.null(info) || is.na(info$size) || info$size < 132) return(FALSE)
    con <- file(path, "rb"); on.exit(close(con), add = TRUE)
    seek(con, where = 128, origin = "start")
    ident <- rawToChar(readBin(con, what = "raw", n = 4))
    identical(ident, "DICM")
  }
  
  
  # helper: recompute the coords list for JS + reactiveVal
  .current_coords_for_js <- function() {
    unname(lapply(click_coord_reactive_list$coords, function(c) list(x = c$x, y = c$y)))
  }
  
  
  
  patient_id_reactive_val <- reactiveVal("0")
  
  get_next_incomplete_id <- function(next_id = TRUE) {
    # rcon <- redcapConnection(url = 'https://redcap.uthscsa.edu/REDCap/api/', token = xx, config =  httr::config(ssl_verifypeer = FALSE))    
    
    patient_records_all_df <-     as_tibble(exportRecordsTyped(rcon = rcon,drop_fields = "dicom",
                                                               fields = c("record_id", "spine_dicom_coordinates_user_complete"),
                                                               filter_empty_rows = FALSE)
    )

    incompleted_df <- patient_records_all_df %>%
      select(record_id, spine_dicom_coordinates_user_complete) %>%
      filter(spine_dicom_coordinates_user_complete != "Complete") %>%
      filter(as.numeric(record_id) > 350)
    
    if(next_id){
      sample(incompleted_df$record_id, size = 1)
      
      # incompleted_df$record_id[[1]]
    }else{
      
      nrow(incompleted_df)
    }
  }
  
  # get_next_incomplete_id <- function(next_id = TRUE) {
  #   # rcon <- redcapConnection(url = 'https://redcap.uthscsa.edu/REDCap/api/', token = xx, config =  httr::config(ssl_verifypeer = FALSE))    
  #   
  #   patient_records_all_df <-     as_tibble(exportRecordsTyped(rcon = rcon,drop_fields = "dicom",
  #                                                              fields = c("record_id", "spine_dicom_coordinates_complete"),
  #                                                              filter_empty_rows = FALSE)
  #   )
  #   
  #   completed_records_vector <- unique((filter(patient_records_all_df, spine_dicom_coordinates_complete == "Complete"))$record_id)
  #   
  #   if(next_id){
  #     next_incomplete_id <- (patient_records_all_df %>%
  #                              filter(record_id %in% completed_records_vector == FALSE) %>%
  #                              select(record_id) %>%
  #                              distinct() %>%
  #                              pull(record_id))[[1]]
  #     
  #     next_incomplete_id 
  #   }else{
  #     length(unique(patient_records_all_df$record_id)) - length(completed_records_vector)
  #   }
  # }
  

  
  output$number_of_records_remaining <- renderText({
    
    next_id <- patient_id_reactive_val()
    
    req(next_id)
    paste("Number of Records Remaining =",get_next_incomplete_id(next_id = FALSE))
    
  })
  
  
  observeEvent(input$pull_next_record_button, ignoreInit = TRUE, {
    # rcon <- redcapConnection(url = 'https://redcap.uthscsa.edu/REDCap/api/', token = "xx", config =  httr::config(ssl_verifypeer = FALSE)) 
    
    withProgress(message = "Retrieving next record…", value = 0, {
      # 1) Find next record
      
      if(str_length(as.character(input$next_record_id_input))==4){
        next_id <- as.character(input$next_record_id_input)
      }else{
        next_id <- get_next_incomplete_id()
        if (is.null(next_id)) {
          showNotification("No incomplete REDCap records found.", type = "warning")
          return()
        } 
      }
      
      patient_id_reactive_val(next_id)
      incProgress(0.2, detail = glue::glue("Patient {next_id}"))
      
      # 2) Pull the DICOM from REDCap into a temp folder
      tmp_dir <- file.path(tempdir(), "redcap_dicom_pull")
      if (!dir.exists(tmp_dir)) dir.create(tmp_dir, recursive = TRUE)
      pre_files <- list.files(tmp_dir, full.names = TRUE)
      
      
      
      tryCatch({
        redcapAPI::exportFiles(
          rcon   = rcon,
          record = next_id,
          field  = "dicom",
          dir    = tmp_dir
        )
      }, error = function(e) {
        showNotification(paste("REDCap export failed:", e$message), type = "error")
        return()
      })
      
      new_files <- setdiff(list.files(tmp_dir, full.names = TRUE), pre_files)
      if (length(new_files) == 0) {
        showNotification("No file returned from REDCap for this record.", type = "error")
        return()
      }
      
      new_file <- {
        lat_hits <- new_files[grepl("LAT", basename(new_files), ignore.case = TRUE)]
        if (length(lat_hits) >= 1) lat_hits[[1]] else new_files[[which.max(file.info(new_files)$mtime)]]
      }
      
      # 3) Ensure dicoms/ folder has only this one file
      # base_dir <- "dicoms"
      # if (dir.exists(base_dir)) unlink(base_dir, recursive = TRUE, force = TRUE)
      # dir.create(base_dir, recursive = TRUE)
      # 
      # dest <- file.path(base_dir, basename(new_file))
      # ok <- file.rename(new_file, dest)
      # if (!ok) {
      #   file.copy(new_file, dest, overwrite = TRUE)
      #   unlink(new_file)
      # }
      
      # 3) Ensure the dicoms folder contains only this one file
      base_dir <- DICOM_DIR  # <- use the absolute path
      if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)
      
      # remove existing files inside, but keep the directory
      old <- list.files(base_dir, full.names = TRUE, all.files = FALSE, no.. = TRUE)
      if (length(old)) file.remove(old)
      
      # always use a safe basename
      safe_name <- gsub("[^A-Za-z0-9._-]", "_", basename(new_file))
      dest <- file.path(base_dir, safe_name)
      
      # move (or copy if cross-filesystem)
      ok <- file.rename(new_file, dest)
      if (!ok) {
        ok <- file.copy(new_file, dest, overwrite = TRUE)
        if (ok) unlink(new_file)
      }
      if (!ok) stop("Could not save DICOM to: ", dest)
      
      incProgress(0.6, detail = "Processing DICOM…")
      
      
      process_image_path(
        path  = dest,
        name  = basename(dest),
        mime  = "application/dicom",
        session = session,
        pixel_spacing_reactive_values = pixel_spacing_reactive_values,
        spine_orientation = spine_orientation,
        patient_id_reactive_val = patient_id_reactive_val,
        clear_click_state = clear_click_state
      )
      
      incProgress(1, detail = "Done")
    })
  })
  
  
  output$patient_id <- renderText({
    pid <- patient_id_reactive_val()
    if (!is.null(pid) && nzchar(pid) && pid != "0") glue("Patient ID: {pid}") else "Patient ID not found"
  })
  
  
  
  observeEvent(input$image, {
    req(input$image$datapath)
    process_image_path(
      path  = input$image$datapath[1],
      name  = (input$image$name %||% "")[1],
      mime  = (input$image$type %||% "")[1],
      session = session,
      pixel_spacing_reactive_values = pixel_spacing_reactive_values,
      spine_orientation = spine_orientation,
      patient_id_reactive_val = patient_id_reactive_val,
      clear_click_state = clear_click_state
    )
  })
  
  
  # observeEvent(input$image, {
  #   req(input$image$datapath)
  # 
  #   # helpers
  #   get_hdr_df <- function(hdr) if (is.list(hdr) && !is.data.frame(hdr)) hdr[[1]] else hdr
  #   get_tag <- function(hdr, name) {
  #     df <- get_hdr_df(hdr); v <- df$value[df$name == name]
  #     if (length(v)) as.character(v[[1]]) else NA_character_
  #   }
  # parse_nums <- function(x) {
  #   if (is.null(x) || length(x) == 0 || is.na(x[[1]])) return(numeric())
  #   as.numeric(strsplit(as.character(x[[1]]), "\\\\|,|\\s+")[[1]])
  # }
  #   to_data_url_png <- function(raw_png) paste0("data:image/png;base64,", base64enc::base64encode(raw_png))
  # 
  #   # first file only
  #   path <- input$image$datapath[1]
  #   name <- tolower((input$image$name %||% "")[1])
  #   mime <- (input$image$type %||% "")[1]
  # 
  #   is_dicom <-
  #     grepl("\\.(dcm|dicom)$", name) ||
  #     mime %in% c("application/dicom", "application/dicom+json", "application/octet-stream") ||
  #     looks_like_dicom(path)
  # 
  #   if (is_dicom) {
  #     dcm <- tryCatch(oro.dicom::readDICOMFile(path),
  #                     error = function(e) { showNotification(paste("DICOM read failed:", conditionMessage(e)), type = "error"); NULL })
  #     req(!is.null(dcm))
  # 
  #     img <- dcm$img
  #     hdr <- dcm$hdr
  # 
  #     # rescale + window
  #     rs <- suppressWarnings(parse_nums(get_tag(hdr, "RescaleSlope"))[1]);      if (is.finite(rs)) img <- img * rs
  #     ri <- suppressWarnings(parse_nums(get_tag(hdr, "RescaleIntercept"))[1]);  if (is.finite(ri)) img <- img + ri
  #     wc <- suppressWarnings(parse_nums(get_tag(hdr, "WindowCenter"))[1])
  #     ww <- suppressWarnings(parse_nums(get_tag(hdr, "WindowWidth"))[1])
  #     if (is.finite(wc) && is.finite(ww) && ww > 0) {
  #       low <- wc - ww/2; high <- wc + ww/2
  #       img[img < low] <- low; img[img > high] <- high
  #     }
  # 
  #     # normalize to [0,1]
  #     rng <- range(img, finite = TRUE)
  #     img01 <- if (diff(rng) > 0) (img - rng[1]) / diff(rng) else matrix(0, nrow(img), ncol(img))
  # 
  #     # invert if MONOCHROME1
  #     pm <- tolower(get_tag(hdr, "PhotometricInterpretation"))
  #     if (identical(pm, "monochrome1")) img01 <- 1 - img01
  # 
  #     # PixelSpacing (mm per ORIGINAL pixel): prefer PixelSpacing, else ImagerPixelSpacing
  #     ps <- parse_nums(get_tag(hdr, "PixelSpacing"))
  #     if (length(ps) < 2 || any(!is.finite(ps[1:2]))) ps <- parse_nums(get_tag(hdr, "ImagerPixelSpacing"))
  # 
  #     long_edge_height <- if_else(is.numeric(parse_nums(get_tag(hdr, "FieldOfViewDimensions"))[1]), parse_nums(get_tag(hdr, "FieldOfViewDimensions"))[1], 1650)
  # 
  #     # --- compress uniformly to fit a target long edge, then flip upright ---
  #     old_rows <- nrow(img01); old_cols <- ncol(img01)
  #     target_long_edge <- long_edge_height  # << adjust as needed (max of width/height in pixels)
  #     scale <- min(1, target_long_edge / max(old_cols, old_rows))  # 0<scale<=1 (no upscaling)
  # 
  #     rc <- rawConnection(raw(), "wb"); png::writePNG(img01, target = rc); raw_png <- rawConnectionValue(rc); close(rc)
  #     im <- magick::image_read(raw_png)
  #     if (scale < 1) {
  #       im <- magick::image_resize(im, sprintf("%d%%", as.integer(round(scale * 100))))  # uniform scale
  #     }
  #     im <- magick::image_flip(im)  # keep superior at top for browser display
  #     out <- magick::image_write(im, format = "png")
  #     session$sendCustomMessage("load-image", list(src = to_data_url_png(out)))
  # 
  #     # --- store ADJUSTED spacing (mm per DISPLAY pixel) for later distance calc ---
  #     # If we downscale by 'scale', one display pixel spans 1/scale original pixels.
  #     # So mm_per_display_px = mm_per_original_px / scale (uniform).
  #     if (length(ps) >= 2 && all(is.finite(ps[1:2]))) {
  #       pixel_spacing_reactive_values$spacing <- c(ps[1] / scale, ps[2] / scale)  # c(row_mm, col_mm) per DISPLAY px
  #     } else {
  #       pixel_spacing_reactive_values$spacing <- NULL
  #       showNotification("No PixelSpacing present; distances will be in pixels.", type = "warning", duration = 6)
  #     }
  # 
  #     if(str_detect(get_tag(hdr, "PatientOrientation"), "P")){
  #       spine_orientation("left")
  #     }else{
  #       spine_orientation("right")
  #     }
  # 
  #     if(!is.na(get_tag(hdr = hdr, name = "PatientID"))){
  #       patient_id_reactive_val(as.double(get_tag(hdr = hdr, name = "PatientID")))
  #     }else{
  # 
  #     }
  # 
  # 
  #     # patient_id_reactive_val
  # 
  #     # reset points table
  #     pixel_spacing_reactive_values$pts <- data.frame(point = integer(), x_px = numeric(), y_px = numeric(),
  #                                                     x_mm = numeric(), y_mm = numeric(), stringsAsFactors = FALSE)
  # 
  #     updateCheckboxInput(session, "xray_file_uploaded", value = TRUE)
  # 
  #   } else {
  #     # Non-DICOM passthrough; no pixel spacing available
  #     b64 <- base64enc::base64encode(path)
  #     mime_out <- if (grepl("^image/", mime)) mime else "image/*"
  #     session$sendCustomMessage("load-image", list(src = paste0("data:", mime_out, ";base64,", b64)))
  #     pixel_spacing_reactive_values$spacing <- NULL
  #     pixel_spacing_reactive_values$pts <- data.frame(point = integer(), x_px = numeric(), y_px = numeric(),
  #                                                     x_mm = numeric(), y_mm = numeric(), stringsAsFactors = FALSE)
  #     updateCheckboxInput(session, "xray_file_uploaded", value = TRUE)
  #   }
  # })
  
  
  
  # Helper to send current filter state
  send_filters <- function(){
    session$sendCustomMessage("apply-filters", list(
      brightness = input$img_brightness %||% 100,
      contrast   = input$img_contrast   %||% 100,
      gamma      = input$img_gamma      %||% 100,
      invert     = isTRUE(input$img_invert)
    ))
  }
  
  observe({
    req(input$xray_file_uploaded)  # only after image is up
    # Re-send whenever any control changes
    junk <- list(input$img_brightness, input$img_contrast, input$img_gamma, input$img_invert)
    send_filters()
  })
  
  observeEvent(input$img_reset_filters, {
    updateSliderInput(session, "img_brightness", value = 100)
    updateSliderInput(session, "img_contrast",   value = 100)
    updateSliderInput(session, "img_gamma",      value = 100)
    updateCheckboxInput(session, "img_invert",   value = FALSE)
    send_filters()
  })
  
  # After a new image loads, also apply the current filters (so state persists)
  observeEvent(input$xray_file_uploaded, {
    send_filters()
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$image, {
    req(input$image)
    updateSwitchInput(session = session, inputId = "xray_file_uploaded", value = TRUE)
  })
  
  
  
  
  observeEvent(input$spine_orientation_button, ignoreInit = TRUE, {
    # Update the spine orientation value
    if (spine_orientation() == "left") {
      spine_orientation("right")
    } else {
      spine_orientation("left")
    }
  })
  
  
  
  observeEvent(spine_orientation(), {
    # Update the spine orientation value
    if (spine_orientation() == "left") {
      updateActionButton(session, "spine_orientation_button", label = "Facing LEFT", icon = icon("arrow-left"))
    } else {
      updateActionButton(session, "spine_orientation_button", label = "Facing RIGHT", icon = icon("arrow-right"))
    }
  })
  
  
  
  
  # Helper: given what's already clicked, return the next id to collect
  # Always work with IDs
  get_next_point_id_function <- function(clicked_ids, all_ids = spine_point_ids) {
    remaining <- setdiff(all_ids, clicked_ids)   # compares IDs to IDs
    if (length(remaining)) remaining[[1]] else NA_character_
  }
  
  # Safe label lookup (no error if id missing)
  instruction_for_id_function <- function(id) {
    if (is.na(id)) return(NULL)
    if (!id %in% names(spine_points_vector)) return(NULL)
    unname(spine_points_vector[id])  # returns NA instead of error if absent
  }
  
  observeEvent(input$xray_click, ignoreInit = TRUE, {
    clicked_ids <- names(click_coord_reactive_list$coords)
    next_id <- get_next_point_id_function(clicked_ids)
    
    if (!is.na(next_id)) {
      click_coord_reactive_list$coords[[next_id]] <- list(
        x = input$xray_click$x, y = input$xray_click$y
      )
      
      
      coords_for_js <- .current_coords_for_js()
      plot_points_coordinates_reactiveval(coords_for_js)
      session$sendCustomMessage('plot-coordinates', list(coords = coords_for_js))
    }
  })
  
  
  # observeEvent(click_coord_reactive_list$coords, ignoreInit = TRUE, {
  #   clicked_ids <- names(click_coord_reactive_list$coords)
  # 
  #   if(length(isTRUE(str_detect(clicked_ids, "t12"))) == 2){
  #     
  #     coord_named_list <- purrr::map(coords, ~ c(.x$x, .x$y))
  #     
  #     if (spine_orientation() == "left") {
  #       
  #       if(coord_named_list$t12_superior_anterior$x > coord_named_list$t12_superior_posterior$x){
  #         sendSweetAlert(session, title = "Suspected error", text = "did you click only the superior endplates of the last vertebra?", type = "error")
  #       }
  #       }else{
  #         if(coord_named_list$t12_superior_anterior$x < coord_named_list$t12_superior_posterior$x){
  #           sendSweetAlert(session, title = "Suspected error", text = "did you click only the superior endplates of the last vertebra?", type = "error")
  #         }
  #     }
  #   }
  # })
  
  endplate_orientation_bad <- function(coords, level = "t12", plate = "superior") {
    sa <- coords[[paste0(level, "_", plate, "_anterior")]]
    sp <- coords[[paste0(level, "_", plate, "_posterior")]]
    
    # if either missing, don't flag (caller should guard with req/all())
    if (is.null(sa) || is.null(sp)) return(FALSE)
    
    sa_x <- sa$x
    sp_x <- sp$x
    
    ori <- spine_orientation()  # "left" or "right"
    (ori == "left"  && sa_x > sp_x) ||
      (ori == "right" && sa_x < sp_x)
  }
  
  observeEvent(click_coord_reactive_list$coords, ignoreInit = TRUE, {
    coords <- click_coord_reactive_list$coords
    needed <- c("t12_superior_anterior", "t12_superior_posterior")
    
    # fire only when both t12 superior points exist
    if (!all(needed %in% names(coords))) return(NULL)
    
    if (endplate_orientation_bad(coords, level = "t12", plate = "superior")) {
      sendSweetAlert(
        session,
        title = "Suspected error",
        text  = "It looks like you clicked the inferior Coordinate - Only superior endplates from here until C2 need clicked.",
        type  = "error"
      )
    }
  })
  # If you support delete/reset elsewhere, always re-send the current set:
  
  
  observeEvent(list(input$xray_click, input$xray_delete_last_point, input$xray_reset_points), {
    req(!is.null(plot_points_coordinates_reactiveval()))
    session$sendCustomMessage('plot-coordinates',
                              list(coords = plot_points_coordinates_reactiveval()))
  })
  
  
  observeEvent(input$xray_delete_last_point, {
    clicked_ids <- names(click_coord_reactive_list$coords)
    req(length(clicked_ids) > 0)
    
    # Use the SAME id vector used everywhere else
    ordered_clicked <- intersect(spine_point_ids, clicked_ids)
    req(length(ordered_clicked) > 0)
    
    last_id <- tail(ordered_clicked, 1)
    # remove
    click_coord_reactive_list$coords[[last_id]] <- NULL
    
    # push fresh coords to your reactiveVal and to the browser
    coords_for_js <- .current_coords_for_js()
    plot_points_coordinates_reactiveval(coords_for_js)
    session$sendCustomMessage('plot-coordinates', list(coords = coords_for_js))
  })
  
  
  observeEvent(input$xray_reset_points, ignoreInit = TRUE, {
    click_coord_reactive_list$coords <- list()
  })
  
  
  xray_instructions_reactiveval <- reactiveVal("x")
  
  # Render instructions dynamically based on the number of recorded clicks
  # instructions text
  output$xray_click_instructions <- renderText({
    clicked_ids <- names(click_coord_reactive_list$coords)
    next_id <- get_next_point_id_function(clicked_ids)
    
    total  <- length(spine_point_ids)     # <-- use IDs, not the mapping vector
    n_done <- length(clicked_ids)
    
    if (is.na(next_id)) {
      xray_instructions_reactiveval("Completed")
      return(HTML('<div>All points recorded.</div>'))
    }
    
    nice_label <- instruction_for_id_function(next_id)
    xray_instructions_reactiveval("x")
    
    HTML(glue::glue('<div>Click: <span>{nice_label}</span></div>'))
  })
  
  
  output$xray_instruction_plot <- renderPlot({
    req(input$xray_file_uploaded)
    
    clicked_ids <- names(click_coord_reactive_list$coords)
    next_id <- get_next_point_id_function(clicked_ids)
    
    # Done state
    if (is.na(next_id)) {
      return(
        ggplot() +
          annotation_raster(im_raster_left, xmin = 0, xmax = w, ymin = 0, ymax = h) +
          coord_fixed(xlim = c(0, w), ylim = c(0, h), expand = FALSE) +
          theme_void() +
          annotate("label", x = 0.5*w, y = 0.1*h, label = "All points recorded.", size = 8)
      )
    }
    
    
    if(any(next_id == spine_instruction_points_norm$id)){
      spine_instruction_point_df <- spine_instruction_points_norm
      # Guide point (left-facing coordinates by convention)
      instruction_point_df <- dplyr::filter(spine_instruction_points_norm, id == next_id)
      
      instruction_raster_image_right <- im_raster_right
      instruction_raster_image_left <- im_raster_left
      
      req(nrow(instruction_point_df) == 1)
      
      orient <- spine_orientation()   # <-- READ the reactiveVal
      
      print(orient)
      
      if (identical(orient, "right")) {
        im_raster <- instruction_raster_image_right
        instruction_point_df$x <- w - instruction_point_df$x   # mirror the dot
        # if(instruction_point_df$x < w*0.5){
        #   hor_just <- 0
        # }else{
        #   hor_just <- 1 
        # }
        
      } else {
        im_raster <- instruction_raster_image_left
        # if(instruction_point_df$x < w*0.5){
        #   hor_just <- 0
        # }else{
        #   hor_just <- 1 
        # }
      }
      if(instruction_point_df$x < w*0.5){
        hor_just <- 0
      }else{
        hor_just <- 1 
      }
      
      nice_label <- instruction_for_id_function(next_id)
      
      ggplot() +
        annotation_raster(im_raster, xmin = 0, xmax = w, ymin = 0, ymax = h) +
        geom_point(data = instruction_point_df, aes(x, y), color = "yellow", size = 4) +
        coord_fixed(xlim = c(0, w), ylim = c(0, h), expand = FALSE) +
        theme_void() +
        annotate("label",
                 x = instruction_point_df$x, y = instruction_point_df$y, hjust = hor_just,
                 label = glue("Click:\n{nice_label}"),
                 vjust = -1, label.size = 0.25, fontface = "bold", fill = "black", color = "yellow", alpha = 0.9)
      
    }else if(any(next_id == spine_coord_instructions_right_df$spine_point)){
      
      orient <- spine_orientation()   # <-- READ the reactiveVal
      
      if (identical(orient, "right")) {
        
        next_point_df <- spine_coord_instructions_right_df %>%
          filter(spine_point == next_id)
        
        polys_sf <- polys_right_sf
        
        x_range <- c(min(spine_coord_instructions_right_df$x) - 20, max(spine_coord_instructions_right_df$x) + 20)
        
      }else{
        next_point_df <- spine_coord_instructions_left_df %>%
          filter(spine_point == next_id)
        
        polys_sf <- polys_left_sf
        x_range <- c(min(spine_coord_instructions_left_df$x) - 20, max(spine_coord_instructions_left_df$x) + 20)
      }
      
      point_fill_color <- if_else(str_detect(next_point_df$spine_point, "superior"), "green", "purple")
      
      plot_title <- if_else(str_detect(next_point_df$spine_point, "superior"), "Superior Endplate", "Inferior Endplate")
      # --- plot ---
      ggplot() +
        geom_sf(data = polys_sf, fill = "grey85", color = "grey25", linewidth = 0.3) +
        geom_point(data = next_point_df, aes(x = x, y = y), color = "darkred", size = 7) +
        geom_point(data = next_point_df, aes(x = x, y = y), color = point_fill_color, size = 6) +
        coord_sf(expand = FALSE, 
                 ylim = c(next_point_df$y - 100, next_point_df$y + 100), 
                 xlim = x_range) +
        labs(title = plot_title) +
        theme_void() + 
        theme(plot.title = element_text(hjust = 0.5, size = 18))
    }else{
      
      skull_orientation <- spine_orientation()   # <-- READ the reactiveVal
      
      if(skull_orientation == "left"){
        skull_image <- skull_raster_left
        skull_c1_instructions_df <- skull_c1_instructions_left_df
      }else{
        skull_image <- skull_raster_right
        skull_c1_instructions_df <- skull_c1_instructions_right_df
      }
      
      nice_label <- filter(skull_c1_instructions_df, id == next_id)$label
      
      current_point_df <- skull_c1_instructions_df %>%
        filter(id == next_id)
      
      if(current_point_df$x < width_skull*0.5){
        h_just <- 0
      }else{
        h_just <- 1
      }
      
      ggplot() +
        annotation_raster(skull_image, 
                          xmin = 0, 
                          xmax = width_skull, 
                          ymin = 0, 
                          ymax = height_skull) +
        geom_point(data = current_point_df, aes(x, y), color = "yellow", size = 4) +
        coord_fixed(xlim = c(0, width_skull), ylim = c(0, height_skull), expand = FALSE) +
        annotate("label",
                 x = current_point_df$x,
                 y = current_point_df$y,
                 hjust = h_just,
                 label = glue("Click:\n{nice_label}"),
                 vjust = -1,
                 label.size = 0.25, fontface = "bold", fill = "black", color = "yellow", alpha = 0.9)  +
        theme_void()
    }
    
    
  })
  
  
  
  ################################# MANAGE THE PLOT CLICKS #########################
  ################################# MANAGE THE PLOT CLICKS #########################
  ################################# MANAGE THE PLOT CLICKS #########################
  ################################# MANAGE THE PLOT CLICKS #########################
  ################################# MANAGE THE PLOT CLICKS #########################
  
  # Create a reactive table to display coordinates
  click_coordinates_df_reactive <- reactive({
    if (length(click_coord_reactive_list$coords) > 0) {
      # Convert the list to a tibble
      click_coordinates_df_reactive_df <- tibble(
        spine_point = names(click_coord_reactive_list$coords),
        x = map_dbl(click_coord_reactive_list$coords, "x"),
        y = map_dbl(click_coord_reactive_list$coords, "y")
      )%>%
        mutate(level = map(.x = spine_point, .f = ~ identify_level_by_spine_point_function(spine_point = .x))) %>%
        unnest() %>%
        mutate(spine_marker = case_when(
          level == "pelvis" ~ spine_point, 
          level == "skull" ~ spine_point, 
          # level == "c1" ~ str_remove_all(spine_point, "c1_"), 
          level != "pelvis" & str_detect(spine_point, "inferior_anterior") ~ "inferior_anterior",
          level != "pelvis" & str_detect(spine_point, "inferior_posterior") ~ "inferior_posterior",
          level != "pelvis" & str_detect(spine_point, "superior_anterior") ~ "superior_anterior",
          level != "pelvis" & str_detect(spine_point, "superior_posterior") ~ "superior_posterior"
        )) %>%
        select(level, spine_marker, x, y) %>%
        mutate(level = fct_inorder(level)) 
      
      jh_add_missing_inferior_corners(click_coordinates_df_reactive_df) %>%
      distinct()

        # click_coordinates_df_reactive_filled_df <-  jh_add_missing_inferior_corners(click_coordinates_df_reactive_df) %>%
        # distinct()

        # if(nrow(filter(click_coordinates_df_reactive_filled_df, level == levels_need_inferior_coord_vector[[1]]))==4){
        #   poly_area <- function(x, y) {
        #     # shoelace
        #     0.5 * abs(sum(x * c(y[-1], y[1]) - y * c(x[-1], x[1])))
        #   }
        #   
        #   lumbar_vert_area <- click_coordinates_df_reactive_filled_df %>%
        #     filter(level == "l3")%>%
        #     summarise(area = poly_area(x, y), .groups = "drop") %>%
        #     pull(area)
        #   
        #   most_recent_vert_area <- click_coordinates_df_reactive_filled_df %>%
        #     filter(level == levels_need_inferior_coord_vector[[1]])%>%
        #     summarise(area = poly_area(x, y), .groups = "drop") %>%
        #     pull(area)
        #   
        #   if(most_recent_vert_area < 0.25*lumbar_vert_area){
        #     click_coordinates_df_reactive_filled_df
        #     sendSweetAlert(session, title = "Suspected error", text = "did you click only the superior endplates of the last vertebra?", type = "error")
        #     
        #   }else{
        #     click_coordinates_df_reactive_filled_df
        #   }
        #   
        #   
        # }else{
        #   click_coordinates_df_reactive_filled_df
        # }
      
      
    } else {
      tibble(spine_point = character(), x = double(), y = double())
    }
  })
  
  output$click_coordinates_df <- renderTable({
    if (nrow(click_coordinates_df_reactive()) > 0) {
      click_coordinates_df_reactive()
    } else {
      tibble(spine_point = character(), x = double(), y = double())
    }
  })
  
  output$click_coordinates_text <- renderPrint({
    if (nrow(click_coordinates_df_reactive()) > 0) {
      dput(click_coordinates_df_reactive())
    } else{
      ""
    }
    
  })
  
  spine_coord_plot_reactive <- reactive({
    click_coord_df <- click_coordinates_df_reactive()
    
    if (nrow(click_coord_df) > 1) {
    
      click_coord_to_plot <- click_coord_df %>%
        group_by(level) %>% 
        add_tally() %>%
        filter(n > 1) %>%
        select(-n) %>%
        ungroup()
      
      vert_to_plot_df <- jh_smooth_spine_vertebral_coordinates_function(dat = click_coord_to_plot) %>%
        filter(level != "pelvis") %>%
        filter(level != "skull") %>%
        group_by(level) %>%
        add_tally() %>%
        filter(n == 4) %>%
        ungroup() 
      
      # if(nrow(filter(click_coordinates_df_reactive_filled_df, level == levels_need_inferior_coord_vector[[1]]))==4){
      #   poly_area <- function(x, y) {
      #     # shoelace
      #     0.5 * abs(sum(x * c(y[-1], y[1]) - y * c(x[-1], x[1])))
      #   }
      #   
      #   lumbar_vert_area <- click_coordinates_df_reactive_filled_df %>%
      #     filter(level == "l3")%>%
      #     summarise(area = poly_area(x, y), .groups = "drop") %>%
      #     pull(area)
      #   
      #   most_recent_vert_area <- click_coordinates_df_reactive_filled_df %>%
      #     filter(level == levels_need_inferior_coord_vector[[1]])%>%
      #     summarise(area = poly_area(x, y), .groups = "drop") %>%
      #     pull(area)
      #   
      #   if(most_recent_vert_area < 0.25*lumbar_vert_area){
      #     click_coordinates_df_reactive_filled_df
      #     sendSweetAlert(session, title = "Suspected error", text = "did you click only the superior endplates of the last vertebra?", type = "error")
      #     
      #   }else{
      #     click_coordinates_df_reactive_filled_df
      #   }
      #   
      #   
      # }else{
      #   click_coordinates_df_reactive_filled_df
      # }
      
      sup_endplates_df <- click_coord_to_plot %>%
        filter(level != "pelvis") %>%
        filter(level != "skull") %>%
        group_by(level) %>%
        add_tally() %>%
        filter(n == 2) %>%
        ungroup() 
      
      if(nrow(vert_to_plot_df) > 3){
        spine_plot <- vert_to_plot_df %>%
          ggplot(aes(x = x, y = y, group = level)) +
          geom_polygon(fill = "grey33") +
          coord_fixed() +
          theme_void() 
        
        if(nrow(sup_endplates_df) > 1){
        spine_plot <- spine_plot +
          geom_path(data = sup_endplates_df, aes(x = x, y = y)) 
        }
        spine_plot
        
      }
    }
  })
  
  output$spine_coord_plot <- renderPlot({
    spine_coord_plot_reactive()
  })
  
  output$spine_coord_plot_confirmation <- renderPlot({
    spine_coord_plot_reactive()
  })
  
  
  distance_mm <- function(p1, p2, spacing) {
    # spacing = c(row_mm, col_mm) per DISPLAY pixel
    row_mm <- spacing[1]; col_mm <- spacing[2]
    dx_mm  <- (p2[1] - p1[1]) * col_mm  # x uses column spacing
    dy_mm  <- (p2[2] - p1[2]) * row_mm  # y uses row spacing
    sqrt(dx_mm^2 + dy_mm^2)
  }
  
  output$l1_s1_distance <- renderText({
    
    if(input$all_points_recorded){
      s1_ant <- c(click_coord_reactive_list$coords$s1_superior_anterior$x,
                  click_coord_reactive_list$coords$s1_superior_anterior$y)
      l1_sup <- c(click_coord_reactive_list$coords$l1_superior_anterior$x,
                  click_coord_reactive_list$coords$l1_superior_anterior$y)
      
      pixel_distance <- jh_calculate_distance_between_2_points_function(point_1 = s1_ant,
                                                                        point_2 = l1_sup)
      
      actual_mm <- distance_mm(s1_ant, l1_sup, pixel_spacing_reactive_values$spacing)
      
      print(paste("Pixel distance = ", pixel_distance, ";\n Actual distance from L1 superior anterior corner to S1 superior anterior corner = ", actual_mm))
    }
    
  })
  
  #################### CONFIRMING MEASURES WITH MEANS #############################
  
  computed_parameters_list_reactive <- reactive({
    
    computed_parameters_list <- list()
    
    coords <- click_coord_reactive_list$coords
    req(length(coords) > 0)
    
    # Make a simple named list: point -> c(x, y)
    coord_named_list <- purrr::map(coords, ~ c(.x$x, .x$y))
    
    needed <- c("fem_head_center","s1_superior_anterior","s1_superior_posterior")
    
    if (all(needed %in% names(coord_named_list))) {
      computed_parameters_list$pelvic_incidence <- jh_compute_pelvic_incidence_from_3_coord_function(
        fem_head_coord = coord_named_list$fem_head_center,
        s1_sa_coord    = coord_named_list$s1_superior_anterior,
        s1_sp_coord    = coord_named_list$s1_superior_posterior
      )
      
      orient <- spine_orientation() 
      
      coord_df <- click_coordinates_df_reactive()
      
      computed_parameters_list$pelvic_tilt <- jh_calculate_tilt_function(coord_df = coord_df, vert_level = "sacrum", orientation = orient)

      if (nrow(filter(coord_df, level == "l1")) == 4) {
        
        l1_tilt_computed <- jh_calculate_tilt_function(coord_df = coord_df, vert_level = "l1", orientation = orient)
        computed_parameters_list$l1pa <- computed_parameters_list$pelvic_tilt + l1_tilt_computed
        
      }
      
      if (nrow(filter(coord_df, level == "t9")) == 4) {
        t9_tilt_computed <- jh_calculate_tilt_function(coord_df = coord_df, vert_level = "t9", orientation = orient)
        computed_parameters_list$t9pa <- computed_parameters_list$pelvic_tilt + t9_tilt_computed
      }
      
      if (nrow(filter(coord_df, level == "t4")) == 4) {
        t4_tilt_computed <- jh_calculate_tilt_function(coord_df = coord_df, vert_level = "t4", orientation = orient)
        computed_parameters_list$t4pa <- computed_parameters_list$pelvic_tilt + t4_tilt_computed
      }
      
      if (nrow(filter(coord_df, level == "c2")) == 4) {
        c2_tilt_computed <- jh_calculate_tilt_function(coord_df = coord_df, vert_level = "c2", orientation = orient)
        computed_parameters_list$c2pa <- computed_parameters_list$pelvic_tilt + c2_tilt_computed
      }
      
    }
    
    computed_parameters_list
  })
  
  
  
  
  redcap_means_measures_reactive_df <- reactive({
    
    # rcon <- redcapConnection(url = 'https://redcap.uthscsa.edu/REDCap/api/', token = "zz", config =  httr::config(ssl_verifypeer = FALSE))  
    
    pid <- patient_id_reactive_val()
    if (!is.null(pid) && nzchar(pid) && pid != "0"){
      patient_means_measures_df <- as_tibble(exportRecordsTyped(rcon = rcon,
                                                                records = pid,
                                                                drop_fields = "dicom",
                                                                fields = c('pelvic_incidence', 'pelvic_tilt', 'l1_pelvic_angle', 't9_pelvic_angle', 't4_pelvic_angle', 'c2_pelvic_angle'),
                                                                filter_empty_rows = FALSE)
      )%>%
        remove_empty()
      
      patient_means_measures_df
      
    }else{
      tibble()
    }
  })
  

  
  output$redcap_means_measures <- renderTable({
    
    
    # redcap_means_measures
    
    redcap_means_measures_reactive_df <- redcap_means_measures_reactive_df()
    req(!is.null(redcap_means_measures_reactive_df), nrow(redcap_means_measures_reactive_df) > 0)
    
    means_measures_long_df <- redcap_means_measures_reactive_df %>%
      dplyr::select(-tidyselect::any_of("record_id")) %>%
      tidyr::pivot_longer(dplyr::everything(), names_to = "measure", values_to = "value") %>%
      dplyr::mutate(measure = stringr::str_to_title(stringr::str_replace_all(measure, "_", " ")))%>%
      dplyr::mutate(measure = str_replace_all(measure, " Pelvic Angle", "PA")
                    )
    
    computed_parameters_list <- computed_parameters_list_reactive()
  
    if ("pelvic_incidence" %in% names(computed_parameters_list)) {
      extra <- NULL
      extra <- enframe(computed_parameters_list) %>%
        unnest(value) %>%
        dplyr::mutate(measure = stringr::str_to_title(stringr::str_replace_all(name, "_", " ")))%>%
        dplyr::mutate(measure = str_replace_all(measure, "pa", "PA")
        ) %>%
        select(measure, new = value)
      
      means_measures_long_df <- means_measures_long_df %>%
        left_join(extra)
      
    }else{
      means_measures_long_df
    }
    

    
  })
  

  
  #### CHECK METRICS ####
  
  observeEvent(computed_parameters_list_reactive(), ignoreInit = TRUE, {
    
    # req(computed_parameters_list_reactive())
    computed_parameters_list <- computed_parameters_list_reactive()
    req("pelvic_incidence" %in% names(computed_parameters_list))
    
    patient_means_measures_df <- redcap_means_measures_reactive_df()
    req(nrow(patient_means_measures_df) > 0, "pelvic_incidence" %in% names(patient_means_measures_df))
    
    recorded_pi <- suppressWarnings(as.numeric(patient_means_measures_df$pelvic_incidence[1]))
    calculated_pi_from_coord <- suppressWarnings(as.numeric(computed_parameters_list$pelvic_incidence))
    req(is.finite(recorded_pi), is.finite(calculated_pi_from_coord))
    if (abs(recorded_pi - calculated_pi_from_coord) > 3) {
      msg <- glue::glue(
        "Pelvic Incidence in Database = {round(recorded_pi,1)}\nvs\nNewly calculated PI = {round(calculated_pi_from_coord,1)}\nPlease delete prior points and adjust."
      )
      sendSweetAlert(session, title = "Error in PI", text = msg, type = "error")
      
      clicked_ids <- names(click_coord_reactive_list$coords)
      req(length(clicked_ids) > 0)
      
      # Use the SAME id vector used everywhere else
      ordered_clicked <- intersect(spine_point_ids, clicked_ids)
      req(length(ordered_clicked) > 0)
      
      last_id <- tail(ordered_clicked, 1)
      # remove
      click_coord_reactive_list$coords[[last_id]] <- NULL
      
      # push fresh coords to your reactiveVal and to the browser
      coords_for_js <- .current_coords_for_js()
      plot_points_coordinates_reactiveval(coords_for_js)
      session$sendCustomMessage('plot-coordinates', list(coords = coords_for_js))
      
    }
    

    
  })
  
  ### ALERT FOR accidentally clicking inferior coordinates when not needed
  
  # observeEvent(input$xray_click, ignoreInit = TRUE, {
  #   
  #   coords <- click_coord_reactive_list$coords
  #   req(length(coords) > 0)
  #   
  #   # Make a simple named list: point -> c(x, y)
  #   coord_named_list <- purrr::map(coords, ~ c(.x$x, .x$y))
  #   
  #   
  # 
  #   
  #   })
  
  

  
  
  #### redcap ####
  
  redcap_tables <- reactiveValues()
  
  observeEvent(input$upload_to_redcap, ignoreInit = TRUE, {
    
    if(input$all_points_recorded){
      pid <- patient_id_reactive_val()
      
      redcap_tables$spine_coordinates_df <- click_coordinates_df_reactive() %>%
        mutate(record_id = pid,
               redcap_repeat_instrument = "spine_dicom_coordinates",
               redcap_repeat_instance = row_number())  %>%
        select(record_id, redcap_repeat_instrument, redcap_repeat_instance, spine_level = level, spine_marker, x_coordinate = x, y_coordinate = y)  %>%
        mutate(spine_dicom_coordinates_complete = "Complete")
      
      

    }
  })
  
  output$spine_coord_redcap_table <- renderTable({
    req(redcap_tables$spine_coordinates_df)
    # computed_parameters_list <- computed_parameters_list_reactive()
    # 
    # computed_metrics_df <- if ("pelvic_incidence" %in% names(computed_parameters_list)) {
    #   tibble::tibble(measure = `Pelvic Incidence`, value = computed_parameters_list$pelvic_incidence)
    # } else NULL
    # 
    # print(computed_parameters_list$pelvic_incidence)
    
    # if(any(names(computed_parameters_list) == "pelvic_incidence")){
    #  computed_metrics_df <- tibble(measure = "Pelvic Incidence", value = computed_parameters_list$pelvic_incidence)
    #    
    # }else{
    #   computed_metrics_df <- tibble()
    # }
    # redcap_tables$spine_coordinates_df %>%
    #   left_join(computed_metrics_df)
    
    redcap_tables$spine_coordinates_df
  })
  
  observeEvent(input$upload_to_redcap, ignoreInit = TRUE, {
    showModal(
      modalDialog(
        footer = "Redcap Upload", easyClose = TRUE, size = "l",
        box(width = 12, title = "Upload Data to Redcap", footer = NULL,
            fluidRow(
              actionBttn("confirm_upload_final", "Confirmed, Upload to Redcap",
                         style = "simple", color = "primary")
            ),
            br(),
            fluidRow(
              column(
                6,
                div(
                  style = "max-height:60vh; overflow:auto; border:1px solid #ddd; padding:6px;",
                  # stop text wrapping so horizontal scroll appears
                  tags$style("#spine_coord_redcap_table table { white-space: nowrap; }"),
                  tableOutput("spine_coord_redcap_table")
                )
              ),
              column(6, plotOutput("spine_coord_plot_confirmation"))
            )
        )
      )
    )
  })
  
  redcap_upload_complete_reactiveval <- reactiveVal("Incomplete")
  
  observeEvent(input$confirm_upload_final, ignoreInit = TRUE, {
    
    # rcon <- redcapConnection(url = 'https://redcap.uthscsa.edu/REDCap/api/', token = "zz", 
    #                          config =  httr::config(ssl_verifypeer = FALSE))
    
    pid <- patient_id_reactive_val()
    
    withProgress(message = 'Uploading Data', value = 0, {
      number_of_steps <- 3
      incProgress(1/number_of_steps, detail = paste("Uploading User & Time"))
      
      user_time_flag_df <- tibble(record_id = pid, user_last_name = input$last_name,
                                                          date_time_upload = paste(Sys.time()),
                                  flag_xray = input$flag_xray,
                                  flag_xray_reason = input$flag_xray_reason,
                                                          spine_dicom_coordinates_user_complete = "Complete")
        # flag_xray
      
      
      redcapAPI::importRecords(rcon = rcon, data = user_time_flag_df, 
                               returnContent = "count")
      
      incProgress(1/number_of_steps, detail = paste("Uploading Coordinates"))
      
      redcapAPI::importRecords(rcon = rcon, data = redcap_tables$spine_coordinates_df, returnContent = "count")
      
      incProgress(1/number_of_steps, detail = paste("Uploading Complete"))
      
      redcap_upload_complete_reactiveval("Complete")
      
      completion_text <- paste("Tables were successfully uploaded.") 
    }
    
    
    
    )
    sendSweetAlert(
      session = session,
      title = "Success !!",
      text = completion_text,
      type = "success"
    )
  }
  )
  
  
  observeEvent(input$flag_xray, ignoreInit = TRUE, {
    if(input$flag_xray){
      showModal(
        modalDialog(
          footer = "Redcap Upload", easyClose = TRUE, size = "l",
          box(width = 12, title = "Upload to Redcap", footer = NULL,
              fluidRow(
                textAreaInput(inputId = "flag_xray_reason", label = "Enter Concern:"),
                br(),
                actionBttn("confirm_upload_flag", "Confirmed, Upload to Redcap",
                           style = "simple", color = "primary")
              ),
              br()
          )
        )
      )
    }
  })
  
  observeEvent(input$confirm_upload_flag, ignoreInit = TRUE, {
  
    pid <- patient_id_reactive_val()
    
    withProgress(message = 'Uploading Data', value = 0, {
      number_of_steps <- 1
      incProgress(1/number_of_steps, detail = paste("Uploading User & Time"))
      
      user_time_flag_df <- tibble(record_id = pid, 
                                  user_last_name = input$last_name,
                                  date_time_upload = paste(Sys.time()),
                                  flag_xray = input$flag_xray,
                                  flag_xray_reason = input$flag_xray_reason,
                                  spine_dicom_coordinates_user_complete = "Complete")
      # flag_xray
      
      
      redcapAPI::importRecords(rcon = rcon, data = user_time_flag_df, 
                               returnContent = "count")
      
      incProgress(1/number_of_steps, detail = paste("Uploading Complete"))
      
      completion_text <- paste("Flag was successfully uploaded.") 
    }
    )
    sendSweetAlert(
      session = session,
      title = "Success!!",
      text = completion_text,
      type = "success"
    )
    
    }
  )
  
  observeEvent(xray_instructions_reactiveval(), ignoreInit = TRUE, {
    if(xray_instructions_reactiveval() == "Completed"){
      updateSwitchInput(session = session, 
                        inputId = "all_points_recorded", 
                        value = TRUE)
    }else{
      updateSwitchInput(session = session, 
                        inputId = "all_points_recorded", 
                        value = FALSE)
    }
  })
  
  
  
  # --- Cleanup on session end ---
  session$onSessionEnded(function() {
    if (dir.exists("dicoms")) {
      unlink("dicoms", recursive = TRUE, force = TRUE)
    }
  })
  
  
  ### REQUIRE user to enter last name
  # where we'll keep the user's last name for reuse
  user_state <- reactiveValues(last_name = NULL)
  
  # show the modal right after the first flush (when UI is ready)
  session$onFlushed(function() {
    showModal(
      modalDialog(
        title = "Welcome",
        easyClose = FALSE,  # force an explicit choice
        footer = tagList(
          modalButton("Cancel"),                            # optional
          actionButton("submit_last_name", "Continue", class = "btn-primary")
        ),
        # body
        textInput("last_name", "Please enter your last name", value = "", placeholder = "e.g., Smith"),
        uiOutput("last_name_error")                         # where we show validation text
      )
    )
  }, once = TRUE)
  
  # simple validation helper
  output$last_name_error <- renderUI({
    req(input$submit_last_name)  # only show after a submit attempt
    ln <- trimws(input$last_name %||% "")
    if (!nzchar(ln)) tags$div(style = "color:#c00; margin-top:4px;", "Last name is required.")
  })
  
  # handle submit
  observeEvent(input$submit_last_name, {
    ln <- trimws(input$last_name %||% "")
    req(nzchar(ln))     # blocks if empty and keeps modal open
    user_state$last_name <- ln
    removeModal()
    # (optional) do something with it immediately:
    # showNotification(paste("Hello,", ln), type = "message")
  })
  
  output$user_last_name <- renderText({
    paste("User:", input$last_name)
  })
  
  output$redcap_upload_completion <- renderText({
    redcap_completion <- redcap_upload_complete_reactiveval()
    
    if(redcap_completion == "Complete"){
      paste("REDCAP UPLOAD COMPLETE")
    }else{
      NULL
    }
    
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
