jh_calculate_distance_between_2_points_function <- function(point_1, point_2){
  sqrt((point_1[1] - point_2[1])^2 + 
         (point_1[2] - point_2[2])^2)
}


jh_calculate_3_point_angle_function <- function(ventral_point_coord = c(0,0), 
                                                vertex_point_coord = c(2, 2),
                                                posterior_point_coord = c(2, 0),
                                                spine_orientation = "left") {
  # Compute vectors
  v1 <- ventral_point_coord - vertex_point_coord
  v2 <- posterior_point_coord - vertex_point_coord
  
  # Compute dot product and magnitudes
  dot_product <- sum(v1 * v2)
  mag_v1 <- sqrt(sum(v1^2))
  mag_v2 <- sqrt(sum(v2^2))
  
  # Compute angle in radians
  angle_rad <- acos(dot_product / (mag_v1 * mag_v2))
  
  # Convert to degrees
  angle_deg <- angle_rad * (180 / pi)
  
  return(angle_deg)
}


jh_calculate_vertex_angle <- function(vertex_coord, 
                                      posterior_point_coord, 
                                      ventral_point_coord, 
                                      spine_orientation = "right") {
  # Calculate the vectors from vertex to posterior and vertex to ventral
  vec_posterior <- c(posterior_point_coord[1] - vertex_coord[1], 
                     posterior_point_coord[2] - vertex_coord[2])
  vec_ventral <- c(ventral_point_coord[1] - vertex_coord[1], 
                   ventral_point_coord[2] - vertex_coord[2])
  
  # Calculate the dot product and magnitudes of the vectors
  dot_product <- sum(vec_posterior * vec_ventral)
  mag_posterior <- sqrt(sum(vec_posterior^2))
  mag_ventral <- sqrt(sum(vec_ventral^2))
  
  # Calculate the angle in radians
  angle_radians <- acos(dot_product / (mag_posterior * mag_ventral))
  
  # Convert the angle to degrees
  angle_degrees <- angle_radians * (180 / pi)
  
  # Determine the sign of the angle based on spine orientation and x-coordinates
  if(posterior_point_coord[1] == vertex_coord[1]){
    slope_post_to_vertex <- 1
  }else{
    slope_post_to_vertex <- (posterior_point_coord[2] - vertex_coord[2]) / (posterior_point_coord[1] - vertex_coord[1])
  }
  if(ventral_point_coord[1] == vertex_coord[1]){
    slope_ventral_to_vertex <- 1
  }else{
    slope_ventral_to_vertex <- (ventral_point_coord[2] - ventral_point_coord[2]) / (ventral_point_coord[1] - vertex_coord[1])
  }
  
  
  if (spine_orientation == "right") {
    if(slope_ventral_to_vertex > slope_post_to_vertex){
      angle_degrees <- abs(angle_degrees)
    }else{
      angle_degrees <- -abs(angle_degrees)
    }
  } else if (spine_orientation == "left") {
    if(slope_ventral_to_vertex < slope_post_to_vertex){
      angle_degrees <- abs(angle_degrees)
    }else{
      angle_degrees <- -abs(angle_degrees)
    }
  }
  
  if(vertex_coord[2] > posterior_point_coord[2]){
    angle_degrees <- angle_degrees*-1
  }
  
  return(angle_degrees)
}
####


jh_calculate_vpa_from_xray_data_function <- function(fem_head_center = c(0,0), 
                                                     vertebral_centroid = c(0.15, 0.3), 
                                                     spine_facing = "left",
                                                     pelvic_tilt = 15){
  
  
  fem_head_to_centroid_length <- jh_calculate_distance_between_2_points_function(point_1 = vertebral_centroid, 
                                                                                 point_2 = fem_head_center)
  
  fem_head_to_centroid_x_length <- jh_calculate_distance_between_2_points_function(point_1 = vertebral_centroid, 
                                                                                   point_2 = c(fem_head_center[1],
                                                                                               vertebral_centroid[2]))
  
  tilt_orientation_modifier <- case_when(
    spine_facing == "left" & fem_head_center[[1]] > vertebral_centroid[[1]] ~ 1,
    spine_facing == "left" & fem_head_center[[1]] < vertebral_centroid[[1]] ~ -1,
    spine_facing == "right" & fem_head_center[[1]] > vertebral_centroid[[1]] ~ -1,
    spine_facing == "right" & fem_head_center[[1]] < vertebral_centroid[[1]] ~ 1
  )
  
  vertebral_tilt <- asin(fem_head_to_centroid_x_length/fem_head_to_centroid_length)*180/pi*tilt_orientation_modifier
  
  vpa <- pelvic_tilt + vertebral_tilt 
  
  return(vpa)
  
}

jh_calculate_segment_angle_between_vertebrae_from_coordinates_function <- function(sp_upper, sa_upper, sp_lower, sa_lower) {
  vec1 <- sa_upper - sp_upper  # Vector for upper vertebra
  vec2 <- sa_lower - sp_lower  # Vector for lower vertebra
  
  # Dot product of the two vectors
  dot_product <- sum(vec1 * vec2)
  
  # Magnitudes of the vectors
  mag_vec1 <- sqrt(sum(vec1^2))
  mag_vec2 <- sqrt(sum(vec2^2))
  
  # Calculate the angle using the dot product formula
  cos_theta <- dot_product / (mag_vec1 * mag_vec2)
  
  # Ensure the value is within the valid range for acos (to avoid numerical errors)
  cos_theta <- min(1, max(-1, cos_theta))
  
  # Compute the angle in radians
  theta_radians <- acos(cos_theta)
  
  # Calculate the cross product to determine if it's lordotic or kyphotic
  cross_product <- (vec1[1] * vec2[2]) - (vec1[2] * vec2[1])
  
  # If the cross product is negative, it is kyphotic (angle opens posteriorly), otherwise lordotic
  
  if(sp_upper[1] < sa_upper[1]){
    sign <- ifelse(cross_product < 0, 1, -1)
  }else{
    sign <- ifelse(cross_product < 0, -1, 1)
  }
  
  # Convert the angle to degrees and apply the sign for lordosis/kyphosis
  theta_degrees <- theta_radians * (180 / pi) * sign
  
  return(theta_degrees)
}


jh_calculate_perpendicular_angle_function <- function(coord1_x, coord1_y, coord2_x, coord2_y) {
  # Step 1: Calculate the slope of the line connecting the two points
  
  coord1_x <- if_else(is.na(coord1_x), 0, coord1_x)
  coord1_y <- if_else(is.na(coord1_y), 0, coord1_y)
  coord2_x <- if_else(is.na(coord2_x), 0, coord2_x)
  coord2_y <- if_else(is.na(coord2_y), 0, coord2_y)
  
  slope <- (coord2_y - coord1_y) / (coord2_x - coord1_x)
  
  # Step 2: Get the negative reciprocal for the perpendicular slope
  if (slope != 0) {
    perpendicular_slope <- -1 / slope
  } else {
    # If the original slope is zero (horizontal line), perpendicular line is vertical
    perpendicular_slope <- Inf
  }
  
  # Step 3: Calculate the angle in radians
  # If the slope is infinite (vertical line), the angle is 90 degrees
  if (is.infinite(perpendicular_slope)) {
    angle_radians <- pi / 2
  } else {
    angle_radians <- atan(perpendicular_slope)
  }
  
  # Convert the angle to degrees
  angle_degrees <- angle_radians * 180 / pi
  
  return(angle_degrees)
}


jh_get_point_along_line_function <- function(coord_a, 
                                             coord_b, 
                                             percent_a_to_b){
  new_point <- coord_a + (coord_b - coord_a) * percent_a_to_b
  return(new_point)
}

jh_calculate_point_along_line_function <- function(coord_a, 
                                                   coord_b, 
                                                   percent_a_to_b){
  new_point <- coord_a + (coord_b - coord_a) * percent_a_to_b
  return(new_point)
}


jh_calculate_pelvic_incidence_line_coordinates <- function(fem_head_center = c(0,0), 
                                                           s1_anterior, 
                                                           s1_posterior, 
                                                           spine_facing = "left",
                                                           pelvic_tilt = 10, 
                                                           pelvic_incidence_value = 50) {
  
  # Step 1: Calculate the center (midpoint)
  center_x <- (s1_anterior[1] + s1_posterior[1]) / 2
  center_y <- (s1_anterior[2] + s1_posterior[2]) / 2
  center <- c(center_x, center_y)
  
  # Step 2: Calculate the length of the line between s1_anterior and s1_posterior
  line_length <- sqrt((s1_anterior[1] - s1_posterior[1])^2 + (s1_anterior[2] - s1_posterior[2])^2)
  
  # Step 4: Calculate the length of the perpendicular line (5 times the original length)
  extended_length <- 3 * line_length
  
  if (s1_anterior[1] == s1_posterior[1]) {
    # For vertical lines, the perpendicular is horizontal
    dx <- extended_length
    dy <- 0
  } else if (s1_anterior[2] == s1_posterior[2]) {
    # For horizontal lines, the perpendicular is vertical
    dx <- 0
    dy <- extended_length
  } else {
    pt_pi_diff <- pelvic_incidence_value - pelvic_tilt
    
    pt_pi_diff_rad <- pt_pi_diff*(pi/180)
    
    orientation_modifier <- if_else(spine_facing == "left", -1, 1)
    
    dx <- sin(pt_pi_diff_rad)*extended_length*orientation_modifier
    dy <- cos(pt_pi_diff_rad)*extended_length
  }
  
  # Inferior point is displaced from the center by (dx, dy)
  inferior_x <- center[1] - dx
  inferior_y <- center[2] - dy
  inferior <- c(inferior_x, inferior_y)
  
  pi_line_coordinates_df <- tibble(spine_point = c("fem_head_center", "s1_center", "s1_inferior"), 
                                   x = c(fem_head_center[1], 
                                         center[1],
                                         inferior_x),
                                   y = c(fem_head_center[2],
                                         center[2],
                                         inferior_y)
  )
  # Return the center and inferior points as a list
  # return(list(center = center, inferior = inferior))
  pi_line_coordinates_df
}

jh_calculate_vertebral_slope_function <-  function(vert_coord,
                                                   spine_orientation = "left") {
  # Compute vectors
  v1 <- vert_coord$sa - vert_coord$sp
  v2 <- c(mean(c(vert_coord$sp[[1]], vert_coord$sa[[1]])), vert_coord$sp[[2]]) - vert_coord$sp
  
  # Compute dot product and magnitudes
  dot_product <- sum(v1 * v2)
  mag_v1 <- sqrt(sum(v1^2))
  mag_v2 <- sqrt(sum(v2^2))
  
  # Compute angle in radians
  angle_rad <- acos(dot_product / (mag_v1 * mag_v2))
  
  # Convert to degrees
  angle_deg <- angle_rad * (180 / pi)
  
  if(vert_coord$sp[[2]] < vert_coord$sa[[2]]){
    angle_deg <- angle_deg*-1
  }
  
  return(round(angle_deg, 1))
}

jh_calculate_vertebral_tilt_function <- function(vertebral_centroid, 
                                                 femoral_head_center = c(0,0), 
                                                 spine_orientation = "left", 
                                                 pelvic_tilt_modifier = FALSE){
  
  if(pelvic_tilt_modifier == FALSE){
    if(spine_orientation == "left"){
      if(femoral_head_center[[1]] < vertebral_centroid[[1]]){
        orientation_modifier <- -1
      }else{
        orientation_modifier <- 1
      }
    }else{
      if(femoral_head_center[[1]] < vertebral_centroid[[1]]){
        orientation_modifier <- 1
      }else{
        orientation_modifier <- -1
      }
    } 
  }else{
    orientation_modifier <- 1
  }
  
  plumb_line_point <- c(femoral_head_center[[1]], femoral_head_center[[2]]+100)
  
  # Compute vectors
  v1 <- c(plumb_line_point[[1]] - femoral_head_center[[1]], plumb_line_point[[2]] - femoral_head_center[[2]])
  v2 <- c(vertebral_centroid[[1]] - femoral_head_center[[1]], vertebral_centroid[[2]] - femoral_head_center[[2]])
  # Compute dot product
  dot_product <- sum(v1 * v2)
  # Compute magnitudes
  norm_v1 <- sqrt(sum(v1^2))
  norm_v2 <- sqrt(sum(v2^2))
  # Compute angle in radians
  angle_rad <- acos(dot_product / (norm_v1 * norm_v2))
  # Convert to degrees
  angle_deg <- round(angle_rad * (180 / pi), 1)*orientation_modifier
  
  return(angle_deg)
  
}

jh_calculate_vertebral_tilt_and_vpas_from_coordinates_function <- function(full_centroid_coord_list, 
                                                                           spine_orientation){
  
  pt_computed <- jh_calculate_vertebral_tilt_function(vertebral_centroid = full_centroid_coord_list$s1, 
                                                      femoral_head_center = full_centroid_coord_list$femoral_head, 
                                                      pelvic_tilt_modifier = TRUE,
                                                      spine_orientation = spine_orientation)
  
  centroid_list_for_vpa_calc <- full_centroid_coord_list
  centroid_list_for_vpa_calc$femoral_head <- NULL
  centroid_list_for_vpa_calc$s1 <- NULL
  
  vert_tilt_list <- map(.x = centroid_list_for_vpa_calc, .f = ~ jh_calculate_vertebral_tilt_function(vertebral_centroid = .x, 
                                                                                                     femoral_head_center = full_centroid_coord_list$femoral_head, 
                                                                                                     spine_orientation = spine_orientation,
                                                                                                     pelvic_tilt_modifier = FALSE))
  vpa_list <- map(.x = vert_tilt_list, .f = ~ pt_computed + .x)
  
  vert_tilt_list <- c(list(pt = pt_computed), vert_tilt_list)
  
  return(list(vert_tilt_list = vert_tilt_list, 
              vpa_list = vpa_list,
              pt_computed = pt_computed))
}


# compute_perpendicular_points <- function(x1, y1, x2, y2, distance = 0.01) {
#   # Midpoint
#   midpoint_x <- (x1 + x2) / 2
#   midpoint_y <- (y1 + y2) / 2
#   
#   # Slope of the line (tangent)
#   slope <- (y2 - y1) / (x2 - x1)
#   
#   # Perpendicular slope (-1 / slope)
#   perpendicular_slope <- -1 / slope
#   
#   # Calculate the change in x and y for perpendicular points
#   delta_x <- distance / sqrt(1 + perpendicular_slope^2)
#   delta_y <- perpendicular_slope * delta_x
#   
#   # Two perpendicular points
#   point_1 <- c(midpoint_x + delta_x, midpoint_y + delta_y)
#   point_2 <- c(midpoint_x - delta_x, midpoint_y - delta_y)
#   
#   return(tibble(x1 = point_1[1], y1 = point_1[2], x2 = point_2[1], y2 = point_2[2]))
# }   