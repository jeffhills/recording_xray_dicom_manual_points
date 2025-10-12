
rcon <<- redcapConnection(url = 'https://redcap.uthscsa.edu/REDCap/api/', token = "6DDC3E7AB46DC526ECF3F1A366A4A7DD", config =  httr::config(ssl_verifypeer = FALSE))  

spinal_segments_labels_vector <- c('L5-S1', 'L4-L5', 'L3-L4', 'L2-L3', 'L1-L2', 
                                   'T12-L1', 'T11-T12', 'T10-T11', 'T9-T10', 'T8-T9', 'T7-T8', 'T6-T7', 'T5-T6', 'T4-T5', 'T3-T4', 'T2-T3', 'T1-T2',
                                   'C7-T1', 'C6-C7', 'C5-C6', 'C4-C5', 'C3-C4', 'C2-C3', 'C1-C2')

spine_level_interspace_df <- tibble(level = c('L5-S1', 'L5', 'L4-L5', 'L4', 'L3-L4', 'L3', 'L2-L3', 'L2', 'L1-L2', 'L1', 'T12-L1', 'T12', 'T11-T12', 'T11', 'T10-T11', 'T10', 'T9-T10', 'T9', 'T8-T9', 'T8', 'T7-T8', 'T7', 'T6-T7', 'T6', 'T5-T6', 'T5', 'T4-T5', 'T4', 'T3-T4', 'T3', 'T2-T3', 'T2', 'T1-T2', 'T1', 'C7-T1', 'C7', 'C6-C7', 'C6', 'C5-C6', 'C5', 'C4-C5', 'C4', 'C3-C4', 'C3', 'C2-C3', 'C2', 'C1-C2')) %>%
  mutate(level_count = seq(from = 0.5, by = 0.5, length = nrow(.)))

jh_spine_levels_factors_df <- tibble(level = c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "l1", "l2", "l3", "l4", "l5")) %>%
  mutate(level_label = str_to_upper(level)) %>% 
  mutate(level_tilt = paste0(level, "_tilt")) %>%
  mutate(level_pelvic_angle = paste0(level, "_pelvic_angle")) %>%
  mutate(level_cobb_angle = paste0(level, "_s1")) %>%
  mutate(level_tilt_label = paste0(level_label, " Tilt")) %>%
  mutate(level_pelvic_angle_label = paste0(level_label, " Pelvic Angle")) %>%
  mutate(level_cobb_label = paste0(level_label, "-S1")) %>%
  select(level, level_tilt, level_pelvic_angle, level_cobb_angle, level_label, level_tilt_label, level_pelvic_angle_label, level_cobb_label) %>%
  mutate(across(everything(), fct_inorder)) %>%
  mutate(across(everything(), fct_rev)) %>%
  mutate(interspace = paste0(level_label, "-", lead(level_label))) %>%
  mutate(interspace = if_else(interspace == "L5-NA", "L5-S1", interspace)) %>%
  mutate(interspace = fct_inorder(interspace))


spine_points_vector <- c(
  # 'fem_head_center' = 'Femoral Head Center',
                         'asis_1' = 'ASIS 1',
                         'asis_2' = 'ASIS 2',
                         'pubic_symphysis' = 'Pubic Symphysis',
                         'fem_head_center' = 'Femoral Head Center',
                         'anterior_acetabulum' = 'Anterior Acetabulum',
                         'posterior_acetabulum' = 'Posterior Acetabulum',
                         'sacrum_inferior_anterior' = 'Sacrum Anterior S3',
                         'sacrum_inferior_posterior' = 'Sacrum Posterior S3',
                         's1_superior_posterior' = 'S1 Superior Posterior Corner',
                         's1_superior_anterior' = 'S1 Superior Anterior Corner',
                         'l5_inferior_anterior' = 'L5 Inferior Anterior Corner',
                         'l5_inferior_posterior' = 'L5 Inferior Posterior Corner',
                         'l5_superior_posterior' = 'L5 Superior Posterior Corner',
                         'l5_superior_anterior' = 'L5 Superior Anterior Corner',
                         'l4_inferior_anterior' = 'L4 Inferior Anterior Corner',
                         'l4_inferior_posterior' = 'L4 Inferior Posterior Corner',
                         'l4_superior_posterior' = 'L4 Superior Posterior Corner',
                         'l4_superior_anterior' = 'L4 Superior Anterior Corner',
                         'l3_inferior_anterior' = 'L3 Inferior Anterior Corner',
                         'l3_inferior_posterior' = 'L3 Inferior Posterior Corner',
                         'l3_superior_posterior' = 'L3 Superior Posterior Corner',
                         'l3_superior_anterior' = 'L3 Superior Anterior Corner',
                         'l2_inferior_anterior' = 'L2 Inferior Anterior Corner',
                         'l2_inferior_posterior' = 'L2 Inferior Posterior Corner',
                         'l2_superior_posterior' = 'L2 Superior Posterior Corner',
                         'l2_superior_anterior' = 'L2 Superior Anterior Corner',
                         'l1_inferior_anterior' = 'L1 Inferior Anterior Corner',
                         'l1_inferior_posterior' = 'L1 Inferior Posterior Corner',
                         'l1_superior_posterior' = 'L1 Superior Posterior Corner',
                         'l1_superior_anterior' = 'L1 Superior Anterior Corner',
                         # 't12_inferior_anterior' = 'T12 Inferior Anterior Corner',
                         # 't12_inferior_posterior' = 'T12 Inferior Posterior Corner',
                         't12_superior_posterior' = 'T12 Superior Posterior Corner',
                         't12_superior_anterior' = 'T12 Superior Anterior Corner',
                         # 't11_inferior_anterior' = 'T11 Inferior Anterior Corner',
                         # 't11_inferior_posterior' = 'T11 Inferior Posterior Corner',
                         't11_superior_posterior' = 'T11 Superior Posterior Corner',
                         't11_superior_anterior' = 'T11 Superior Anterior Corner',
                         # 't10_inferior_anterior' = 'T10 Inferior Anterior Corner',
                         # 't10_inferior_posterior' = 'T10 Inferior Posterior Corner',
                         't10_superior_posterior' = 'T10 Superior Posterior Corner',
                         't10_superior_anterior' = 'T10 Superior Anterior Corner',
                         # 't9_inferior_anterior' = 'T9 Inferior Anterior Corner',
                         # 't9_inferior_posterior' = 'T9 Inferior Posterior Corner',
                         't9_superior_posterior' = 'T9 Superior Posterior Corner',
                         't9_superior_anterior' = 'T9 Superior Anterior Corner',
                         # 't8_inferior_anterior' = 'T8 Inferior Anterior Corner',
                         # 't8_inferior_posterior' = 'T8 Inferior Posterior Corner',
                         't8_superior_posterior' = 'T8 Superior Posterior Corner',
                         't8_superior_anterior' = 'T8 Superior Anterior Corner',
                         # 't7_inferior_anterior' = 'T7 Inferior Anterior Corner',
                         # 't7_inferior_posterior' = 'T7 Inferior Posterior Corner',
                         't7_superior_posterior' = 'T7 Superior Posterior Corner',
                         't7_superior_anterior' = 'T7 Superior Anterior Corner',
                         # 't6_inferior_anterior' = 'T6 Inferior Anterior Corner',
                         # 't6_inferior_posterior' = 'T6 Inferior Posterior Corner',
                         't6_superior_posterior' = 'T6 Superior Posterior Corner',
                         't6_superior_anterior' = 'T6 Superior Anterior Corner',
                         # 't5_inferior_anterior' = 'T5 Inferior Anterior Corner',
                         # 't5_inferior_posterior' = 'T5 Inferior Posterior Corner',
                         't5_superior_posterior' = 'T5 Superior Posterior Corner',
                         't5_superior_anterior' = 'T5 Superior Anterior Corner',
                         # 't4_inferior_anterior' = 'T4 Inferior Anterior Corner',
                         # 't4_inferior_posterior' = 'T4 Inferior Posterior Corner',
                         't4_superior_posterior' = 'T4 Superior Posterior Corner',
                         't4_superior_anterior' = 'T4 Superior Anterior Corner',
                         # 't3_inferior_anterior' = 'T3 Inferior Anterior Corner',
                         # 't3_inferior_posterior' = 'T3 Inferior Posterior Corner',
                         't3_superior_posterior' = 'T3 Superior Posterior Corner',
                         't3_superior_anterior' = 'T3 Superior Anterior Corner',
                         # 't2_inferior_anterior' = 'T2 Inferior Anterior Corner',
                         # 't2_inferior_posterior' = 'T2 Inferior Posterior Corner',
                         't2_superior_posterior' = 'T2 Superior Posterior Corner',
                         't2_superior_anterior' = 'T2 Superior Anterior Corner',
                         # 't1_inferior_anterior' = 'T1 Inferior Anterior Corner',
                         # 't1_inferior_posterior' = 'T1 Inferior Posterior Corner',
                         't1_superior_posterior' = 'T1 Superior Posterior Corner',
                         't1_superior_anterior' = 'T1 Superior Anterior Corner',
                         # 'c7_inferior_anterior' = 'C7 Inferior Anterior Corner',
                         # 'c7_inferior_posterior' = 'C7 Inferior Posterior Corner',
                         'c7_superior_posterior' = 'C7 Superior Posterior Corner',
                         'c7_superior_anterior' = 'C7 Superior Anterior Corner',
                         # 'c6_inferior_anterior' = 'C6 Inferior Anterior Corner',
                         # 'c6_inferior_posterior' = 'C6 Inferior Posterior Corner',
                         'c6_superior_posterior' = 'C6 Superior Posterior Corner',
                         'c6_superior_anterior' = 'C6 Superior Anterior Corner',
                         # 'c5_inferior_anterior' = 'C5 Inferior Anterior Corner',
                         # 'c5_inferior_posterior' = 'C5 Inferior Posterior Corner',
                         'c5_superior_posterior' = 'C5 Superior Posterior Corner',
                         'c5_superior_anterior' = 'C5 Superior Anterior Corner',
                         # 'c4_inferior_anterior' = 'C4 Inferior Anterior Corner',
                         # 'c4_inferior_posterior' = 'C4 Inferior Posterior Corner',
                         'c4_superior_posterior' = 'C4 Superior Posterior Corner',
                         'c4_superior_anterior' = 'C4 Superior Anterior Corner',
                         # 'c3_inferior_anterior' = 'C3 Inferior Anterior Corner',
                         # 'c3_inferior_posterior' = 'C3 Inferior Posterior Corner',
                         'c3_superior_posterior' = 'C3 Superior Posterior Corner',
                         'c3_superior_anterior' = 'C3 Superior Anterior Corner',
                         'c2_inferior_anterior' = 'C2 Inferior Anterior Corner',
                         'c2_inferior_posterior' = 'C2 Inferior Posterior Corner',
                         'c2_superior_posterior' = 'C2 Superior Posterior Corner',
                         'c2_superior_anterior' = 'C2 Superior Anterior Corner',
                         'c1_superior_anterior' = 'C1 Anterior Arch',
                         'c1_superior_posterior' = 'C1 Posterior Arch',
                         'orbital_center' = 'Orbital Center'
)

identify_level_by_spine_point_function <- function(spine_point){
  spine_point <- str_to_lower(spine_point)
  level <- case_when(
    spine_point %in% c('fem_head_center', 'asis_1', 'asis_2', 'pubic_symphysis', 'anterior_acetabulum', 'posterior_acetabulum') ~ "pelvis",
    str_starts(spine_point, "s1|sacrum") ~ "sacrum",
    # str_starts(spine_point, "s1") ~ "s1", 
    str_starts(spine_point, "l5") ~ "l5", 
    str_starts(spine_point, "l4") ~ "l4", 
    str_starts(spine_point, "l3") ~ "l3", 
    str_starts(spine_point, "l2") ~ "l2", 
    str_starts(spine_point, "l1") ~ "l1", 
    str_starts(spine_point, "t12") ~ "t12",
    str_starts(spine_point, "t11") ~ "t11",
    str_starts(spine_point, "t10") ~ "t10",
    str_starts(spine_point, "t9") ~ "t9",
    str_starts(spine_point, "t8") ~ "t8",
    str_starts(spine_point, "t7") ~ "t7",
    str_starts(spine_point, "t6") ~ "t6",
    str_starts(spine_point, "t5") ~ "t5",
    str_starts(spine_point, "t4") ~ "t4",
    str_starts(spine_point, "t3") ~ "t3",
    str_starts(spine_point, "t2") ~ "t2",
    str_starts(spine_point, "t1") ~ "t1",
    str_starts(spine_point, "c7") ~ "c7",
    str_starts(spine_point, "c6") ~ "c6",
    str_starts(spine_point, "c5") ~ "c5",
    str_starts(spine_point, "c4") ~ "c4",
    str_starts(spine_point, "c3") ~ "c3",
    str_starts(spine_point, "c2") ~ "c2",
    str_starts(spine_point, "c1") ~ "c1",
    str_starts(spine_point, "orbit") ~ "skull"
  )
  level
}

levels_need_inferior_coord_vector <- tibble(spine_point = names(spine_points_vector)) %>%
  mutate(level = map(.x = spine_point, .f = ~ identify_level_by_spine_point_function(spine_point = .x))) %>%
  unnest() %>%
  mutate(vertebral_corner = case_when(
    level == "pelvis" ~ "na", 
    level == "skull" ~ "na", 
    level != "pelvis" & str_detect(spine_point, "inferior_anterior") ~ "inferior_anterior",
    level != "pelvis" & str_detect(spine_point, "inferior_posterior") ~ "inferior_posterior",
    level != "pelvis" & str_detect(spine_point, "superior_anterior") ~ "superior_anterior",
    level != "pelvis" & str_detect(spine_point, "superior_posterior") ~ "superior_posterior"
  )) %>%
  group_by(level) %>%
  add_tally() %>%
  filter(n == 2) %>%
  select(level) %>%
  distinct() %>%
  pull(level)

spine_point_ids <- names(spine_points_vector)



################### 
click_instructions_df <- tibble(spine_point = c(
  's1_center',
  'fem_head_center',
  'asis_1',
  'asis_2',
  'pubic_symphysis',
  'anterior_acetabulum',
  'posterior_acetabulum',
  'sacrum_inferior_anterior',
  'sacrum_inferior_posterior',
  's1_superior_anterior',
  's1_superior_posterior',
  'l5_inferior_anterior',
  'l5_superior_anterior',
  'l4_inferior_anterior',
  'l4_superior_anterior',
  'l3_inferior_anterior',
  'l3_superior_anterior',
  'l2_inferior_anterior',
  'l2_superior_anterior',
  'l1_inferior_anterior',
  'l1_superior_anterior',
  't12_inferior_anterior',
  't12_superior_anterior',
  't11_inferior_anterior',
  't11_superior_anterior',
  't10_inferior_anterior',
  't10_superior_anterior',
  't9_inferior_anterior',
  't9_superior_anterior',
  't8_inferior_anterior',
  't8_superior_anterior',
  't7_inferior_anterior',
  't7_superior_anterior',
  't6_inferior_anterior',
  't6_superior_anterior',
  't5_inferior_anterior',
  't5_superior_anterior',
  't4_inferior_anterior',
  't4_superior_anterior',
  't3_inferior_anterior',
  't3_superior_anterior',
  't2_inferior_anterior',
  't2_superior_anterior',
  't1_inferior_anterior',
  't1_superior_anterior',
  'c7_inferior_anterior',
  'c7_superior_anterior',
  'c6_inferior_anterior',
  'c6_superior_anterior',
  'c5_inferior_anterior',
  'c5_superior_anterior',
  'c4_inferior_anterior',
  'c4_superior_anterior',
  'c3_inferior_anterior',
  'c3_superior_anterior',
  'c2_inferior_anterior',
  'c2_superior_anterior',
  'l5_inferior_posterior',
  'l5_superior_posterior',
  'l4_inferior_posterior',
  'l4_superior_posterior',
  'l3_inferior_posterior',
  'l3_superior_posterior',
  'l2_inferior_posterior',
  'l2_superior_posterior',
  'l1_inferior_posterior',
  'l1_superior_posterior',
  't12_inferior_posterior',
  't12_superior_posterior',
  't11_inferior_posterior',
  't11_superior_posterior',
  't10_inferior_posterior',
  't10_superior_posterior',
  't9_inferior_posterior',
  't9_superior_posterior',
  't8_inferior_posterior',
  't8_superior_posterior',
  't7_inferior_posterior',
  't7_superior_posterior',
  't6_inferior_posterior',
  't6_superior_posterior',
  't5_inferior_posterior',
  't5_superior_posterior',
  't4_inferior_posterior',
  't4_superior_posterior',
  't3_inferior_posterior',
  't3_superior_posterior',
  't2_inferior_posterior',
  't2_superior_posterior',
  't1_inferior_posterior',
  't1_superior_posterior',
  'c7_inferior_posterior',
  'c7_superior_posterior',
  'c6_inferior_posterior',
  'c6_superior_posterior',
  'c5_inferior_posterior',
  'c5_superior_posterior',
  'c4_inferior_posterior',
  'c4_superior_posterior',
  'c3_inferior_posterior',
  'c3_superior_posterior',
  'c2_inferior_posterior',
  'c2_superior_posterior'),
  x = c(
    172.887021997022,
    175.935798642654,
    243.483872858645,
    253.135567460198,
    225.490140996232,
    195.185450163079,
    152.52461760969,
    96.0810545390519,
    86.2362470267313,
    185.287245472375,
    160.486798521669,
    192.329347692946,
    202.739411845094,
    204.576481989591,
    203.04559020251,
    199.021536820983,
    190.152184828874,
    184.508051742986,
    169.994566664988,
    165.425506547841,
    152.873112784019,
    148.115210340791,
    137.647824965689,
    135.110276995967,
    127.425830078125,
    125.225830078125,
    117.825830078125,
    116.425830078125,
    112.025830078125,
    110.625830078125,
    107.445782470703,
    106.245782470703,
    105.045782470703,
    104.845782470703,
    106.271350097656,
    106.271350097656,
    107.471350097656,
    107.671350097656,
    112.441851806641,
    113.041851806641,
    118.068176269531,
    118.868176269531,
    124.562491764772,
    124.98735467904,
    127.324100707514,
    127.961395078916,
    130.935435478792,
    131.36029839306,
    134.759201707204,
    135.821358992874,
    139.857556678421,
    140.919713964091,
    144.28759765625,
    145.48759765625,
    149.08759765625,
    149.68759765625,
    151.892700195312,
    164.817705821385,
    174.006584644695,
    174.817368070281,
    173.466062360971,
    171.574234367936,
    161.034049835315,
    158.331438416695,
    144.098194989983,
    140.038762896981,
    125.723025359214,
    123.723342871682,
    108.29722082501,
    106.583207264269,
    99.6216017002569,
    97.9188781768013,
    89.7458052642146,
    88.7241711501412,
    86.3403582173034,
    85.3187241032301,
    80.8689880371094,
    80.4689880371094,
    81.2689880371094,
    81.2689880371094,
    82.4497375488281,
    82.8497375488281,
    87.0337768554687,
    87.6337768554688,
    94.4337768554688,
    95.4337768554688,
    102.788824462891,
    104.188824462891,
    108.788824462891,
    109.388824462891,
    113.588824462891,
    114.788824462891,
    118.588824462891,
    119.412042236328,
    122.212042236328,
    123.612042236328,
    127.965771484375,
    129.565771484375,
    133.365771484375,
    134.565771484375,
    138.365771484375,
    138.965771484375,
    146.236399825607
  ),
  y = c(
    700.059184029214,
    593.962506892944,
    666.515677392581,
    664.217654868402,
    568.722129935962,
    609.996501462668,
    581.774719927349,
    648.063090510307,
    660.533180025914,
    691.63927920027,
    708.479088858157,
    700.518451565338,
    729.911573877285,
    744.608135033259,
    774.307435702622,
    786.945072379747,
    815.703274293557,
    825.916467496592,
    851.180682261995,
    858.706193043179,
    882.103965468265,
    888.765028888785,
    913.188928097355,
    918.264024036798,
    937.572973632812,
    943.372973632813,
    963.172973632812,
    966.972973632812,
    984.972973632812,
    989.372973632813,
    1007.40756835937,
    1011.40756835937,
    1029.00756835938,
    1032.40756835938,
    1051.89084472656,
    1054.49084472656,
    1073.09084472656,
    1075.89084472656,
    1094.27424316406,
    1098.27424316406,
    1112.97440185547,
    1115.97440185547,
    1131.97122451129,
    1135.1576963683,
    1151.09005565335,
    1154.27652751036,
    1167.23484639553,
    1170.20888679541,
    1182.74234276631,
    1185.07908879479,
    1197.40011330856,
    1199.94929079417,
    1210.94354858398,
    1214.34354858398,
    1227.14354858398,
    1229.94354858398,
    1267.97075195313,
    715.504483712124,
    740.098247621573,
    746.043992742539,
    774.421412638057,
    780.637418900885,
    806.041966235921,
    810.636405647576,
    838.03253016775,
    843.831718872039,
    871.291235666625,
    874.719262788107,
    900.715135126018,
    905.571506881452,
    929.007633321848,
    933.094169778142,
    954.207941468991,
    958.975567334666,
    978.38661550206,
    983.835330777118,
    1004.21767578125,
    1007.81767578125,
    1028.61767578125,
    1032.41767578125,
    1053.01362304687,
    1057.41362304688,
    1075.00340576172,
    1079.00340576172,
    1099.00340576172,
    1102.80340576172,
    1119.73178710938,
    1122.93178710937,
    1136.53178710938,
    1139.93178710937,
    1154.93178710937,
    1158.33178710938,
    1172.53178710938,
    1176.00803222656,
    1187.00803222656,
    1190.80803222656,
    1203.53012084961,
    1206.73012084961,
    1217.53012084961,
    1221.13012084961,
    1234.53012084961,
    1237.93012084961,
    1269.83001179223
  )
)


spine_coord_instructions_right_df <- click_instructions_df %>%
  mutate(level = case_when(
    spine_point %in% c('fem_head_center', 'asis_1', 'asis_2', 'pubic_symphysis', 'anterior_acetabulum', 'posterior_acetabulum') ~ "pelvis",
    str_starts(spine_point, "s1|sacrum") ~ "sacrum",
    # str_starts(spine_point, "s1") ~ "s1", 
    str_starts(spine_point, "l5") ~ "l5", 
    str_starts(spine_point, "l4") ~ "l4", 
    str_starts(spine_point, "l3") ~ "l3", 
    str_starts(spine_point, "l2") ~ "l2", 
    str_starts(spine_point, "l1") ~ "l1", 
    str_starts(spine_point, "t12") ~ "t12",
    str_starts(spine_point, "t11") ~ "t11",
    str_starts(spine_point, "t10") ~ "t10",
    str_starts(spine_point, "t9") ~ "t9",
    str_starts(spine_point, "t8") ~ "t8",
    str_starts(spine_point, "t7") ~ "t7",
    str_starts(spine_point, "t6") ~ "t6",
    str_starts(spine_point, "t5") ~ "t5",
    str_starts(spine_point, "t4") ~ "t4",
    str_starts(spine_point, "t3") ~ "t3",
    str_starts(spine_point, "t2") ~ "t2",
    str_starts(spine_point, "t1") ~ "t1",
    str_starts(spine_point, "c7") ~ "c7",
    str_starts(spine_point, "c6") ~ "c6",
    str_starts(spine_point, "c5") ~ "c5",
    str_starts(spine_point, "c4") ~ "c4",
    str_starts(spine_point, "c3") ~ "c3",
    str_starts(spine_point, "c2") ~ "c2",
    str_starts(spine_point, "c1") ~ "c1",
    str_starts(spine_point, "orbit") ~ "skull"
  )) %>%
  mutate(vertebral_corner = case_when(
    level == "pelvis" ~ "na", 
    level == "skull" ~ "na", 
    level != "pelvis" & str_detect(spine_point, "inferior_anterior") ~ "inferior_anterior",
    level != "pelvis" & str_detect(spine_point, "inferior_posterior") ~ "inferior_posterior",
    level != "pelvis" & str_detect(spine_point, "superior_anterior") ~ "superior_anterior",
    level != "pelvis" & str_detect(spine_point, "superior_posterior") ~ "superior_posterior"
  )) %>%
  filter(spine_point != "s1_center") %>%
  select(level, vertebral_corner, spine_point, x, y) %>%
  mutate(level = fct_inorder(level)) %>%
  arrange(level)   %>%
  mutate(vertebral_corner = fct(vertebral_corner, levels = c("na", "inferior_anterior", "inferior_posterior", "superior_posterior", "superior_anterior"))) %>%
  arrange(level, vertebral_corner)

spine_coord_instructions_left_df <- spine_coord_instructions_right_df %>%
  mutate(x = x*-1)

# skull:
skull_left <- magick::image_read(path = "images/skull.png")
# Read original, precompute rasters once
skull_right <- magick::image_flop(skull_left)             # horizontally mirrored

info_skull_left <- magick::image_info(skull_left)
width_skull <- info_skull_left$width
height_skull <- info_skull_left$height

skull_raster_left  <- as.raster(skull_left)
skull_raster_right <- as.raster(skull_right)

skull_c1_instructions_left_df <- tribble(
  ~id,                       ~x,    ~y,     ~label,
  "c1_superior_anterior",         340, 305,   "C1 Anterior Arch",
  "c1_superior_posterior",        445,  280,   "C1 Posterior Arch",
  "orbital_center",        95,  410,      "Orbital Center"
)

skull_c1_instructions_right_df <- skull_c1_instructions_left_df %>%
  mutate(x = width_skull - x)

make_vertebra_polys <- function(corners_tbl, buffer_radius = 1, nQuadSegs = 8) {
  # 1) enforce a consistent corner order around the body (clockwise)
  # corner_order <- c("superior_anterior", "superior_posterior",
  #                   "inferior_posterior", "inferior_anterior")
  
  polys <-  corners_tbl %>%
    # mutate(vertebral_corner = factor(vertebral_corner, levels = corner_order)) %>%
    arrange(level, vertebral_corner) %>%
    group_by(level) %>%
    summarise(
      geom = list({
        coords <- cbind(as.numeric(x), as.numeric(y))
        coords <- rbind(coords, coords[1, , drop = FALSE])
        st_polygon(list(coords))
      }),
      .groups = "drop"
    )%>%
    mutate(geometry = st_sfc(geom)) %>%  # convert list -> sfc
    st_as_sf() %>%
    select(-geom)
  
  polys %>%
    st_make_valid() %>%
    st_buffer(buffer_radius, nQuadSegs = nQuadSegs, joinStyle = "ROUND") %>%
    st_buffer(-buffer_radius, nQuadSegs = nQuadSegs, joinStyle = "ROUND") %>%
    st_make_valid()
}



polys_left_sf <- make_vertebra_polys(spine_coord_instructions_left_df %>%
                                       filter(level != "pelvis"), buffer_radius = 150)

polys_right_sf <- make_vertebra_polys(spine_coord_instructions_right_df %>%
                                        filter(level != "pelvis"), buffer_radius = 150)



### THEME FUNCTION
jh_theme_function <- function(theme_template = theme_minimal(), 
                              axis_text_size = 8,
                              axis_title_size = 10,
                              axis_ticks_length = 1,
                              plot_title_size = 12,
                              axis_line_size = 0.5, 
                              top_margin = 5,
                              bottom_margin = 5, 
                              left_margin = 5, 
                              right_margin = 5){
  my_theme <- theme_template +
    theme(strip.text = element_text(face = "bold", size = 10, hjust = 0.5), 
          strip.placement = "outside",
          axis.text = element_text(size = axis_text_size),
          axis.title = element_text(face = "bold", size = axis_title_size, hjust = 0.5),
          axis.line = element_line(colour = "black", size = axis_line_size),
          axis.ticks = element_line(rel(axis_ticks_length)),
          plot.subtitle = element_text(size = axis_title_size, hjust = 0.5),
          plot.caption = element_text(size = 8, hjust = 1),
          plot.title = element_text(face = "bold", size = plot_title_size, hjust = 0.5),
          plot.margin = margin(top_margin,right_margin,bottom_margin,left_margin)
    )
  
  
  return(my_theme)
}

## SYMBOLS LIST
# https://cran.r-project.org/web/packages/utf8/vignettes/utf8.html
jh_symbols_list <- list(superscript_2 = "²",
                        degree_symbol = "º",
                        rho_label = sprintf('\u03c1'),
                        multiplication_symbol = "×", 
                        beta_symbol = "β", 
                        plus_minus_sign = "±")

# jh_symbols_list

# 0 1 2 3 4 5 6 7 8 9 a b c d e f
# 8                                
# 9                                
# a   ¡ ¢ £ ¤ ¥ ¦ § ¨ © ª « ¬   ® ¯
# b ° ± ² ³ ´ µ ¶ · ¸ ¹ º » ¼ ½ ¾ ¿
# c À Á Â Ã Ä Å Æ Ç È É Ê Ë Ì Í Î Ï
# d Ð Ñ Ò Ó Ô Õ Ö × Ø Ù Ú Û Ü Ý Þ ß
# e à á â ã ä å æ ç è é ê ë ì í î ï
# f ð ñ ò ó ô õ ö ÷ ø ù ú û ü ý þ ÿ

## COLOR PALETTES 

colorblind_palette <<- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
colorblind_palette_dark <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

color_palette_orange_red_9 <- RColorBrewer::brewer.pal(n = 9, name = "OrRd")
color_palette_paired_8 <- RColorBrewer::brewer.pal(12, name = "Paired")
color_palette_reds_9 <<- RColorBrewer::brewer.pal(n = 9, name = "Reds")
color_palette_blues_9 <<- RColorBrewer::brewer.pal(n = 9, name = "Blues")
color_palette_greens_9 <- RColorBrewer::brewer.pal(n = 9, name = "PuBuGn")
color_palette_dark_8 <- RColorBrewer::brewer.pal(n =8,name = "Dark2")

jh_colors_list <- list(
  # view_all_colors = colors_plot,
  colors_reds_4 = color_palette_reds_9[9:6],
  colors_reds_5 = color_palette_reds_9[9:5],
  colors_reds_6 = color_palette_reds_9[9:4],
  colors_blues_4 = color_palette_blues_9[9:6],
  colors_blues_5 = color_palette_blues_9[9:5],
  colors_blues_6 = color_palette_blues_9[9:4],
  colors_greens_4 = color_palette_greens_9[9:6],
  colors_greens_5 = color_palette_greens_9[9:5],
  colors_greens_6 = color_palette_greens_9[9:4], 
  colorblind_palette_light = colorblind_palette,
  colorblind_palette_dark = colorblind_palette_dark
)

jh_colors_list$view_all_colors_plot <- tibble(jh_colors_list) %>%
  mutate(group = names(jh_colors_list)) %>%
  unnest() %>%
  group_by(group) %>%
  mutate(count = row_number()) %>%
  ungroup() %>%
  ggplot(aes(x = 1, y = count, color = jh_colors_list)) +
  geom_point(size = 3) +
  geom_text(aes(label = jh_colors_list, x = 0.45, y = count)) +
  xlim(0, 1.1) +
  facet_wrap(facets = "group", scales = "free") +
  jh_theme_function() +
  scale_color_identity()



jh_view_color_palette_function <- function(color_palette){
  tibble(y = c(1:length(color_palette))) %>%
    mutate(color = color_palette) %>%
    ggplot() + 
    geom_hline(aes(yintercept = y, color = color), size = 3) + 
    scale_color_identity()
}


spine_levels_indexed_df <- tibble(level = c('occiput', 'o_c1', 'c1', 'c1_c2', 'c2', 'c2_c3', 'c3', 'c3_c4', 'c4', 'c4_c5', 'c5', 'c5_c6', 'c6', 'c6_c7', 'c7', 'c7_t1', 't1', 't1_t2', 't2', 't2_t3', 't3', 't3_t4', 't4', 't4_t5', 't5', 't5_t6', 't6', 't6_t7', 't7', 't7_t8', 't8', 't8_t9', 't9', 't9_t10', 't10', 't10_t11', 't11', 't11_t12', 't12', 't12_l1', 'l1', 'l1_l2', 'l2', 'l2_l3', 'l3', 'l3_l4', 'l4', 'l4_l5', 'l5', 'l5_s1', 's1', 'pelvis'), 
                                  level_index = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5, 11, 11.5, 12, 12.5, 13, 13.5, 14, 14.5, 15, 15.5, 16, 16.5, 17, 17.5, 18, 18.5, 19, 19.5, 20, 20.5, 21, 21.5, 22, 22.5, 23, 23.5, 24, 24.5, 25, 26))

jh_spine_level_numeric_function <- function(spine_level = "x", spine_index = 100){
  if(spine_level !='x'){
    spine_level <- str_to_lower(str_replace(spine_level, "-", "_"))
    
    return(spine_levels_indexed_df %>%
             filter(level == spine_level) %>%
             pull(level_index))
  }else if(spine_index < 28){
    return(spine_levels_indexed_df %>%
             filter(level_index == spine_index) %>%
             pull(level))
  }
}





############################ SHINY INPUT & MODAL FUNCTIONS #####################
create_spine_rigid_level_input_function <- function(segment_input_label){
  segment_id <- paste0("preop_", str_to_lower(str_replace_all(segment_input_label, pattern = "-", "_")), "_segment")
  rigid_segment_id <- str_replace_all(segment_id, "_segment", "_rigid_xray")
  segment_label <- segment_input_label
  div(
    class = "segment-input",
    prettyCheckbox(
      inputId = rigid_segment_id,
      label = segment_label,
      value = FALSE,
      bigger = TRUE,
      status = "danger",
      shape = "curve"
    )
  )
  # )
}


# generate_spine_level_controls <- function(spine_level, 
#                                           spine_level_font_size = 11, 
#                                           return_as_full_table = TRUE) {
#   
#   label_style <- glue("font-size:{paste(spine_level_font_size)}px; color:darkblue; font-weight:bold; text-align:center; margin-top:0; margin-bottom:0")
#   
#   spine_level_id <- str_to_lower(str_replace_all(spine_level, "-", "_"))
#   
#   label_percent_width <- 30
#   button_percent_width <- (100-label_percent_width)/4
#   
#   row <- tags$tr(width = "100%",
#                  tags$td(width = paste0(button_percent_width, "%"),
#                          actionBttn(
#                            inputId = paste0(spine_level_id, "_lordosis_down_5"),
#                            label = "-5",
#                            style = "material-circle",
#                            size = "xs"
#                          )
#                  ),
#                  tags$td(width = paste0(button_percent_width, "%"),
#                          actionBttn(
#                            inputId = paste0(spine_level_id, "_lordosis_down_1"),
#                            label = "-1",
#                            style = "material-circle",
#                            size = "xs"
#                          )
#                  ),
#                  tags$td(width = paste0(label_percent_width, "%"), 
#                          tags$div(style = label_style, paste(spine_level))),
#                  tags$td(width = paste0(button_percent_width, "%"),
#                          actionBttn(
#                            inputId = paste0(spine_level_id, "_lordosis_up_1"),
#                            label = "+1",
#                            style = "material-circle",
#                            size = "xs"
#                          )
#                  ),
#                  tags$td(width = paste0(button_percent_width, "%"),
#                          actionBttn(
#                            inputId = paste0(spine_level_id, "_lordosis_up_5"),
#                            label = "+5",
#                            style = "material-circle",
#                            size = "xs"
#                          )
#                  ),
#   )
#   
#   if(return_as_full_table == TRUE){
#     return(tags$table(width = "100%",
#                       row))
#   }else{
#     return(row)
#   }
#   
# }

# generate_spine_level_controls <- function(spine_level) {
#   spine_level_id <- str_to_lower(str_replace_all(spine_level, "-", "_"))
#   
#   tags$tr( 
#     tags$div(
#       class = "btn-group",
#       role  = "group",
#       `aria-label` = "Basic example",
#       # -5 button
#       tags$button(
#         type  = "button",
#         class = "btn btn-secondary",
#         id    = paste0(spine_level_id, "_lordosis_down_5"),
#         "-5º"
#       ),
#       
#       # -1 button
#       tags$button(
#         type  = "button",
#         class = "btn btn-secondary",
#         id    = paste0(spine_level_id, "_lordosis_down_1"),
#         "-1º"
#       )
#     ),
#     
#     # Spine level label
#     tags$span(
#       spine_level,
#       style = "font-weight: bold; padding: 0 10px; color: darkblue;"
#     ),
#     tags$div(
#       class = "btn-group",
#       role  = "group",
#       `aria-label` = "Basic example",
#       # -5 button
#       # +1 button
#       tags$button(
#         type  = "button",
#         class = "btn btn-secondary",
#         id    = paste0(spine_level_id, "_lordosis_up_1"),
#         "+1º"
#       ),
#       
#       # +5 button
#       tags$button(
#         type  = "button",
#         class = "btn btn-secondary",
#         id    = paste0(spine_level_id, "_lordosis_up_5"),
#         "+5º"
#       )
#     ))
# }

generate_spine_level_controls <- function(spine_level) {
  spine_level_id <- str_to_lower(str_replace_all(spine_level, "-", "_"))
  
  left_button_group_style <- "font-weight: bold;
        padding: 0px; 
        float: right;
        border-radius: 10px;
        border: groove;
        margin: 1px;
        color: darkblue"  
  
  right_button_group_style <- "font-weight: bold;
        padding: 0px; 
        float: left;
        border-radius: 10px;
        border: groove;
        margin: 1px;
        color: darkblue"  
  
  left_button_style <- "border-right: 2px solid darkgrey;
  padding: 0px 10px;
  line-height: 1;
  font-size: 14px"  
  
  right_button_style <- "border-left: 2px solid darkgrey;
  padding: 0px 10px;
  line-height: 1;
  font-size: 14px" 
  
  spine_level_style <- "font-weight: bold;
        font-size: 12px;
        padding: 1px;
        line-height: 1;
        text-align: -webkit-center;
        color: darkblue"
  
  tags$tr(
    tags$td(
      class = "btn-group",
      role  = "group",
      `aria-label` = "Basic example",
      style = left_button_group_style,
      tags$button(
        type  = "button",
        class = "btn btn-secondary",
        style = left_button_style,
        id    = paste0(spine_level_id, "_lordosis_down_5"),
        "-5º"
      ),
      tags$button(
        type  = "button",
        class = "btn btn-secondary",
        style = right_button_style,
        id    = paste0(spine_level_id, "_lordosis_down_1"),
        "-1º"
      )
    ),
    tags$td(
      style = spine_level_style,
      spine_level
    ),
    tags$td(
      class = "btn-group",
      role  = "group",
      `aria-label` = "Basic example",
      style = right_button_group_style,
      tags$button(
        type  = "button",
        class = "btn btn-secondary",
        style = left_button_style,
        id    = paste0(spine_level_id, "_lordosis_up_1"),
        "+1º"
      ),
      tags$button(
        type  = "button",
        class = "btn btn-secondary",
        style = right_button_style,
        id    = paste0(spine_level_id, "_lordosis_up_5"),
        "+5º"
      )
    ),
  )
  
}

update_spine_segmental_planning_df_function <- function(spine_segmental_planning_df,
                                                        spine_interspace_input, 
                                                        change) {
  spine_segmental_planning_df$df <- spine_segmental_planning_df$df %>%
    mutate(adjustment = if_else(spine_interspace == spine_interspace_input, adjustment + change, adjustment))
  # mutate(adjustment_performed = if_else(spine_interspace == spine_interspace_input, "no", adjustment_performed))
}


update_spine_segmental_planning_table_observe_button_function <- function(spine_segmental_planning_df, 
                                                                          spine_interspace, session) {
  spine_interspace_id <- gsub("-", "_", tolower(spine_interspace))  # Ensure ID consistency
  
  observeEvent(session$input[[paste0(spine_interspace_id, "_lordosis_down_5")]], {
    spine_segmental_planning_df$df <- update_spine_segmental_planning_df_function(spine_segmental_planning_df = spine_segmental_planning_df,
                                                                                  spine_interspace, -5)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(session$input[[paste0(spine_interspace_id, "_lordosis_down_1")]], {
    spine_segmental_planning_df$df <- update_spine_segmental_planning_df_function(spine_segmental_planning_df = spine_segmental_planning_df,
                                                                                  spine_interspace, -1)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(session$input[[paste0(spine_interspace_id, "_lordosis_up_1")]], {
    spine_segmental_planning_df$df <- update_spine_segmental_planning_df_function(spine_segmental_planning_df = spine_segmental_planning_df,
                                                                                  spine_interspace, 1)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(session$input[[paste0(spine_interspace_id, "_lordosis_up_5")]], {
    spine_segmental_planning_df$df <-  update_spine_segmental_planning_df_function(spine_segmental_planning_df = spine_segmental_planning_df,
                                                                                   spine_interspace, 5)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
}



jh_construct_rod_coordinates_function <- function(planned_spine_coord_df,
                                                  uiv = "T4", 
                                                  liv = "Pelvis",
                                                  spine_orientation = "left",
                                                  number_of_knots = 10){
  
  spine_coord_for_rod_list <- jh_convert_spine_coord_df_to_lists_function(planned_spine_coord_df)
  
  # print("got to here in rod function")
  
  if(str_to_lower(liv) == "pelvis"){
    inferior_rod_point <- jh_get_point_along_line_function(coord_a = spine_coord_for_rod_list$l5$ip, 
                                                           coord_b = spine_coord_for_rod_list$sacrum$sp, 
                                                           percent_a_to_b = 10)
    
    # print("got to pelvis in rod function")
  }else if(str_to_lower(liv) == "s1" | str_to_lower(liv) == "sacrum"){
    
    inferior_rod_point <- jh_get_point_along_line_function(coord_a = spine_coord_for_rod_list$l5$ip, 
                                                           coord_b = spine_coord_for_rod_list$sacrum$sp, 
                                                           percent_a_to_b = 5)
    
    # print("got to inf rod point in non-pelvis liv function")
  }else{
    inferior_rod_point <-  spine_coord_for_rod_list[[which(names(spine_coord_for_rod_list) == str_to_lower(liv))]]$ip
  }
  
  
  
  superior_rod_point <-  spine_coord_for_rod_list[[which(names(spine_coord_for_rod_list) == str_to_lower(uiv)) + 1]]$ip
  
  s1_length <- jh_calculate_distance_between_2_points_function(point_1 = spine_coord_for_rod_list$sacrum$sa, 
                                                               point_2 = spine_coord_for_rod_list$sacrum$sp)
  
  x_modifier <- if_else(spine_orientation == "left", s1_length, s1_length*-1)
  
  rod_coord_df <- planned_spine_coord_df %>%
    filter(vert_point %in% c("s1_posterior_superior", "sp")) %>%
    select(spine_level, x, y) %>%
    filter(y < superior_rod_point[[2]])%>%
    filter(y > inferior_rod_point[[2]]) %>% 
    add_row(spine_level = "superior_rod", x = superior_rod_point[[1]], y = superior_rod_point[[2]]) %>%
    add_row(spine_level = "inferior_rod", x = inferior_rod_point[[1]], y = inferior_rod_point[[2]]) %>%
    mutate(x = x + x_modifier)%>%
    arrange(y)
  
  
  dist_vals <- sqrt(diff(rod_coord_df$x)^2 + diff(rod_coord_df$y)^2)
  t_vals <- c(0, cumsum(dist_vals))
  spx <- smooth.spline(t_vals, rod_coord_df$x)
  spy <- smooth.spline(t_vals, rod_coord_df$y)
  t_seq <- seq(from = min(t_vals), to = max(t_vals), length.out = number_of_knots)
  
  smoothed_rod_df <-tibble(x = predict(spx, t_seq)$y,
                           y = predict(spy, t_seq)$y
  )
  
  smoothed_rod_df <- smoothed_rod_df %>%
    mutate(index = seq(from = 0, to = 90, length = length(smoothed_rod_df$x))) %>%
    full_join(tibble(index = c(0:90))) %>%
    arrange(index) %>%
    mutate(x = zoo::na.spline(x)) %>%
    mutate(y = zoo::na.spline(y)) 
  
  return(smoothed_rod_df)
  
}


jh_format_text_to_print_tibble_in_shiny_function <- function(df){
  # Start building the code string
  code_str <- "spine_plan_df <- tibble("
  
  # Loop through each column of the tibble
  for(i in seq_along(df)) {
    col_name <- names(df)[i]
    col_values <- df[[i]]
    
    # If the column is character or factor, wrap values in quotes
    if(is.character(col_values) || is.factor(col_values)) {
      col_text <- paste0("'", col_values, "'", collapse = ", ")
    } else {
      col_text <- paste0(col_values, collapse = ", ")
    }
    
    # Add this column’s code to the overall string
    code_str <- paste0(
      code_str, "\n  ", col_name, " = c(", col_text, ")",
      if (i < ncol(df)) "," else ""
    )
  }
  
  # Close the tibble call
  code_str <- paste0(code_str, "\n)")
  
  # Return the full tibble definition as text
  code_str 
}

jh_reformat_vert_tibble <- function(tibble_input) {
  # Convert the tibble to a named list with each point as a vector
  named_list <- list(
    sp = c(tibble_input$x[tibble_input$vert_point == "sp"], tibble_input$y[tibble_input$vert_point == "sp"]),
    sa = c(tibble_input$x[tibble_input$vert_point == "sa"], tibble_input$y[tibble_input$vert_point == "sa"]),
    ia = c(tibble_input$x[tibble_input$vert_point == "ia"], tibble_input$y[tibble_input$vert_point == "ia"]),
    ip = c(tibble_input$x[tibble_input$vert_point == "ip"], tibble_input$y[tibble_input$vert_point == "ip"]),
    centroid = c(tibble_input$x[tibble_input$vert_point == "centroid"], tibble_input$y[tibble_input$vert_point == "centroid"])
  )
  return(named_list)
}



jh_convert_spine_coord_df_to_lists_function <- function(spine_coord_df = tibble(spine_level = character(), vert_point = character(), x = numeric(), y = numeric())){
  spine_coord_df_with_list_df <- spine_coord_df %>%
    mutate(coord_list = map2(.x = x, .y = y, .f = ~ c(.x, .y))) %>%
    select(spine_level, vert_point, coord_list) %>%
    group_by(spine_level) 
  
  names(spine_coord_df_with_list_df$coord_list) <- spine_coord_df_with_list_df$vert_point
  
  spine_coord_list <- map(.x = unique(spine_coord_df_with_list_df$spine_level),
                          .f = ~ spine_coord_df_with_list_df %>%
                            filter(spine_level == .x)%>%
                            pull(coord_list))
  
  names(spine_coord_list) <- unique(spine_coord_df_with_list_df$spine_level)
  return(spine_coord_list)
}   



jh_make_rod_plot_for_pdf_function <- function(uiv, liv, planned_spine_vert_coord_df, rod_coord_df){
  
  return_list <- list()
  
  spine_levels <- c("pelvis", "sacrum", "l5", "l4", "l3", "l2", "l1",
                    "t12", "t11", "t10", "t9", "t8", "t7", "t6", "t5", "t4", "t3", "t2", "t1",
                    "c7", "c6", "c5", "c4", "c3", "c2", "c1")
  
  # Ensure spine_level is a factor with the correct order
  instrumented_vert_centered_df <- planned_spine_vert_coord_df %>%
    mutate(spine_level = factor(spine_level, levels = spine_levels, ordered = TRUE)) %>%
    group_by(spine_level) %>%
    filter(spine_level >= str_to_lower(liv) & spine_level <= str_to_lower(uiv)) %>%
    mutate(x = x - rod_coord_df$x[[1]])%>%
    mutate(y = y - rod_coord_df$y[[1]])
  
  rod_coord_centered_df <- rod_coord_df %>%
    mutate(x = x - rod_coord_df$x[[1]])%>%
    mutate(y = y - rod_coord_df$y[[1]])
  
  # # Calculate the actual plot range after centering
  x_range_centered <- range(c(rod_coord_centered_df$x, instrumented_vert_centered_df$x), na.rm = TRUE)
  y_range_centered <- range(c(rod_coord_centered_df$y, instrumented_vert_centered_df$y), na.rm = TRUE)
  
  y_grid_breaks <- 50
  
  y_grid_max <- ceiling(y_range_centered[[2]]/ y_grid_breaks) * y_grid_breaks
  y_grid_min <- floor(y_range_centered[[1]]/ y_grid_breaks) * y_grid_breaks
  x_grid_max <- ceiling(x_range_centered[[2]]/ 20) * 20
  x_grid_min <- floor(x_range_centered[[1]]/ 20) * 20
  
  grid_corners_df <- expand_grid(x = c(x_grid_min, x_grid_max), y = c(y_grid_min, y_grid_max))
  
  y_gridline_tibble_df <- tibble(x_min = x_grid_min, x_max = x_grid_max, y = seq(from = y_grid_min, to = y_grid_max, by = 50))
  
  x_gridline_tibble_df <- tibble(x = seq(from = x_grid_min, to = x_grid_max, by = 40), y_min = y_grid_min, y_max = y_grid_min + 25)%>%
    slice(-1)
  
  return_list$grid_corners_df <- grid_corners_df
  
  return_list$rod_plot_template <- ggplot() +
    geom_polygon(data = instrumented_vert_centered_df, aes(x = x, y = y, group = spine_level), color = "grey50", fill = NA, alpha = 0.3) +
    geom_segment(data = y_gridline_tibble_df, aes(x = x_min, xend = x_max, y = y, yend = y), color = "grey75", linetype = "dashed")+ 
    draw_text(text = paste0(as.character(y_gridline_tibble_df$y), "mm"), x = y_gridline_tibble_df$x_min + 20, y = y_gridline_tibble_df$y, size = 7) +
    geom_segment(data = x_gridline_tibble_df, aes(x = x, xend = x, y = y_min, yend = y_max), color = "grey75", linetype = "dashed")+ 
    draw_text(text = paste0(as.character(x_gridline_tibble_df$x), "mm"),
              x = x_gridline_tibble_df$x, y = x_gridline_tibble_df$y_max, size = 7) +
    geom_path(data = rod_coord_centered_df,
              aes(x = x, y = y),
              color = "blue",
              size = 2,
              lineend = "round",
              linejoin = "round") +
    geom_point(data = grid_corners_df, aes(x =x, y = y), color = "red") +
    coord_fixed(expand = FALSE, xlim = c(x_grid_min, x_grid_max), ylim = c(y_grid_min, y_grid_max)) +
    theme_void()
  
  return_list$plot_width_mm <- diff(c(x_grid_min, x_grid_max))  # Width in mm
  return_list$plot_height_mm <- diff(c(y_grid_min, y_grid_max)) # Height in mm
  
  # Convert mm to inches (1 inch = 25.4 mm)
  return_list$plot_width_in <- return_list$plot_width_mm / 25.4
  return_list$plot_height_in <- return_list$plot_height_mm / 25.4
  
  return(return_list)
  
}
