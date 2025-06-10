# Bivariate maps od coverage and consumption of rice, wheat, sorghum, maize and millet using SEN as an example
# AuthOr: Uchenna A
# pacman::p_load(tidyverse,
#                readxl,
#                gridExtra,
#                survey,
#                sf,
#                srvyr,
#                devtools,
#                rnaturalearth,
#                tmap,
#                wesanderson,
#                stringi,
#                haven,
#                biscale,
#                cowplot)


# Step 1: Function for food cons
process_food_consumption2 <- function(hh_info_path, cons_path) {
  hh_info <- read.csv(hh_info_path)
  
  cons_data <- read.csv(cons_path) %>%
    left_join(hh_info, by = "hhid") %>%
    mutate(quantity_g = quantity_g / afe) %>%
    select(hhid, item_code, quantity_g, adm1, survey_wgt, ea, res)
  
  return(cons_data)
}

# Step 2: Specify shapefile object
sen_adm1 <- ne_states(country = "Senegal", returnclass = "sf")

# Select only the name and geometry columns (you can inspect the whole objects)
sen_adm1 <- sen_adm1 %>% 
  select(name, geometry) %>%
  rename(adm1=name) %>%
  mutate(adm1 = recode(adm1, "Kédougou" = "Kedougou",
                       "Sédhiou"="Sedhiou",
                       "Thiès"="Thies"))

#=================================================Rice======================================================
# function to calculate rice reach
calculate_rice_reach <- function(data, rice_codes, hhid_col, adm1_col, survey_wgt_col, ea_col, res_col, item_code_col){
  
  # Mark rice consumers
  rice <- data %>%
    filter({{item_code_col}} %in% rice_codes) %>%
    mutate(consumed_rice = 1)
  
  # Mark non-rice consumers and combine
  all_rice <- data %>%
    filter(!( {{hhid_col}} %in% rice[[rlang::as_name(enquo(hhid_col))]] )) %>%
    mutate(consumed_rice = 0) %>%
    bind_rows(rice)
  
  # Collapse to one row per household
  hh_rice_status <- all_rice %>%
    group_by({{hhid_col}}, {{adm1_col}}, {{survey_wgt_col}}, {{ea_col}}, {{res_col}}) %>%
    summarise(consumed_rice = max(consumed_rice), .groups = "drop")
  
  # Survey design and calculate reach
  reach_rice <- hh_rice_status %>%
    as_survey_design(ids = {{ea_col}}, strata = {{res_col}}, weights = {{survey_wgt_col}}, , nest = T) %>%
    group_by({{adm1_col}}) %>%
    summarise(rice_reach_pct = survey_mean(consumed_rice, proportion = TRUE) * 100) %>%
    select(-rice_reach_pct_se)
  
  return(reach_rice)
}

# Function to calculate rice intake
calculate_rice_intake <- function(data, rice_codes, adm1_col, quantity_col, survey_wgt_col, ea_col, res_col, item_code_col){
  # Filter rice items
  rice_quantity <- data %>%
    filter({{item_code_col}} %in% rice_codes)
  
  # Survey design
  rice_svy_design <- rice_quantity %>%
    as_survey_design(ids = {{ea_col}}, strata = {{res_col}}, weights = {{survey_wgt_col}}, nest = T)
  
  # Calculate survey-weighted mean rice consumption
  intake_rice <- rice_svy_design %>%
    group_by({{adm1_col}}) %>%
    summarise(mean_rice_g = survey_mean({{quantity_col}})) %>%
    select(-mean_rice_g_se)
  
  return(intake_rice)
}


sen_food_cons <- process_food_consumption2("sen_ehcvm2122_hh_info.csv",
                                           "sen_food_consumption.csv")

# Example usage:
sen_reach_rice <- calculate_rice_reach(
  data = sen_food_cons,
  rice_codes = c(1, 2, 3, 4),
  hhid_col = hhid,
  adm1_col = adm1,
  survey_wgt_col = survey_wgt,
  ea_col = ea,
  res_col = res,
  item_code_col = item_code
)

# View the result
sen_reach_rice

# Calculate rice intake
sen_intake_rice <- calculate_rice_intake(
  data = sen_food_cons,
  rice_codes = c(1, 2, 3, 4),
  adm1_col = adm1,
  quantity_col = quantity_g,
  survey_wgt_col = survey_wgt,
  ea_col = ea,
  res_col = res,
  item_code_col = item_code
)

# View result
sen_intake_rice


sen_reach_intake <- 
  sen_reach_rice %>% 
  left_join(sen_intake_rice, by = "adm1") %>% 
  mutate(
    across(everything(), ~ifelse(is.na(.), 0, .))
  ) %>% 
  mutate(
    # Create bins for the reach percentage from rice reach
    reach_bins = cut(
      rice_reach_pct, 
      breaks = c(0, 25, 50, 75, 100), 
      include.lowest = TRUE
    ),
    # Create bins for rice intake (mean consumption in grams)
    intake_bins = cut(
      mean_rice_g, 
      breaks = c(-Inf, 75, 149, 300, Inf),
      labels = c("<75",  "75–149",  "150–300",  ">300"),
      include.lowest = TRUE
    ),
    # Convert adm1 to character to match the shapefile's adm1 column
    adm1 = as.character(adm1)
  ) %>% 
  select(adm1, mean_rice_g, reach_bins, intake_bins) %>% 
  left_join(sen_adm1, by = "adm1") %>% 
  st_as_sf()

# create a bi classs
sen_data_rice <- bi_class(sen_reach_intake, x =reach_bins , y = intake_bins, dim = 4 )


# using ggplot and bi_scale, create a bivariate map
bi_map_rice <- ggplot() + 
  geom_sf(data = sen_data_rice, mapping = aes(fill = bi_class), color = NA,show.legend = F)+
  bi_scale_fill(pal = "DkBlue2",dim = 4)+
  bi_theme()+
  geom_sf(data = sen_adm1, fill= NA, color = 'black', lwd = 1) + 
  #geom_sf_text(data = sen_adm1, aes(label = adm1), size = 3, color = 'black', fontface = 'bold') +
  labs(subtitle = "Coverage and Consumption of Rice in Senegal", )

# create a df of the breaks for each exis
break_vals <- bi_class_breaks(sen_reach_intake, x =reach_bins , y = intake_bins, dim = 4 )

#create a bivariate legend
legend_rice <- bi_legend(pal = "DkBlue2",
                         dim = 4,
                         xlab = "Higher Reach (%) ",
                         ylab = "Higher Consumption (g) ",
                         size = 8, 
                         breaks = break_vals)

# put legend and map together
sen_rice_bivariate <- ggdraw() +
  draw_plot(bi_map_rice, 0, 0, 1, 1) +
  draw_plot(legend_rice, 0.65, .2, 0.45, 0.2)

sen_rice_bivariate


#==================================================millet========================================================
calculate_millet_reach <- function(data, millet_codes, hhid_col, adm1_col, survey_wgt_col, ea_col, res_col, item_code_col){
  
  # Mark millet consumers
  millet <- data %>%
    filter({{item_code_col}} %in% millet_codes) %>%
    mutate(consumed_millet = 1)
  
  # Mark non-millet consumers and combine
  all_millet <- data %>%
    filter(!( {{hhid_col}} %in% millet[[rlang::as_name(enquo(hhid_col))]] )) %>%
    mutate(consumed_millet = 0) %>%
    bind_rows(millet)
  
  # Collapse to one row per household
  hh_millet_status <- all_millet %>%
    group_by({{hhid_col}}, {{adm1_col}}, {{survey_wgt_col}}, {{ea_col}}, {{res_col}}) %>%
    summarise(consumed_millet = max(consumed_millet), .groups = "drop")
  
  # Survey design and calculate reach
  reach_millet <- hh_millet_status %>%
    as_survey_design(ids = {{ea_col}}, strata = {{res_col}}, weights = {{survey_wgt_col}}, , nest = T) %>%
    group_by({{adm1_col}}) %>%
    summarise(millet_reach_pct = survey_mean(consumed_millet, proportion = TRUE) * 100) %>%
    select(-millet_reach_pct_se)
  
  return(reach_millet)
}

calculate_millet_intake <- function(data, millet_codes, adm1_col, quantity_col, survey_wgt_col, ea_col, res_col, item_code_col){
  # Filter millet items
  millet_quantity <- data %>%
    filter({{item_code_col}} %in% millet_codes)
  
  # Survey design
  millet_svy_design <- millet_quantity %>%
    as_survey_design(ids = {{ea_col}}, strata = {{res_col}}, weights = {{survey_wgt_col}}, nest = T)
  
  # Calculate survey-weighted mean millet consumption
  intake_millet <- millet_svy_design %>%
    group_by({{adm1_col}}) %>%
    summarise(mean_millet_g = survey_mean({{quantity_col}})) %>%
    select(-mean_millet_g_se)
  
  return(intake_millet)
}


sen_food_cons <- process_food_consumption2("sen_ehcvm2122_hh_info.csv",
                                           "sen_food_consumption.csv")

# Example usage:
sen_reach_millet <- calculate_millet_reach(
  data = sen_food_cons,
  millet_codes = c(7),
  hhid_col = hhid,
  adm1_col = adm1,
  survey_wgt_col = survey_wgt,
  ea_col = ea,
  res_col = res,
  item_code_col = item_code
)

# View the result
sen_reach_millet

# Calculate millet intake
sen_intake_millet <- calculate_millet_intake(
  data = sen_food_cons,
  millet_codes = c(7),
  adm1_col = adm1,
  quantity_col = quantity_g,
  survey_wgt_col = survey_wgt,
  ea_col = ea,
  res_col = res,
  item_code_col = item_code
)

# View result
sen_intake_millet

sen_reach_intake <- 
  sen_reach_millet %>% 
  left_join(sen_intake_millet, by = "adm1") %>% 
  mutate(
    across(everything(), ~ifelse(is.na(.), 0, .))
  ) %>% 
  mutate(
    # Create bins for the reach percentage from millet reach
    reach_bins = cut(
      millet_reach_pct, 
      breaks = c(0, 25, 50, 75, 100), 
      include.lowest = TRUE
    ),
    # Create bins for millet intake (mean consumption in grams)
    intake_bins = cut(
      mean_millet_g, 
      breaks = c(-Inf, 75, 149, 300, Inf),
      labels = c("<75",  "75–149",  "150–300",  ">300"),
      include.lowest = TRUE
    ),
    # Convert adm1 to character to match the shapefile's adm1 column
    adm1 = as.character(adm1)
  ) %>% 
  select(adm1, mean_millet_g, reach_bins, intake_bins) %>% 
  left_join(sen_adm1, by = "adm1") %>% 
  st_as_sf()

# create a bi classs
sen_data_millet <- bi_class(sen_reach_intake, x =reach_bins , y = intake_bins, dim = 4 )


# using ggplot and bi_scale, create a bivariate map
bi_map_millet <- ggplot() + 
  geom_sf(data = sen_data_millet, mapping = aes(fill = bi_class), color = NA,show.legend = F)+
  bi_scale_fill(pal = "DkBlue2",dim = 4)+
  bi_theme()+
  geom_sf(data = sen_adm1, fill= NA, color = 'black', lwd = 1) + 
  #geom_sf_text(data = sen_adm1, aes(label = adm1), size = 3, color = 'black', fontface = 'bold') +
  labs(subtitle = "Coverage and Consumption of millet in Senegal", )

# create a df of the breaks for each exis
break_vals <- bi_class_breaks(sen_reach_intake, x =reach_bins , y = intake_bins, dim = 4 )

#create a bivariate legend
legend_millet <- bi_legend(pal = "DkBlue2",
                           dim = 4,
                           xlab = "Higher Reach (%) ",
                           ylab = "Higher Consumption (g) ",
                           size = 8, 
                           breaks = break_vals)

# put legend and map together
sen_millet_bivariate <- ggdraw() +
  draw_plot(bi_map_millet, 0, 0, 1, 1) +
  draw_plot(legend_millet, 0.65, .2, 0.45, 0.2)

sen_millet_bivariate

#==================================================Sorghum=====================================================
calculate_sorghum_reach <- function(data, sorghum_codes, hhid_col, adm1_col, survey_wgt_col, ea_col, res_col, item_code_col){
  
  # Mark sorghum consumers
  sorghum <- data %>%
    filter({{item_code_col}} %in% sorghum_codes) %>%
    mutate(consumed_sorghum = 1)
  
  # Mark non-sorghum consumers and combine
  all_sorghum <- data %>%
    filter(!( {{hhid_col}} %in% sorghum[[rlang::as_name(enquo(hhid_col))]] )) %>%
    mutate(consumed_sorghum = 0) %>%
    bind_rows(sorghum)
  
  # Collapse to one row per household
  hh_sorghum_status <- all_sorghum %>%
    group_by({{hhid_col}}, {{adm1_col}}, {{survey_wgt_col}}, {{ea_col}}, {{res_col}}) %>%
    summarise(consumed_sorghum = max(consumed_sorghum), .groups = "drop")
  
  # Survey design and calculate reach
  reach_sorghum <- hh_sorghum_status %>%
    as_survey_design(ids = {{ea_col}}, strata = {{res_col}}, weights = {{survey_wgt_col}}, , nest = T) %>%
    group_by({{adm1_col}}) %>%
    summarise(sorghum_reach_pct = survey_mean(consumed_sorghum, proportion = TRUE) * 100) %>%
    select(-sorghum_reach_pct_se)
  
  return(reach_sorghum)
}

calculate_sorghum_intake <- function(data, sorghum_codes, adm1_col, quantity_col, survey_wgt_col, ea_col, res_col, item_code_col){
  # Filter sorghum items
  sorghum_quantity <- data %>%
    filter({{item_code_col}} %in% sorghum_codes)
  
  # Survey design
  sorghum_svy_design <- sorghum_quantity %>%
    as_survey_design(ids = {{ea_col}}, strata = {{res_col}}, weights = {{survey_wgt_col}}, nest = T)
  
  # Calculate survey-weighted mean sorghum consumption
  intake_sorghum <- sorghum_svy_design %>%
    group_by({{adm1_col}}) %>%
    summarise(mean_sorghum_g = survey_mean({{quantity_col}})) %>%
    select(-mean_sorghum_g_se)
  
  return(intake_sorghum)
}


sen_food_cons <- process_food_consumption2("sen_ehcvm2122_hh_info.csv",
                                           "sen_food_consumption.csv")

# Example usage:
sen_reach_sorghum <- calculate_sorghum_reach(
  data = sen_food_cons,
  sorghum_codes = c(8),
  hhid_col = hhid,
  adm1_col = adm1,
  survey_wgt_col = survey_wgt,
  ea_col = ea,
  res_col = res,
  item_code_col = item_code
)

# View the result
sen_reach_sorghum

# Calculate sorghum intake
sen_intake_sorghum <- calculate_sorghum_intake(
  data = sen_food_cons,
  sorghum_codes = c(8),
  adm1_col = adm1,
  quantity_col = quantity_g,
  survey_wgt_col = survey_wgt,
  ea_col = ea,
  res_col = res,
  item_code_col = item_code
)

# View result
sen_intake_sorghum


sen_reach_intake <- 
  sen_reach_sorghum %>% 
  left_join(sen_intake_sorghum, by = "adm1") %>% 
  mutate(
    across(everything(), ~ifelse(is.na(.), 0, .))
  ) %>% 
  mutate(
    # Create bins for the reach percentage from sorghum reach
    reach_bins = cut(
      sorghum_reach_pct, 
      breaks = c(0, 25, 50, 75, 100), 
      include.lowest = TRUE
    ),
    # Create bins for sorghum intake (mean consumption in grams)
    intake_bins = cut(
      mean_sorghum_g, 
      breaks = c(-Inf, 75, 149, 300, Inf),
      labels = c("<75",  "75–149",  "150–300",  ">300"),
      include.lowest = TRUE
    ),
    # Convert adm1 to character to match the shapefile's adm1 column
    adm1 = as.character(adm1)
  ) %>% 
  select(adm1, mean_sorghum_g, reach_bins, intake_bins) %>% 
  left_join(sen_adm1, by = "adm1") %>% 
  st_as_sf()

# create a bi classs
sen_data_sorghum <- bi_class(sen_reach_intake, x =reach_bins , y = intake_bins, dim = 4 )

# using ggplot and bi_scale, create a bivariate map
bi_map_sorghum <- ggplot() + 
  geom_sf(data = sen_data_sorghum, mapping = aes(fill = bi_class), color = NA,show.legend = F)+
  bi_scale_fill(pal = "DkBlue2",dim = 4)+
  bi_theme()+
  geom_sf(data = sen_adm1, fill= NA, color = 'black', lwd = 1) + 
  #geom_sf_text(data = sen_adm1, aes(label = adm1), size = 3, color = 'black', fontface = 'bold') +
  labs(subtitle = "Coverage and Consumption of sorghum in Senegal", )

# create a df of the breaks for each exis
break_vals <- bi_class_breaks(sen_reach_intake, x =reach_bins , y = intake_bins, dim = 4 )

#create a bivariate legend
legend_sorghum <- bi_legend(pal = "DkBlue2",
                            dim = 4,
                            xlab = "Higher Reach (%) ",
                            ylab = "Higher Consumption (g) ",
                            size = 8, 
                            breaks = break_vals)

# put legend and map together
sen_sorghum_bivariate <- ggdraw() +
  draw_plot(bi_map_sorghum, 0, 0, 1, 1) +
  draw_plot(legend_sorghum, 0.65, .2, 0.45, 0.2)

sen_sorghum_bivariate

#==============================================+=Wheat=======================================================
calculate_wheat_reach <- function(data, wheat_codes, hhid_col, adm1_col, survey_wgt_col, ea_col, res_col, item_code_col){
  
  # Mark wheat consumers
  wheat <- data %>%
    filter({{item_code_col}} %in% wheat_codes) %>%
    mutate(consumed_wheat = 1)
  
  # Mark non-wheat consumers and combine
  all_wheat <- data %>%
    filter(!( {{hhid_col}} %in% wheat[[rlang::as_name(enquo(hhid_col))]] )) %>%
    mutate(consumed_wheat = 0) %>%
    bind_rows(wheat)
  
  # Collapse to one row per household
  hh_wheat_status <- all_wheat %>%
    group_by({{hhid_col}}, {{adm1_col}}, {{survey_wgt_col}}, {{ea_col}}, {{res_col}}) %>%
    summarise(consumed_wheat = max(consumed_wheat), .groups = "drop")
  
  # Survey design and calculate reach
  reach_wheat <- hh_wheat_status %>%
    as_survey_design(ids = {{ea_col}}, strata = {{res_col}}, weights = {{survey_wgt_col}}, , nest = T) %>%
    group_by({{adm1_col}}) %>%
    summarise(wheat_reach_pct = survey_mean(consumed_wheat, proportion = TRUE) * 100) %>%
    select(-wheat_reach_pct_se)
  
  return(reach_wheat)
}

calculate_wheat_intake <- function(data, wheat_codes, adm1_col, quantity_col, survey_wgt_col, ea_col, res_col, item_code_col){
  # Filter wheat items
  wheat_quantity <- data %>%
    filter({{item_code_col}} %in% wheat_codes)
  
  # Survey design
  wheat_svy_design <- wheat_quantity %>%
    as_survey_design(ids = {{ea_col}}, strata = {{res_col}}, weights = {{survey_wgt_col}}, nest = T)
  
  # Calculate survey-weighted mean wheat consumption
  intake_wheat <- wheat_svy_design %>%
    group_by({{adm1_col}}) %>%
    summarise(mean_wheat_g = survey_mean({{quantity_col}})) %>%
    select(-mean_wheat_g_se)
  
  return(intake_wheat)
}


sen_food_cons <- process_food_consumption2("sen_ehcvm2122_hh_info.csv",
                                           "sen_food_consumption.csv")

# Example usage:
sen_reach_wheat <- calculate_wheat_reach(
  data = sen_food_cons,
  wheat_codes = c(9,16,21,22,24,25,26),
  hhid_col = hhid,
  adm1_col = adm1,
  survey_wgt_col = survey_wgt,
  ea_col = ea,
  res_col = res,
  item_code_col = item_code
)

# View the result
sen_reach_wheat

calculate_wheat_intake <- function(data,
                                   wheat_codes,
                                   adm1_col,
                                   quantity_col,
                                   survey_wgt_col,
                                   ea_col,
                                   res_col,
                                   item_code_col) {
  
  # define adjustment factors for codes 25-29; others default to 1
  adjustment_map <- c(
    `21` = 0.745,
    `22` = 0.745,
    `24` = 0.630,
    `25` = 0.635,
    `26` = 0.635
  )
  
  data %>%
    # keep only our codes
    filter({{item_code_col}} %in% wheat_codes) %>%
    # adjust quantity based on item code
    mutate(
      adj_quantity = {{quantity_col}} * 
        coalesce(adjustment_map[as.character({{item_code_col}})], 1)
    ) %>%
    # set up survey design
    as_survey_design(
      ids = {{ea_col}},
      strata = {{res_col}},
      weights = {{survey_wgt_col}},
      nest = TRUE
    ) %>%
    # compute mean of the adjusted quantity by region
    group_by({{adm1_col}}) %>%
    summarise(
      mean_wheat_g = survey_mean(adj_quantity),
      .groups = "drop"
    ) %>%
    select(-mean_wheat_g_se)
}

# example usage:
sen_intake_wheat <- calculate_wheat_intake(
  data            = sen_food_cons,
  wheat_codes     = c(9,16,21,22,24,25,26),
  adm1_col        = adm1,
  quantity_col    = quantity_g,
  survey_wgt_col  = survey_wgt,
  ea_col          = ea,
  res_col         = res,
  item_code_col   = item_code
)

sen_intake_wheat

sen_reach_intake <- 
  sen_reach_wheat %>% 
  left_join(sen_intake_wheat, by = "adm1") %>% 
  mutate(
    across(everything(), ~ifelse(is.na(.), 0, .))
  ) %>% 
  mutate(
    # Create bins for the reach percentage from wheat reach
    reach_bins = cut(
      wheat_reach_pct, 
      breaks = c(0, 25, 50, 75, 100), 
      include.lowest = TRUE
    ),
    # Create bins for wheat intake (mean consumption in grams)
    intake_bins = cut(
      mean_wheat_g, 
      breaks = c(-Inf, 75, 149, 300, Inf),
      labels = c("<75",  "75–149",  "150–300",  ">300"),
      include.lowest = TRUE
    ),
    # Convert adm1 to character to match the shapefile's adm1 column
    adm1 = as.character(adm1)
  ) %>% 
  select(adm1, mean_wheat_g, reach_bins, intake_bins) %>% 
  left_join(sen_adm1, by = "adm1") %>% 
  st_as_sf()

# create a bi classs
sen_data_wheat <- bi_class(sen_reach_intake, x =reach_bins , y = intake_bins, dim = 4 )


# using ggplot and bi_scale, create a bivariate map
bi_map_wheat <- ggplot() + 
  geom_sf(data = sen_data_wheat, mapping = aes(fill = bi_class), color = NA,show.legend = F)+
  bi_scale_fill(pal = "DkBlue2",dim = 4)+
  bi_theme()+
  geom_sf(data = sen_adm1, fill= NA, color = 'black', lwd = 1) + 
  #geom_sf_text(data = sen_adm1, aes(label = adm1), size = 3, color = 'black', fontface = 'bold') +
  labs(subtitle = "Coverage and Consumption of wheat in Senegal", )

# create a df of the breaks for each exis
break_vals <- bi_class_breaks(sen_reach_intake, x =reach_bins , y = intake_bins, dim = 4 )

#create a bivariate legend
legend_wheat <- bi_legend(pal = "DkBlue2",
                          dim = 4,
                          xlab = "Higher Reach (%) ",
                          ylab = "Higher Consumption (g) ",
                          size = 8, 
                          breaks = break_vals)

# put legend and map together
sen_wheat_bivariate <- ggdraw() +
  draw_plot(bi_map_wheat, 0, 0, 1, 1) +
  draw_plot(legend_wheat, 0.65, .2, 0.45, 0.2)

sen_wheat_bivariate


#=======================================================Maize==========================================================
calculate_corn_reach <- function(data, corn_codes, hhid_col, adm1_col, survey_wgt_col, ea_col, res_col, item_code_col){
  
  # Mark corn consumers
  corn <- data %>%
    filter({{item_code_col}} %in% corn_codes) %>%
    mutate(consumed_corn = 1)
  
  # Mark non-corn consumers and combine
  all_corn <- data %>%
    filter(!( {{hhid_col}} %in% corn[[rlang::as_name(enquo(hhid_col))]] )) %>%
    mutate(consumed_corn = 0) %>%
    bind_rows(corn)
  
  # Collapse to one row per household
  hh_corn_status <- all_corn %>%
    group_by({{hhid_col}}, {{adm1_col}}, {{survey_wgt_col}}, {{ea_col}}, {{res_col}}) %>%
    summarise(consumed_corn = max(consumed_corn), .groups = "drop")
  
  # Survey design and calculate reach
  reach_corn <- hh_corn_status %>%
    as_survey_design(ids = {{ea_col}}, strata = {{res_col}}, weights = {{survey_wgt_col}}, , nest = T) %>%
    group_by({{adm1_col}}) %>%
    summarise(corn_reach_pct = survey_mean(consumed_corn, proportion = TRUE) * 100) %>%
    select(-corn_reach_pct_se)
  
  return(reach_corn)
}

calculate_corn_intake <- function(data, corn_codes, adm1_col, quantity_col, survey_wgt_col, ea_col, res_col, item_code_col){
  # Filter corn items
  corn_quantity <- data %>%
    filter({{item_code_col}} %in% corn_codes)
  
  # Survey design
  corn_svy_design <- corn_quantity %>%
    as_survey_design(ids = {{ea_col}}, strata = {{res_col}}, weights = {{survey_wgt_col}}, nest = T)
  
  # Calculate survey-weighted mean corn consumption
  intake_corn <- corn_svy_design %>%
    group_by({{adm1_col}}) %>%
    summarise(mean_corn_g = survey_mean({{quantity_col}})) %>%
    select(-mean_corn_g_se)
  
  return(intake_corn)
}

sen_food_cons <- process_food_consumption2("sen_ehcvm2122_hh_info.csv",
                                           "sen_food_consumption.csv")

# Example usage:
sen_reach_corn <- calculate_corn_reach(
  data = sen_food_cons,
  corn_codes = c(12),
  hhid_col = hhid,
  adm1_col = adm1,
  survey_wgt_col = survey_wgt,
  ea_col = ea,
  res_col = res,
  item_code_col = item_code
)

# View the result
sen_reach_corn

# Calculate corn intake
sen_intake_corn <- calculate_corn_intake(
  data = sen_food_cons,
  corn_codes = c(12),
  adm1_col = adm1,
  quantity_col = quantity_g,
  survey_wgt_col = survey_wgt,
  ea_col = ea,
  res_col = res,
  item_code_col = item_code
)

# View result
sen_intake_corn


sen_reach_intake <- 
  sen_reach_corn %>% 
  left_join(sen_intake_corn, by = "adm1") %>% 
  mutate(
    across(everything(), ~ifelse(is.na(.), 0, .))
  ) %>% 
  mutate(
    # Create bins for the reach percentage from corn reach
    reach_bins = cut(
      corn_reach_pct, 
      breaks = c(0, 25, 50, 75, 100), 
      include.lowest = TRUE
    ),
    # Create bins for corn intake (mean consumption in grams)
    intake_bins = cut(
      mean_corn_g, 
      breaks = c(-Inf, 75, 149, 300, Inf),
      labels = c("<75",  "75–149",  "150–300",  ">300"),
      include.lowest = TRUE
    ),
    # Convert adm1 to character to match the shapefile's adm1 column
    adm1 = as.character(adm1)
  ) %>% 
  select(adm1, mean_corn_g, reach_bins, intake_bins) %>% 
  left_join(sen_adm1, by = "adm1") %>% 
  st_as_sf()

# create a bi classs
sen_data_corn <- bi_class(sen_reach_intake, x =reach_bins , y = intake_bins, dim = 4 )


# using ggplot and bi_scale, create a bivariate map
bi_map_corn <- ggplot() + 
  geom_sf(data = sen_data_corn, mapping = aes(fill = bi_class), color = NA,show.legend = F)+
  bi_scale_fill(pal = "DkBlue2",dim = 4)+
  bi_theme()+
  geom_sf(data = sen_adm1, fill= NA, color = 'black', lwd = 1) + 
  #geom_sf_text(data = sen_adm1, aes(label = adm1), size = 3, color = 'black', fontface = 'bold') +
  labs(subtitle = "Coverage and Consumption of corn in Senegal", )

# create a df of the breaks for each exis
break_vals <- bi_class_breaks(sen_reach_intake, x =reach_bins , y = intake_bins, dim = 4 )

#create a bivariate legend
legend_corn <- bi_legend(pal = "DkBlue2",
                         dim = 4,
                         xlab = "Higher Reach (%) ",
                         ylab = "Higher Consumption (g) ",
                         size = 8, 
                         breaks = break_vals)

# put legend and map together
sen_corn_bivariate <- ggdraw() +
  draw_plot(bi_map_corn, 0, 0, 1, 1) +
  draw_plot(legend_corn, 0.65, .2, 0.45, 0.2)

sen_corn_bivariate