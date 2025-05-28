# Author: Uchenna Agu
# Date:


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
#                cowplot,
#                biscale)

# Desription: Function fir creating the base ai (applies the AFEs to FCQ and FCT plus outlier handling)
process_food_consumption <- function(hh_info_path, cons_path, fct_path) {
  
  # Read Household Information and Extract AFE
  hh_info <- read.csv(hh_info_path) %>%
    select(hhid, afe)
  
  # Read Food Consumption Data
  cons_data <- read.csv(cons_path) %>%
    left_join(hh_info, by = "hhid") %>%
    mutate(quantity_100g = quantity_100g / afe) %>%
    select(-afe, -quantity_g)
  
  # Handle Outliers in Log Space
  cons_data <- cons_data %>%
    mutate(log_quantity_100g = log(quantity_100g)) %>%
    group_by(item_code) %>%
    mutate(
      median_q = median(log_quantity_100g, na.rm = TRUE),
      sd_q = sd(log_quantity_100g, na.rm = TRUE),
      log_quantity_100g = ifelse(log_quantity_100g > median_q + 4 * sd_q, median_q, log_quantity_100g)
    ) %>%
    ungroup() %>%
    select(-median_q, -sd_q) %>% 
    mutate(quantity_100g = exp(log_quantity_100g)) %>%
    select(-log_quantity_100g)
  
  # Read Food Composition Table (FCT)
  fct_data <- read.csv(fct_path) 
  
  # Merge with Food Composition Table
  cons_fct_data <- merge(cons_data, fct_data, by = "item_code")
  
  # Calculate Nutrient Contributions
  cons_fct_data <- cons_fct_data %>%
    mutate(across(energy_kcal:zn_mg, ~ . * quantity_100g))
  
  # Aggregate at Household Level (Fix applied here)
  cons_fct_totals <- cons_fct_data %>%
    group_by(hhid) %>%
    summarise(across(energy_kcal:zn_mg, \(x) sum(x, na.rm = TRUE)))
  
  return(cons_fct_totals)
}

# Define the EAR reference data for targets creation
allen_ear <- data.frame(
  nutrient = c(
    "energy_kcal",
    "vita_rae_mcg",
    "thia_mg",
    "ribo_mg",
    "niac_mg",
    "vitb6_mg",
    "vitd_mcg",
    "folate_mcg",
    "vitb12_mcg",
    "fe_mg",
    "zn_mg"
  ),
  ear_value = c(
    2100,  # WHO
    490,
    0.9,
    1.3,
    11,
    1.3,
    10,
    250,
    2,
    22.4, # Low absorption
    10.2  # Unrefined
  )
)

# Create a named reference vector from allen_ear
reference_values <- setNames(allen_ear$ear_value, allen_ear$nutrient)

# Define function to apply nutrient deficiency flag
apply_nutrient_deficiency_flag <- function(data) {
  data %>%
    mutate(across(.cols = -hhid, ~ ifelse(. < reference_values[cur_column()], 1, 0)))
}


# Function for FE full probability
fe_full_prob <- function(data, group1 = NULL, group2 = NULL, bio_avail = 10, hh_weight = NULL) {
  
  # Compute probability of inadequacy based on iron intake
  data <- data %>%
    mutate(prob_inad = case_when(
      bio_avail == 5 ~ case_when(
        fe_mg <= 15 ~ "1",
        fe_mg <= 16.7 & fe_mg > 15 ~ "0.96",
        fe_mg <= 18.7 & fe_mg > 16.7 ~ "0.93",
        fe_mg <= 21.4 & fe_mg > 18.7 ~ "0.85",
        fe_mg <= 23.6 & fe_mg > 21.4 ~ "0.75",
        fe_mg <= 25.7 & fe_mg > 23.6 ~ "0.65",
        fe_mg <= 27.8 & fe_mg > 25.7 ~ "0.55",
        fe_mg <= 30.2 & fe_mg > 27.8 ~ "0.45",
        fe_mg <= 33.2 & fe_mg > 30.2 ~ "0.35",
        fe_mg <= 37.3 & fe_mg > 33.2 ~ "0.25",
        fe_mg <= 45.0 & fe_mg > 37.3 ~ "0.15",
        fe_mg <= 53.5 & fe_mg > 45.0 ~ "0.08",
        fe_mg <= 63.0 & fe_mg > 53.5 ~ "0.04",
        fe_mg > 63 ~ "0"),
      bio_avail == 10 ~ case_when(
        fe_mg <= 7.5 ~ "1",
        fe_mg <= 8.4 & fe_mg > 7.5 ~ "0.96",
        fe_mg <= 9.4 & fe_mg > 8.4 ~ "0.93",
        fe_mg <= 10.7 & fe_mg > 9.4 ~ "0.85",
        fe_mg <= 11.8 & fe_mg > 10.7 ~ "0.75",
        fe_mg <= 12.9 & fe_mg > 11.8 ~ "0.65",
        fe_mg <= 13.9 & fe_mg > 12.9 ~ "0.55",
        fe_mg <= 15.1 & fe_mg > 13.9 ~ "0.45",
        fe_mg <= 16.6 & fe_mg > 15.1 ~ "0.35",
        fe_mg <= 18.7 & fe_mg > 16.6 ~ "0.25",
        fe_mg <= 22.5 & fe_mg > 18.7 ~ "0.15",
        fe_mg <= 26.7 & fe_mg > 22.5 ~ "0.08",
        fe_mg <= 31.5 & fe_mg > 26.7 ~ "0.04",
        fe_mg > 31.5 ~ "0"),
      bio_avail == 15 ~ case_when(
        fe_mg <= 5 ~ "1",
        fe_mg <= 5.6 & fe_mg > 5 ~ "0.96",
        fe_mg <= 6.2 & fe_mg > 5.6 ~ "0.93",
        fe_mg <= 7.1 & fe_mg > 6.2 ~ "0.85",
        fe_mg <= 7.9 & fe_mg > 7.1 ~ "0.75",
        fe_mg <= 8.6 & fe_mg > 7.9 ~ "0.65",
        fe_mg <= 9.3 & fe_mg > 8.6 ~ "0.55",
        fe_mg <= 10.1 & fe_mg > 9.3 ~ "0.45",
        fe_mg <= 11.1 & fe_mg > 10.1 ~ "0.35",
        fe_mg <= 12.4 & fe_mg > 11.1 ~ "0.25",
        fe_mg <= 15.0 & fe_mg > 12.4 ~ "0.15",
        fe_mg <= 17.8 & fe_mg > 15.0 ~ "0.08",
        fe_mg <= 21.0 & fe_mg > 17.8 ~ "0.04",
        fe_mg > 21.0 ~ "0")
    ))
  
  # Assign weight column 
  if (!is.null(hh_weight) && hh_weight %in% colnames(data)) {
    data <- data %>% rename(weight = all_of(hh_weight))
  } else {
    data <- data %>% mutate(weight = 1)
  }
  
  # Compute prevalence of iron inadequacy
  if (missing(group1) & missing(group2)) {
    result <- data %>%
      group_by(prob_inad) %>%
      summarise(fe_weighted = sum(weight), .groups = "drop") %>%
      summarise(prev_inad = sum(fe_weighted * as.numeric(prob_inad)) / sum(fe_weighted) * 100) %>%
      pivot_longer(cols = everything(), names_to = "subpopulation", values_to = "prev_inad")
  } else {
    result <- data %>%
      group_by(prob_inad, {{group1}}, {{group2}}) %>%
      summarise(fe_weighted = sum(weight), .groups = "drop") %>%
      pivot_wider(names_from = {{group1}}, values_from = fe_weighted, values_fill = 0) %>%
      rename_with(~ gsub("^prev_inad_", "", .x)) %>%  # Remove prefix to maintain original names
      summarise(across(-prob_inad, ~ sum(.x * as.numeric(prob_inad)) / sum(.x) * 100, .names = "{.col}")) %>%
      pivot_longer(cols = everything(), names_to = "subpopulation", values_to = "fe_mg_prop")
  }
  
  return(result)
}


#### Base ai creation 

# ----------------- Apply to the different countries ---------------------------
sen_base_ai <- process_food_consumption("sen_ehcvm2122_hh_info.csv", 
                                        "sen_food_consumption.csv", 
                                        "sen_ehcvm2122_fct.csv")

ner_base_ai <- process_food_consumption("ner_ehcvm2122_hh_info.csv", 
                                        "ner_food_consumption.csv", 
                                        "bfa_ehcvm2122_fct.csv")

tgo_base_ai <- process_food_consumption("tgo_ehcvm2122_hh_info.csv", 
                                        "tgo_food_consumption.csv", 
                                        "sen_ehcvm2122_fct.csv")

bfa_base_ai <- process_food_consumption("bfa_ehcvm2122_hh_info.csv", 
                                        "bfa_food_consumption.csv", 
                                        "bfa_ehcvm2122_fct.csv")

mli_base_ai <- process_food_consumption("mli_ehcvm2122_hh_info.csv", 
                                        "mli_food_consumption.csv", 
                                        "bfa_ehcvm2122_fct.csv")

gnb_base_ai <- process_food_consumption("gnb_ehcvm2122_hh_info.csv", 
                                        "gnb_food_consumption.csv", 
                                        "sen_ehcvm2122_fct.csv")

ben_base_ai <- process_food_consumption("ben_ehcvm2122_hh_info.csv",
                                        "ben_food_consumption.csv",
                                        "sen_ehcvm2122_fct.csv")

civ_base_ai <- process_food_consumption("civ_ehcvm2122_hh_info.csv",
                                        "civ_food_consumption.csv",
                                        "sen_ehcvm2122_fct.csv")

nga_base_ai <- process_food_consumption("nga_lss1819_hh_info.csv",
                                        "nga_food_consumption.csv",
                                        "nga_lss1819_fct.csv")

gha_base_ai <- process_food_consumption("gha_glss17_hh_info.csv",
                                        "gha_food_consumption.csv",
                                        "gha_glss17_fct.csv")


