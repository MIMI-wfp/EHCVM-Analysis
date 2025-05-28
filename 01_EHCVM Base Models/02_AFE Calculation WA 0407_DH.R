################################################################################
######## SCRIPT FOR AFE CALCULATION - WESTERN AFRICA EHCVM 2021 -2022 ##########
################################################################################

# Author: Daniel Hernandez
# Date created: 17-03-2025
# Last edited: 01-04-2025

# In this script, I will extract household information from Western African  
# countries:
# Benin: https://microdata.worldbank.org/index.php/catalog/6272
# Burkina Faso: https://microdata.worldbank.org/index.php/catalog/6277 
# Cote d'Ivoire: https://microdata.worldbank.org/index.php/catalog/6273 
# Guinea Bissau: https://microdata.worldbank.org/index.php/catalog/6274 
# Mali: https://microdata.worldbank.org/index.php/catalog/6275 
# Niger: https://microdata.worldbank.org/index.php/catalog/6276 
# Togo: https://microdata.worldbank.org/index.php/catalog/6279
# Senegal: https://microdata.worldbank.org/index.php/catalog/6278

# INSTALL AND LOAD PACKAGES:

setwd("C:/Users/daniel.hernandezrive/OneDrive - World Food Programme/Documents/WA AFEs")

rq_packages <- c("readr", "tidyverse", "haven", "dplyr", "srvyr", "glue", "ggplot2")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

# Specify rounding settings:
options(scipen = 10, digits = 3)

# List of assumptions made for AFE calculations: 

# 1 AFE = 2291kcal/day as calculated using the FAO/WHO/UNU (2004) equations for a 55kg female

# PAL = 1.76 (active/moderately active lifestyle) - reference: table 5.1 FAO/WHO/UNU (2004)

# Average men's weight = 65kg (Assumed)
# Average women's weight = 55kg (Assumed)

# Average energy cost of lactation = 505kcal for first 6-months of lactation
# 460kcal after 6-months of lactation (Chapter 7 in FAO/WHO/UNU (2004))

# Average total energy cost of a preganancy = 77,100kcal (reference: table 6.3 FAO/WHO/UNU (2004))
# Average length of a pregnancy = 280days
# Therefore average daily energy cost during pregnancy = 275kcal/day
# There is no data in the NPS-5 to determine pregnancy trimester

#-------------------------------------------------------------------------------

# Create function read csv and generate hhid 

read_and_hhid <- function(file_path) {
  df <- read_csv(file_path)  %>%
    mutate(hhid = paste0(grappe, "_", menage))
  
  return(df)
}

# create_hhid <- function(grappe, menage) {
#  as.double(paste0(grappe, sprintf("%02d", menage)))
# }

#-------------------------------------------------------------------------------

# Tidy up datasets for Benin and Burkina Faso 

# Benin

convert_dta_to_csv <- function(input_path, output_path = NULL) {
  # Read the .dta file
  df <- haven::read_dta(input_path)
  
  # Default output path if not specified
  if (is.null(output_path)) {
    output_path <- sub("\\.dta$", ".csv", input_path)
  }
  
  # Write to CSV
  readr::write_csv(df, output_path)
  
  message(glue::glue("✔ Converted {basename(input_path)} to {basename(output_path)}"))
  
  # Return the output path (invisibly)
  invisible(output_path)
}

ben_dsets_list <- c("Data/s00_me_ben2021.dta", "Data/s01_me_ben2021.dta", 
                    "Data/s03_me_ben2021.dta", "Data/ehcvm_welfare_ben2021.dta",
                    "Data/ehcvm_individu_ben2021.dta")

walk(ben_dsets_list, convert_dta_to_csv)

#rename "departement" as "region"
file_path <- "Data/ehcvm_welfare_ben2021.csv"

# Read, rename, and overwrite
df <- read_csv(file_path, show_col_types = FALSE) %>%
  rename(region = departement)

# Overwrite the original CSV
write_csv(welfare_df, file_path)

rm(ben_dsets_list)

# Burkina Faso

bfa_dsets_list <- c("Data/s01_me_bfa2021.csv", "Data/s03_me_bfa2021.csv")

rename_pid <- function(file_path) {
  df <- readr::read_csv(file_path, show_col_types = FALSE)  # Suppresses type guessing warnings
  
  if ("pid" %in% names(df)) {
    df <- df %>%
      dplyr::rename(membres__id = pid)
    
    readr::write_csv(df, file_path, na = "")  # Prevents unwanted NA insertions
    message(glue::glue("✔ Renamed 'pid' to 'membres__id' in: {basename(file_path)}"))
  } else {
    message(glue::glue("ℹ No 'pid' column found in: {basename(file_path)} — no changes made."))
  }
  
  invisible(file_path)
}

walk(bfa_dsets_list, rename_pid)

file_path <- "Data/ehcvm_individu_bfa2021.csv"

# Read, rename, and overwrite
df <- read_csv(file_path, show_col_types = FALSE) %>%
  rename(numind = pid)

# Overwrite the original CSV
write_csv(df, file_path)

# Harmonizing "milieu" with the other datasets
file_path <- "Data/ehcvm_welfare_bfa2021.csv"

# Read, rename, and overwrite
df <- read_csv(file_path, show_col_types = FALSE)

df <- df %>% 
  mutate(milieu = case_when(milieu == "Rural" ~ 2,
                         milieu == "Urbain" ~ 1,
                         TRUE ~ NA_real_))

# Overwrite the original CSV
write_csv(df, file_path)

rm(file_path, df)


rm(bfa_dsets_list)

#-------------------------------------------------------------------------------

# Create a function to generalize and automate the script across the countries

process_country <- function(country_code) {
  # Define file paths using country code
  
  s00_file <- glue("Data/s00_me_{country_code}2021.csv")
  s01_file <- glue("Data/s01_me_{country_code}2021.csv")
  s03_file <- glue("Data/s03_me_{country_code}2021.csv")
  welfare_file <- glue("Data/ehcvm_welfare_{country_code}2021.csv")
  ind_file <- glue("Data/ehcvm_individu_{country_code}2021.csv")
  adm1_list <- glue("Data/adm1_list_{country_code}.csv")
  adm2_list <- glue("Data/adm2_list_{country_code}.csv")
  output_file <- glue("Data/{country_code}_ehcvm2122_hh_info.csv")
  output_plots <- glue("Data/{country_code}_plots.csv")

# READ IN DEMOGRAPHIC DATA:

demographic <- read_and_hhid(ind_file) %>%
  select(hhid, numind, sexe, age, resid) %>% 
  filter(resid == 1) %>%  # only for individuals residing in the household
  select(hhid, numind, sexe, age)

#-------------------------------------------------------------------------------

# IDENTIFY INDIVUDUALS FALLING INTO EACH DEMOGRAPHIC GROUP:

# CHILDREN UNDER 2: 
u2 <- demographic %>% 
  filter(age < 2)

# Get age in months: 
u2_age <- read_and_hhid(s01_file)  %>% 
  rename(numind = membres__id,
         day_birth = s01q03a,
         month_birth = s01q03b,
         year_birth = s01q03c)

ref_date_df <- read_and_hhid(s00_file) %>%
  mutate(
    ref_date = coalesce(s00q25b, s00q24b, s00q23b)
  ) %>%
  select(hhid, ref_date)

u2 <- u2 %>% 
  left_join(u2_age, by= c('hhid', 'numind')) %>%
  left_join(ref_date_df, by = "hhid") %>%
  select(hhid, numind, age, day_birth, month_birth, year_birth, ref_date) %>% 
  mutate(
    birth_date = make_date(year_birth, month_birth, day_birth), 
    age_months = interval(birth_date, ref_date) %/% months(1)
  ) 

rm(u2_age, ref_date_df)


# LACTATING AND PREGNANT WOMEN: 
women_subset <- read_and_hhid(s03_file) %>% 
  rename(numind = membres__id,
         lact_m = s03q47,          # Woman had a baby in the past 12 months
         pregnant = s03q49) %>%    # woman is pregnant at the moment of the interview
  filter(lact_m == 1 | pregnant == 1) # filter twice to optimize performance

women_subset <- demographic %>%
  left_join(women_subset, by = c('hhid', 'numind')) %>%
  select(hhid, numind, sexe, age, lact_m, pregnant) %>% 
  filter(lact_m == 1 | pregnant == 1) 

# DEMOGRAPHIC ALL OTHERS: 
demographic_others <- demographic %>% 
  anti_join(u2, by = c("hhid", "numind")) %>% 
  anti_join(women_subset, by = c("hhid", "numind")) %>% 
  filter(age > 1)
#-------------------------------------------------------------------------------

# ESTIMATE ENERGY REQUIREMENTS AND AFE's FOR THOSE AGED < 24-months:

# Assign energy requirements for different age groups - SOURCE: 
# Book - Complementary feeding of young Children in Developing Countries, 
# Table 10, page 51.
# WHO 1998, edited by Kenneth Brown, Kathryn Dewey, Lindsay Allen

u2 <- u2 %>%
  mutate(TEE = case_when(
    age_months <= 2 ~ 0,   # only breast feeding - no food intake
    age_months >= 3 & age_months <= 5 ~ 76,  # energy from food is 76 kcal per day for 3-5 months of age
    age_months >= 6 & age_months <= 8 ~ 269,  # 269 kcal per day for 6-8 months of age
    age_months >= 9 & age_months <= 11 ~ 451,   # 451 kcal per day for 9-11 months of age
    age_months >= 12 ~ 746, # 746 kcal per day for those aged 12-months - 2years
    is.na(age_months) ~ 746)) # 746 kcal for those without a birth certificate, assuming they can be older

# AFE calculation for children below 2 years old:
afeu2 <- u2 %>%
  mutate(afe = TEE/2291) %>% # 1AFE = 2291kcal
  select(hhid, numind, afe)

#rm(u2)

# ESTIMATING TEE FOR THOSE AGED >2YEARS:

tee_calc <- demographic %>%
  mutate(ind_weight = ifelse(sexe == 1, 65, 55)) %>% # Assumed average weight of men = 65kg
                                                      # Assumed average weight of women = 55kg
  filter(age >= 2) %>%  # Remove under 2's as these have already been calculated above
  mutate(PAL = ifelse(age > 18, 1.76, NA))   # Set a PAL at 1.76 for all over 18's:

# TEE FOR CHILDREN (2-18 years old) (formula from tables 4.5 and 4.6 in Human energy requirements
# Report from FAO/WHO/UNU (2001)):
tee_calc <- tee_calc %>% 
  mutate(TEE = case_when(    sexe == 1 & age == 2 ~ 950,
                             sexe == 1 & age == 3 ~ 1125,
                             sexe == 1 & age == 4 ~ 1250,
                             sexe == 1 & age == 5 ~ 1350,
                             sexe == 1 & age == 6 ~ 1475,
                             sexe == 1 & age == 7 ~ 1575,
                             sexe == 1 & age == 8 ~ 1700,
                             sexe == 1 & age == 9 ~ 1825,
                             sexe == 1 & age == 10 ~ 1975,
                             sexe == 1 & age == 11 ~ 2150,
                             sexe == 1 & age == 12 ~ 2350,
                             sexe == 1 & age == 13 ~ 2550,
                             sexe == 1 & age == 14 ~ 2775,
                             sexe == 1 & age == 15 ~ 3000,
                             sexe == 1 & age == 16 ~ 3175,
                             sexe == 1 & age == 17 ~ 3325,
                             sexe == 1 & age == 18 ~ 3400,
                             sexe == 2 & age == 2 ~ 850,
                             sexe == 2 & age == 3 ~ 1050,
                             sexe == 2 & age == 4 ~ 1150,
                             sexe == 2 & age == 5 ~ 1250,
                             sexe == 2 & age == 6 ~ 1325,
                             sexe == 2 & age == 7 ~ 1425,
                             sexe == 2 & age == 8 ~ 1550,
                             sexe == 2 & age == 9 ~ 1700,
                             sexe == 2 & age == 10 ~ 1850,
                             sexe == 2 & age == 11 ~ 2000,
                             sexe == 2 & age == 12 ~ 2150,
                             sexe == 2 & age == 13 ~ 2275,
                             sexe == 2 & age == 14 ~ 2375,
                             sexe == 2 & age == 15 ~ 2450,
                             sexe == 2 & age > 15 & age <= 18 ~ 2500))

# TEE FOR ADULTS (Formula from table 5.2 in FAO/WHO/UNU (2004)):
tee_calc <- tee_calc %>% 
  mutate(BMR = case_when( # Firstly need to calculate BMR for different age categories:
    sexe == 1 & age >18 & age <= 30 ~ 15.057 * ind_weight + 692.2,
    sexe == 1 & age >30 & age < 60 ~ 11.472 * ind_weight + 873.1,
    sexe == 1 & age >= 60 ~ 11.711 * ind_weight + 587.7,
    sexe == 2 & age >18 & age <= 30 ~ 14.818 * ind_weight + 486.6,
    sexe == 2 & age >30 & age < 60 ~ 8.126 * ind_weight + 845.6, 
    sexe == 2 & age >= 60 ~ 9.082 * ind_weight + 658.5,
    TRUE ~ NA)) %>% # Get TEE by multiplying BMR by PAL for over 18's: 
  mutate(TEE = ifelse(age > 18, BMR * PAL, TEE)) # 

# x <- as.data.frame(table(tee_calc$TEE, useNA = "ifany"))  checking distribution of possible values

#-------------------------------------------------------------------------------

# ENERGY REQUIREMENTS FOR PREGNANT WOMEN: 

afe_preg <- women_subset %>%
  filter(pregnant == 1) %>% # the  women that are pregnant and have had a baby 
                            # in the past 12 months will be counted as pregnant
  left_join(tee_calc %>% select(hhid, numind, TEE),
            by = c("hhid", "numind")) %>% 
  select(-ends_with(".y")) %>%
  rename_with(~ sub("\\.x$", "", .x), ends_with(".x")) %>%
  mutate(TEE = TEE + 275) %>% # Usual energy requirements +275 kcal/day: 
  mutate(afe = TEE / 2291) %>%  # AFE = Total energy expenditure / 2291 kcal/day
  select(hhid, numind, afe)

# ENERGY REQUIREMENT FOR LACTATING WOMEN:

afe_lact <- women_subset %>%
  filter(lact_m == 1 & pregnant == 2) %>%
  left_join(tee_calc %>% select(hhid, numind, TEE),
          by = c("hhid", "numind")) %>% 
  select(-ends_with(".y")) %>%
  rename_with(~ sub("\\.x$", "", .x), ends_with(".x")) %>%
  mutate (TEE = TEE + 483) %>% 
  mutate(afe = TEE / 2291) %>% # AFE = Total energy expenditure / 2291kcal/day
  select(hhid, numind, afe)

# rm(women_subset)

#-------------------------------------------------------------------------------

# CALCULATE AFE FOR ALL OTHER INDIVIDUALS: 
afe_other <- demographic_others %>% 
  left_join(tee_calc %>% select(hhid, numind, TEE),
            by = c("hhid", "numind")) %>% 
#  select(-ends_with(".y"), -resid) %>%
#  rename_with(~ sub("\\.x$", "", .x), ends_with(".x")) %>%
  # Calculate AFE:
  mutate(afe = TEE / 2291) %>%  # AFE = Total energy expenditure / 2291kcal/day
  select(hhid, numind, afe)

#-------------------------------------------------------------------------------

# CALCULATE TOTAL AFE PER HOUSEHOLD: 

hh_afe <- bind_rows(afeu2, afe_lact, afe_preg, afe_other) %>% 
  group_by(hhid) %>% 
  summarise(afe = sum(afe, na.rm = TRUE))


################################################################################
################### SCRIPT FOR EXTRACTING HOUSEHOLD INFORMATION ################
################################################################################

hh_info <- read_and_hhid(welfare_file) 
adm2_date <- read_and_hhid(s00_file)
adm1_list_ds <- read_csv(adm1_list)
adm2_list_ds <- read_csv(adm2_list)

#-------------------------------------------------------------------------------

# SELECT RELEVANT HOUSEHOLD INFO DATA: 

hh_info <- hh_info %>% 
  select(hhid, hhweight, region, grappe, milieu, hgender, hage, heduc, dtot, # for Benin, it's not region but department
         eqadu1,eqadu2, hhsize) %>% 
  rename(adm1 = region,
         survey_wgt = hhweight,
         res = milieu, 
         sex_head = hgender,
         age_head = hage,
         educ_head = heduc,
         pc_expenditure = dtot,
         ea = grappe) %>% 
  mutate(res = case_when(res == 2 ~ "Rural",
                         res == 1 ~ "Urban",
                         TRUE ~ NA_character_),
         adm1 = adm1_list_ds$category[match(adm1, adm1_list_ds$value)],
# The adm1_list data frames for each country were created from the World Bank's 
# repository, by printing the statistical summary for s00q01 on pdf and asking 
# ChatGPT to create csv files with two columns: one for the values and another 
# for the categories. These outputs from ChatGPT had to be checked and minor
# errors were corrected
         sex_head = case_when(sex_head == 2 ~ "Female",
                         sex_head == 1 ~ "Male",
                         TRUE ~ NA_character_),
         educ_head = case_when(educ_head == 1 ~ "None",
                          educ_head == 2 ~ "Kindergarden",
                          educ_head == 3 ~ "Elementary",
                          educ_head == 4 ~ "Secondary, 1st Level - General",
                          educ_head == 5 ~ "Secondary, 1st Level - Technical",
                          educ_head == 6 ~ "Secondary, 2nd Level - General",
                          educ_head == 7 ~ "Secondary, 2nd Level - Technical",
                          educ_head == 8 ~ "Some Post-Secondary",
                          educ_head == 9 ~ "Higher Education",
                          TRUE ~ NA_character_)) #,

adm2_date <- adm2_date %>% 
  select(hhid, s00q02, s00q25b, s00q24b, s00q23b) %>%
  rename(adm2 = s00q02) %>%   
  mutate(adm2 = adm2_list_ds$category[match(adm2, adm2_list_ds$value)],
         int_date = coalesce(s00q25b, s00q24b, s00q23b),
         month = month(int_date),
         year = year(int_date)
         )

# The adm2_list data frames for each country were created from the World Bank's 
# repository, by printing the statistical summary for s00q02 on pdf and asking 
# ChatGPT to create csv files based on this print with two columns: one for the
# values and another for the categories. These outputs from ChatGPT had to be 
# checked and minor errors were corrected

#-------------------------------------------------------------------------------

# CALCULATE CONSUMPTION QUINTILES: 

# Firstly extract total consumption (spatially and temporally adjusted): 
hh_consumption <- hh_info %>% 
  select(hhid, survey_wgt, res, pc_expenditure)

# Create tbl_svy object: 
svy_hh_consumption <- hh_consumption %>% 
  as_survey_design(weights = survey_wgt)

# Calculate consumption quintiles cut-points: 
consumption_quantiles <- svy_hh_consumption %>% 
  summarise(consumption = survey_quantile(pc_expenditure, c(0.2, 0.4, 0.6, 0.8)))

urban_quantiles <- svy_hh_consumption %>% 
  filter(res == "Urban") %>%
  summarise(consumption = survey_quantile(pc_expenditure, c(0.2, 0.4, 0.6, 0.8)))

rural_quantiles <- svy_hh_consumption %>% 
  filter(res == "Rural") %>%
  summarise(consumption = survey_quantile(pc_expenditure, c(0.2, 0.4, 0.6, 0.8)))

# Apply cut-points to data: 
hh_consumption <- hh_consumption %>% 
  mutate(sep_quintile = case_when(pc_expenditure < consumption_quantiles$consumption_q20 ~ 1,
                                  pc_expenditure >= consumption_quantiles$consumption_q20 & 
                                    pc_expenditure < consumption_quantiles$consumption_q40 ~ 2,
                                  pc_expenditure >= consumption_quantiles$consumption_q40 & 
                                    pc_expenditure < consumption_quantiles$consumption_q60 ~ 3,
                                  pc_expenditure >= consumption_quantiles$consumption_q60 & 
                                    pc_expenditure < consumption_quantiles$consumption_q80 ~ 4,
                                  pc_expenditure >= consumption_quantiles$consumption_q80 ~ 5,
                                  TRUE ~ NA_real_)) %>% 
  mutate(res_quintile = case_when(res == "Urban" & 
                                    pc_expenditure < urban_quantiles$consumption_q20 ~ 1,
                                  res == "Urban" & pc_expenditure >= urban_quantiles$consumption_q20 & 
                                    pc_expenditure < urban_quantiles$consumption_q40 ~ 2,
                                  res == "Urban" & 
                                    pc_expenditure >= urban_quantiles$consumption_q40 & 
                                    pc_expenditure < urban_quantiles$consumption_q60 ~ 3,
                                  res == "Urban" & 
                                    pc_expenditure >= urban_quantiles$consumption_q60 & 
                                    pc_expenditure < urban_quantiles$consumption_q80 ~ 4,
                                  res == "Urban" & 
                                    pc_expenditure >= urban_quantiles$consumption_q80 ~ 5,
                                  res == "Rural" & 
                                    pc_expenditure < rural_quantiles$consumption_q20 ~ 1,
                                  res == "Rural" & 
                                    pc_expenditure >= rural_quantiles$consumption_q20 & 
                                    pc_expenditure < rural_quantiles$consumption_q40 ~ 2,
                                  res == "Rural" & 
                                    pc_expenditure >= rural_quantiles$consumption_q40 & 
                                    pc_expenditure < rural_quantiles$consumption_q60 ~ 3,
                                  res == "Rural" & 
                                    pc_expenditure >= rural_quantiles$consumption_q60 & 
                                    pc_expenditure < rural_quantiles$consumption_q80 ~ 4,
                                  res == "Rural" & 
                                    pc_expenditure >= rural_quantiles$consumption_q80 ~ 5,
                                  TRUE ~ NA_real_))

rm(consumption_quantiles, urban_quantiles, rural_quantiles, svy_hh_consumption)

#-------------------------------------------------------------------------------

# Join all relevant variables to hh_info: 
hh_info <- hh_info %>% 
  left_join(hh_afe, by = "hhid") %>% 
  left_join(adm2_date, by = "hhid") %>% 
  left_join(hh_consumption %>% 
              dplyr::select(hhid, sep_quintile, res_quintile),
            by = "hhid") %>% 
#  select(hhid, afe, eqadu1,eqadu2, hhsize, ea) # for plotting
  select(hhid, adm1, adm2, res, pc_expenditure, sep_quintile, res_quintile, 
         year, month, survey_wgt, afe, ea) # final version of dataset - as shared by UA on April 1st

#  select(hhid, adm1, adm2, res, pc_expenditure, sep_quintile, res_quintile, 
#         age_head, sex_head, educ_head, year, month, survey_wgt, afe, ea)

# hh_info <- hh_info %>% # final version of dataset - based on Tanzania during onboarding
#   select(hhid, adm1, adm2, res, pc_expenditure, sep_quintile, res_quintile, 
#          age_head, sex_head, educ_head, year, month, survey_wgt, afe, ea)

#-------------------------------------------------------------------------------
write_csv(hh_info, output_file)
return(list(
  hh_info = hh_info,
  u2 = u2,
  women_subset = women_subset,
  demographic_others = demographic_others
))

}

country_list <- c("ben", "civ", "gnb", "mli", "ner", "sen", "tgo") # excluding bfa and mli

results <- purrr::map(country_list, ~ try(process_country(.x), silent = TRUE))

names(results) <- country_list

# walk(country_list, process_country) # faster results
#-------------------------------------------------------------------------------
# Quality checks

summary_tables <- purrr::imap(results, function(res, country_name) {
  if (is.null(res)) return(NULL)  # Skip failed runs
  
  hh_info <- res$hh_info
  
  # For each variable in hh_info, create a table (with NAs)
  var_tables <- purrr::map(names(hh_info), function(var) {
    tbl <- table(hh_info[[var]], useNA = "ifany")
    return(tbl)
  })
  
  names(var_tables) <- names(hh_info)  # Label each table by variable name
  return(var_tables)
})

names(summary_tables$ben)

saveRDS(summary_tables, file = "summary_tables0319.rds")

saveRDS(results, file = "results0319.rds")



# Write data
write_csv(hh_info, "Data/sen_ehcvm2122_hh_info.csv")

rm(list = ls())

#-------------------------------------------------------------------------------
# Jitter plots
plot_afe_vs_equiv <- function(country_list, data_dir = "Data", output_dir = "Plots") {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  for (country in country_list) {
    file_path <- file.path(data_dir, glue::glue("{country}_ehcvm2122_hh_info.csv"))
    
    if (!file.exists(file_path)) {
      warning(glue::glue("File not found: {file_path}"))
      next
    }
    
    hh_info_plots <- readr::read_csv(file_path, show_col_types = FALSE)
    
    # Plot 1: AFE vs Household Size (with jitter and smooth line)
    p1 <- ggplot(hh_info_plots, aes(x = hhsize, y = afe)) +
      geom_jitter(width = 0.25, height = 0.25, alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      geom_smooth(method = "loess", se = FALSE, color = "blue") +
      labs(
        title = glue::glue("[{country}] AFE vs Household Size"),
        x = "Household Size",
        y = "AFE"
      ) +
      theme_minimal(base_family = "", base_size = 11) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    # Plot 2: AFE vs Household Size (with jitter)
    p2 <- ggplot(hh_info_plots, aes(x = hhsize, y = afe)) +
      geom_jitter(width = 0.25, height = 0.25, alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      labs(
        title = glue::glue("[{country}] AFE vs Household Size"),
        x = "Household Size",
        y = "AFE"
      ) +
      theme_minimal(base_family = "", base_size = 11) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    # Plot 3: AFE vs Adult Equivalent 1
    p3 <- ggplot(hh_info_plots, aes(x = eqadu1, y = afe)) +
      geom_jitter(width = 0.25, height = 0.25, alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "solid") +
      geom_smooth(method = "loess", se = FALSE, color = "blue") +
      labs(
        title = glue::glue("[{country}] AFE vs FAO Adult Equivalent 1"),
        x = "Adult Equivalent 1",
        y = "AFE"
      ) +
      theme_minimal(base_family = "", base_size = 11) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    # Plot 4: AFE vs Adult Equivalent 2
    p4 <- ggplot(hh_info_plots, aes(x = eqadu2, y = afe)) +
      geom_jitter(width = 0.25, height = 0.25, alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "solid") +
      geom_smooth(method = "loess", se = FALSE, color = "blue") +
      labs(
        title = glue::glue("[{country}] AFE vs FAO Adult Equivalent 2"),
        x = "Adult Equivalent 2",
        y = "AFE"
      ) +
      theme_minimal(base_family = "", base_size = 11) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    # Save plots to output folder
    ggsave(filename = glue::glue("{output_dir}/{country}_afe_vs_hhsize_2.png"), plot = p1, width = 6, height = 4)
    ggsave(filename = glue::glue("{output_dir}/{country}_afe_vs_hhsize_1.png"), plot = p2, width = 6, height = 4)
    ggsave(filename = glue::glue("{output_dir}/{country}_afe_vs_eqadu1.png"), plot = p3, width = 6, height = 4)
    ggsave(filename = glue::glue("{output_dir}/{country}_afe_vs_eqadu2.png"), plot = p4, width = 6, height = 4)
    
    message(glue::glue("✅ Plots saved for {country}"))
  }
}


map(country_list, ~ try(plot_afe_vs_equiv(.x), silent = TRUE))


################################################################################
################################ END OF SCRIPT #################################
################################################################################