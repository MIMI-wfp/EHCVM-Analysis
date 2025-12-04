# Author: Uchenna Agu
# Creates adm1 and adm2 level weighted proportions for each country

#-----------------------------------------------------------------------------------------------------------
sen_hh_info <- read.csv("sen_ehcvm2122_hh_info.csv")

sen_hh <- sen_hh_info %>%
  select(hhid, adm1, adm2, survey_wgt)

sen_base_ai_fe <- sen_base_ai %>%
  left_join(sen_hh, by = "hhid") %>%
  select(hhid, adm1, adm2, survey_wgt, fe_mg)


sen_fe_inadequacy_adm1 <- fe_full_prob(sen_base_ai_fe, group1 = adm1, bio_avail = 10, hh_weight = "hh_weight")

sen_fe_inadequacy_adm2 <- fe_full_prob(sen_base_ai_fe, group1 = adm2, bio_avail = 10, hh_weight = "hh_weight")

sen_targets <- apply_nutrient_deficiency_flag(sen_base_ai)

sen_target_hh_info <- sen_targets %>%
  left_join(sen_hh_info, by="hhid") %>%
  select(-pc_expenditure, -sep_quintile, -res_quintile, -year, -month, -afe, -fe_mg)

# Convert the dataset into a survey design
sen_survey <- sen_target_hh_info %>%
  as_survey_design(
    ids = ea,            # Clusters (PSU)
    strata = res,        # Stratification variable
    weights = survey_wgt # Household survey weights
  )

sen_inadq_proportion_adm1 <- sen_survey %>%
  group_by(adm1) %>%
  summarise(across(
    .cols = c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg, 
              vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, zn_mg),
    .fns = ~ survey_mean(. == 1, na.rm = TRUE, vartype = NULL) * 100, 
    .names = "{.col}_prop"
  )) %>%
  left_join(sen_fe_inadequacy_adm1, by = c("adm1"="subpopulation"))


sen_inadq_proportion_adm2 <- sen_survey %>%
  group_by(adm2) %>%
  summarise(across(
    .cols = c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg, 
              vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, zn_mg),
    .fns = ~ survey_mean(. == 1, na.rm = TRUE, vartype = NULL) * 100, 
    .names = "{.col}_prop"
  )) %>%
  left_join(sen_fe_inadequacy_adm2, by = c("adm2"="subpopulation"))


#-------------------------------------------------------civ--------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------
civ_hh_info <- read.csv("civ_ehcvm2122_hh_info.csv")

civ_hh <- civ_hh_info %>%
  select(hhid, adm1, adm2, survey_wgt)

civ_base_ai_fe <- civ_base_ai %>%
  left_join(civ_hh, by = "hhid") %>%
  select(hhid, adm1, adm2, survey_wgt, fe_mg)


civ_fe_inadequacy_adm1 <- fe_full_prob(civ_base_ai_fe, group1 = adm1, bio_avail = 10, hh_weight = "hh_weight")

civ_fe_inadequacy_adm2 <- fe_full_prob(civ_base_ai_fe, group1 = adm2, bio_avail = 10, hh_weight = "hh_weight")

civ_targets <- apply_nutrient_deficiency_flag(civ_base_ai)

civ_target_hh_info <- civ_targets %>%
  left_join(civ_hh_info, by="hhid") %>%
  select(-pc_expenditure, -sep_quintile, -res_quintile, -year, -month, -afe, -fe_mg)

# Convert the dataset into a survey design
civ_survey <- civ_target_hh_info %>%
  as_survey_design(
    ids = ea,            # Clusters (PSU)
    strata = res,        # Stratification variable
    weights = survey_wgt # Household survey weights
  )

civ_inadq_proportion_adm1 <- civ_survey %>%
  group_by(adm1) %>%
  summarise(across(
    .cols = c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg, 
              vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, zn_mg),
    .fns = ~ survey_mean(. == 1, na.rm = TRUE, vartype = NULL) * 100, 
    .names = "{.col}_prop"
  )) %>%
  left_join(civ_fe_inadequacy_adm1, by = c("adm1"="subpopulation"))


civ_inadq_proportion_adm2 <- civ_survey %>%
  group_by(adm2) %>%
  summarise(across(
    .cols = c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg, 
              vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, zn_mg),
    .fns = ~ survey_mean(. == 1, na.rm = TRUE, vartype = NULL) * 100, 
    .names = "{.col}_prop"
  )) %>%
  left_join(civ_fe_inadequacy_adm2, by = c("adm2"="subpopulation"))




