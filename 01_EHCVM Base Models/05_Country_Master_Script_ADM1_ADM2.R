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


#-------------------------------------------------------bfa--------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------
bfa_hh_info <- read.csv("bfa_ehcvm2122_hh_info.csv")

bfa_hh <- bfa_hh_info %>%
  select(hhid, adm1, adm2, survey_wgt)

bfa_base_ai_fe <- bfa_base_ai %>%
  left_join(bfa_hh, by = "hhid") %>%
  select(hhid, adm1, adm2, survey_wgt, fe_mg)


bfa_fe_inadequacy_adm1 <- fe_full_prob(bfa_base_ai_fe, group1 = adm1, bio_avail = 10, hh_weight = "hh_weight")

bfa_fe_inadequacy_adm2 <- fe_full_prob(bfa_base_ai_fe, group1 = adm2, bio_avail = 10, hh_weight = "hh_weight")

bfa_targets <- apply_nutrient_deficiency_flag(bfa_base_ai)

bfa_target_hh_info <- bfa_targets %>%
  left_join(bfa_hh_info, by="hhid") %>%
  select(-pc_expenditure, -sep_quintile, -res_quintile, -year, -month, -afe, -fe_mg)

# Convert the dataset into a survey design
bfa_survey <- bfa_target_hh_info %>%
  as_survey_design(
    ids = ea,            # Clusters (PSU)
    strata = res,        # Stratification variable
    weights = survey_wgt,
    nest = T # Household survey weights
  )

bfa_inadq_proportion_adm1 <- bfa_survey %>%
  group_by(adm1) %>%
  summarise(across(
    .cols = c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg, 
              vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, zn_mg),
    .fns = ~ survey_mean(. == 1, na.rm = TRUE, vartype = NULL) * 100, 
    .names = "{.col}_prop"
  )) %>%
  left_join(bfa_fe_inadequacy_adm1, by = c("adm1"="subpopulation"))


bfa_inadq_proportion_adm2 <- bfa_survey %>%
  group_by(adm2) %>%
  summarise(across(
    .cols = c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg, 
              vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, zn_mg),
    .fns = ~ survey_mean(. == 1, na.rm = TRUE, vartype = NULL) * 100, 
    .names = "{.col}_prop"
  )) %>%
  left_join(bfa_fe_inadequacy_adm2, by = c("adm2"="subpopulation"))


#-------------------------------------------------------mli--------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------
mli_hh_info <- read.csv("mli_ehcvm2122_hh_info.csv")

mli_hh <- mli_hh_info %>%
  select(hhid, adm1, adm2, survey_wgt)

mli_base_ai_fe <- mli_base_ai %>%
  left_join(mli_hh, by = "hhid") %>%
  select(hhid, adm1, adm2, survey_wgt, fe_mg)


mli_fe_inadequacy_adm1 <- fe_full_prob(mli_base_ai_fe, group1 = adm1, bio_avail = 10, hh_weight = "hh_weight")

mli_fe_inadequacy_adm2 <- fe_full_prob(mli_base_ai_fe, group1 = adm2, bio_avail = 10, hh_weight = "hh_weight")

mli_targets <- apply_nutrient_deficiency_flag(mli_base_ai)

mli_target_hh_info <- mli_targets %>%
  left_join(mli_hh_info, by="hhid") %>%
  select(-pc_expenditure, -sep_quintile, -res_quintile, -year, -month, -afe, -fe_mg)

# Convert the dataset into a survey design
mli_survey <- mli_target_hh_info %>%
  as_survey_design(
    ids = ea,            # Clusters (PSU)
    strata = res,        # Stratification variable
    weights = survey_wgt # Household survey weights
  )

mli_inadq_proportion_adm1 <- mli_survey %>%
  group_by(adm1) %>%
  summarise(across(
    .cols = c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg, 
              vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, zn_mg),
    .fns = ~ survey_mean(. == 1, na.rm = TRUE, vartype = NULL) * 100, 
    .names = "{.col}_prop"
  )) %>%
  left_join(mli_fe_inadequacy_adm1, by = c("adm1"="subpopulation"))


mli_inadq_proportion_adm2 <- mli_survey %>%
  group_by(adm2) %>%
  summarise(across(
    .cols = c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg, 
              vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, zn_mg),
    .fns = ~ survey_mean(. == 1, na.rm = TRUE, vartype = NULL) * 100, 
    .names = "{.col}_prop"
  )) %>%
  left_join(mli_fe_inadequacy_adm2, by = c("adm2"="subpopulation"))


#-------------------------------------------------------ben--------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------
ben_hh_info <- read.csv("ben_ehcvm2122_hh_info.csv")

ben_hh <- ben_hh_info %>%
  select(hhid, adm1, adm2, survey_wgt)

ben_base_ai_fe <- ben_base_ai %>%
  left_join(ben_hh, by = "hhid") %>%
  select(hhid, adm1, adm2, survey_wgt, fe_mg)


ben_fe_inadequacy_adm1 <- fe_full_prob(ben_base_ai_fe, group1 = adm1, bio_avail = 10, hh_weight = "hh_weight")

ben_fe_inadequacy_adm2 <- fe_full_prob(ben_base_ai_fe, group1 = adm2, bio_avail = 10, hh_weight = "hh_weight")

ben_targets <- apply_nutrient_deficiency_flag(ben_base_ai)

ben_target_hh_info <- ben_targets %>%
  left_join(ben_hh_info, by="hhid") %>%
  select(-pc_expenditure, -sep_quintile, -res_quintile, -year, -month, -afe, -fe_mg)

# Convert the dataset into a survey design
ben_survey <- ben_target_hh_info %>%
  as_survey_design(
    ids = ea,            # Clusters (PSU)
    strata = res,        # Stratification variable
    weights = survey_wgt # Household survey weights
  )

ben_inadq_proportion_adm1 <- ben_survey %>%
  group_by(adm1) %>%
  summarise(across(
    .cols = c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg, 
              vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, zn_mg),
    .fns = ~ survey_mean(. == 1, na.rm = TRUE, vartype = NULL) * 100, 
    .names = "{.col}_prop"
  )) %>%
  left_join(ben_fe_inadequacy_adm1, by = c("adm1"="subpopulation"))


ben_inadq_proportion_adm2 <- ben_survey %>%
  group_by(adm2) %>%
  summarise(across(
    .cols = c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg, 
              vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, zn_mg),
    .fns = ~ survey_mean(. == 1, na.rm = TRUE, vartype = NULL) * 100, 
    .names = "{.col}_prop"
  )) %>%
  left_join(ben_fe_inadequacy_adm2, by = c("adm2"="subpopulation"))


#-------------------------------------------------------gnb--------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------
gnb_hh_info <- read.csv("gnb_ehcvm2122_hh_info.csv")

gnb_hh <- gnb_hh_info %>%
  select(hhid, adm1, adm2, survey_wgt)

gnb_base_ai_fe <- gnb_base_ai %>%
  left_join(gnb_hh, by = "hhid") %>%
  select(hhid, adm1, adm2, survey_wgt, fe_mg)


gnb_fe_inadequacy_adm1 <- fe_full_prob(gnb_base_ai_fe, group1 = adm1, bio_avail = 10, hh_weight = "hh_weight")

gnb_fe_inadequacy_adm2 <- fe_full_prob(gnb_base_ai_fe, group1 = adm2, bio_avail = 10, hh_weight = "hh_weight")

gnb_targets <- apply_nutrient_deficiency_flag(gnb_base_ai)

gnb_target_hh_info <- gnb_targets %>%
  left_join(gnb_hh_info, by="hhid") %>%
  select(-pc_expenditure, -sep_quintile, -res_quintile, -year, -month, -afe, -fe_mg)

# Convert the dataset into a survey design
gnb_survey <- gnb_target_hh_info %>%
  as_survey_design(
    ids = ea,            # Clusters (PSU)
    strata = res,        # Stratification variable
    weights = survey_wgt # Household survey weights
  )

gnb_inadq_proportion_adm1 <- gnb_survey %>%
  group_by(adm1) %>%
  summarise(across(
    .cols = c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg, 
              vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, zn_mg),
    .fns = ~ survey_mean(. == 1, na.rm = TRUE, vartype = NULL) * 100, 
    .names = "{.col}_prop"
  )) %>%
  left_join(gnb_fe_inadequacy_adm1, by = c("adm1"="subpopulation"))


gnb_inadq_proportion_adm2 <- gnb_survey %>%
  group_by(adm2) %>%
  summarise(across(
    .cols = c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg, 
              vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, zn_mg),
    .fns = ~ survey_mean(. == 1, na.rm = TRUE, vartype = NULL) * 100, 
    .names = "{.col}_prop"
  )) %>%
  left_join(gnb_fe_inadequacy_adm2, by = c("adm2"="subpopulation"))


#-------------------------------------------------------tgo--------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------
tgo_hh_info <- read.csv("tgo_ehcvm2122_hh_info.csv")

tgo_hh <- tgo_hh_info %>%
  select(hhid, adm1, adm2, survey_wgt)

tgo_base_ai_fe <- tgo_base_ai %>%
  left_join(tgo_hh, by = "hhid") %>%
  select(hhid, adm1, adm2, survey_wgt, fe_mg)


tgo_fe_inadequacy_adm1 <- fe_full_prob(tgo_base_ai_fe, group1 = adm1, bio_avail = 10, hh_weight = "hh_weight")

tgo_fe_inadequacy_adm2 <- fe_full_prob(tgo_base_ai_fe, group1 = adm2, bio_avail = 10, hh_weight = "hh_weight")

tgo_targets <- apply_nutrient_deficiency_flag(tgo_base_ai)

tgo_target_hh_info <- tgo_targets %>%
  left_join(tgo_hh_info, by="hhid") %>%
  select(-pc_expenditure, -sep_quintile, -res_quintile, -year, -month, -afe, -fe_mg)

# Convert the dataset into a survey design
tgo_survey <- tgo_target_hh_info %>%
  as_survey_design(
    ids = ea,            # Clusters (PSU)
    strata = res,        # Stratification variable
    weights = survey_wgt # Household survey weights
  )

tgo_inadq_proportion_adm1 <- tgo_survey %>%
  group_by(adm1) %>%
  summarise(across(
    .cols = c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg, 
              vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, zn_mg),
    .fns = ~ survey_mean(. == 1, na.rm = TRUE, vartype = NULL) * 100, 
    .names = "{.col}_prop"
  )) %>%
  left_join(tgo_fe_inadequacy_adm1, by = c("adm1"="subpopulation"))


tgo_inadq_proportion_adm2 <- tgo_survey %>%
  group_by(adm2) %>%
  summarise(across(
    .cols = c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg, 
              vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, zn_mg),
    .fns = ~ survey_mean(. == 1, na.rm = TRUE, vartype = NULL) * 100, 
    .names = "{.col}_prop"
  )) %>%
  left_join(tgo_fe_inadequacy_adm2, by = c("adm2"="subpopulation"))


#-------------------------------------------------------ner--------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------
ner_hh_info <- read.csv("ner_ehcvm2122_hh_info.csv")

ner_hh <- ner_hh_info %>%
  select(hhid, adm1, adm2, survey_wgt)

ner_base_ai_fe <- ner_base_ai %>%
  left_join(ner_hh, by = "hhid") %>%
  select(hhid, adm1, adm2, survey_wgt, fe_mg)


ner_fe_inadequacy_adm1 <- fe_full_prob(ner_base_ai_fe, group1 = adm1, bio_avail = 10, hh_weight = "hh_weight")

ner_fe_inadequacy_adm2 <- fe_full_prob(ner_base_ai_fe, group1 = adm2, bio_avail = 10, hh_weight = "hh_weight")

ner_targets <- apply_nutrient_deficiency_flag(ner_base_ai)

ner_target_hh_info <- ner_targets %>%
  left_join(ner_hh_info, by="hhid") %>%
  select(-pc_expenditure, -sep_quintile, -res_quintile, -year, -month, -afe, -fe_mg)

# Convert the dataset into a survey design
ner_survey <- ner_target_hh_info %>%
  as_survey_design(
    ids = ea,            # Clusters (PSU)
    strata = res,        # Stratification variable
    weights = survey_wgt # Household survey weights
  )

ner_inadq_proportion_adm1 <- ner_survey %>%
  group_by(adm1) %>%
  summarise(across(
    .cols = c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg, 
              vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, zn_mg),
    .fns = ~ survey_mean(. == 1, na.rm = TRUE, vartype = NULL) * 100, 
    .names = "{.col}_prop"
  )) %>%
  left_join(ner_fe_inadequacy_adm1, by = c("adm1"="subpopulation"))


ner_inadq_proportion_adm2 <- ner_survey %>%
  group_by(adm2) %>%
  summarise(across(
    .cols = c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg, 
              vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, zn_mg),
    .fns = ~ survey_mean(. == 1, na.rm = TRUE, vartype = NULL) * 100, 
    .names = "{.col}_prop"
  )) %>%
  left_join(ner_fe_inadequacy_adm2, by = c("adm2"="subpopulation"))


#-------------------------------------------------------nga--------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------
nga_hh_info <- read_excel("nga_lss1819_hh_info.xlsx")

nga_hh <- nga_hh_info %>%
  select(hhid, adm1, adm2, survey_wgt)

nga_base_ai_fe <- nga_base_ai %>%
  left_join(nga_hh, by = "hhid") %>%
  select(hhid, adm1, adm2, survey_wgt, fe_mg)


nga_fe_inadequacy_adm1 <- fe_full_prob(nga_base_ai_fe, group1 = adm1, bio_avail = 10, hh_weight = "hh_weight")

nga_fe_inadequacy_adm2 <- fe_full_prob(nga_base_ai_fe, group1 = adm2, bio_avail = 10, hh_weight = "hh_weight")


nga_targets <- apply_nutrient_deficiency_flag(nga_base_ai)
#-------------------------


nga_target_hh_info <- nga_targets %>%
  left_join(nga_hh_info, by="hhid") %>%
  select(-pc_expenditure, -sep_quintile, -res_quintile, -year, -month, -afe, -fe_mg)

# Convert the dataset into a survey design
nga_survey <- nga_target_hh_info %>%
  as_survey_design(
    ids = ea,            # Clusters (PSU)
    strata = res,        # Stratification variable
    weights = survey_wgt,
    nest = T# Household survey weights
  )


####
nga_hh_info <- read_excel("nga_lss1819_hh_info.xlsx")

hh_loc <- read_excel("household_locations.xlsx")


nga_adm1adm2 <- nga_hh_info %>%
  inner_join(hh_loc, by = "hhid") %>%
  select(adm1, adm2, state, lga) %>%
  mutate(
    state = str_to_title(state),
    lga   = str_to_title(lga)
  )


nga_adm1_state <- nga_adm1adm2 %>%
  select(adm1, state) %>%
  distinct()

nga_adm2_lga <- nga_adm1adm2 %>%
  select(adm2, lga) %>%
  distinct()

####
nga_fe_inadequacy_adm1 <- nga_fe_inadequacy_adm1 %>%
  mutate(subpopulation = as.numeric(subpopulation))

nga_inadq_proportion_adm1 <- nga_survey %>%
  group_by(adm1) %>%
  summarise(across(
    .cols = c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg, 
              vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, zn_mg),
    .fns = ~ survey_mean(. == 1, na.rm = TRUE, vartype = NULL) * 100,
    .names = "{.col}_prop"
  )) %>%
  left_join(nga_fe_inadequacy_adm1, by = c("adm1" = "subpopulation"))

nga_inadq_proportion_adm1 <- nga_inadq_proportion_adm1 %>%
  left_join(nga_adm1_state, by="adm1") %>%
  select(-adm1) %>%
  rename(adm1=state)
####
nga_adm1_state <- readRDS("nga_adm1.Rds")
nga_inadq_proportion_adm1 <- nga_inadq_proportion_adm1 %>%
  mutate(adm1 = case_when(
    adm1 == "Fct"            ~ "Abuja",
    adm1 == "Nasarawa"            ~ "Nassarawa",
    TRUE                       ~ adm1
  ))
###
nga_fe_inadequacy_adm2 <- nga_fe_inadequacy_adm2 %>%
  mutate(subpopulation = as.numeric(subpopulation))
####
nga_inadq_proportion_adm2 <- nga_survey %>%
  group_by(adm2) %>%
  summarise(across(
    .cols = c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg, 
              vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, zn_mg),
    .fns = ~ survey_mean(. == 1, na.rm = TRUE, vartype = NULL) * 100, 
    .names = "{.col}_prop"
  )) %>%
  left_join(nga_fe_inadequacy_adm2, by = c("adm2"="subpopulation"))

nga_inadq_proportion_adm2 <- nga_inadq_proportion_adm2 %>%
  left_join(nga_adm2_lga, by="adm2") %>%
  select(-adm2) %>%
  rename(adm2=lga)


#----------------------------------------------------------------GHA----------------------------------------------------
gha_hh_info <- read.csv("gha_glss17_hh_info.csv")

gha_hh <- gha_hh_info %>%
  select(hhid, adm1, adm2, survey_wgt)

gha_base_ai_fe <- gha_base_ai %>%
  left_join(gha_hh, by = "hhid") %>%
  select(hhid, adm1, adm2, survey_wgt, fe_mg)


gha_fe_inadequacy_adm1 <- fe_full_prob(gha_base_ai_fe, group1 = adm1, bio_avail = 10, hh_weight = "hh_weight")

gha_fe_inadequacy_adm2 <- fe_full_prob(gha_base_ai_fe, group1 = adm2, bio_avail = 10, hh_weight = "hh_weight")

gha_targets <- apply_nutrient_deficiency_flag(gha_base_ai)

gha_target_hh_info <- gha_targets %>%
  left_join(gha_hh_info, by="hhid") %>%
  select(-pc_expenditure, -sep_quintile, -res_quintile, -year, -month, -afe, -fe_mg)

# Convert the dataset into a survey design
gha_survey <- gha_target_hh_info %>%
  as_survey_design(
    ids = ea,            # Clusters (PSU)
    strata = res,        # Stratification variable
    weights = survey_wgt # Household survey weights
  )

gha_inadq_proportion_adm1 <- gha_survey %>%
  group_by(adm1) %>%
  summarise(across(
    .cols = c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg, 
              vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, zn_mg),
    .fns = ~ survey_mean(. == 1, na.rm = TRUE, vartype = NULL) * 100, 
    .names = "{.col}_prop"
  )) %>%
  left_join(gha_fe_inadequacy_adm1, by = c("adm1"="subpopulation"))

gha_fe_inadequacy_adm2$subpopulation <- as.numeric(gha_fe_inadequacy_adm2$subpopulation)

gha_inadq_proportion_adm2 <- gha_survey %>%
  group_by(adm2) %>%
  summarise(across(
    .cols = c(energy_kcal, vita_rae_mcg, thia_mg, ribo_mg, niac_mg, 
              vitb6_mg, vitd_mcg, folate_mcg, vitb12_mcg, zn_mg),
    .fns = ~ survey_mean(. == 1, na.rm = TRUE, vartype = NULL) * 100, 
    .names = "{.col}_prop"
  )) %>%
  left_join(gha_fe_inadequacy_adm2, by = c("adm2"="subpopulation"))

