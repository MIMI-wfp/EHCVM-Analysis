# Author: Uchenna Agu
# Date: 03/03/2025
# Desription: Reproduces the FCQ for EHCVM countries (per household per day)

#====================================Load required libraries============================================

pacman::p_load(tidyverse,
               readxl,
               gridExtra,
               survey,
               sf,
               srvyr,
               devtools,
               rnaturalearth,
               tmap,
               wesanderson,
               stringi,
               haven)
# Sample processing for Senegal and Cote d'Ivoire
#======================================Senegal===========================================================

sen <- read.csv("s07b_me_sen2021.csv", stringsAsFactors = FALSE)
sen_menage <- read.csv("ehcvm_welfare_sen2021.csv")
nsu <- read.csv("ehcvm_nsu_sen2021_nat.csv")
sen_foods <- read.csv("mapping.csv", stringsAsFactors = FALSE)

sen_cons <- sen %>%
  mutate(hhid = paste0(grappe, "_", menage)) %>%
  select(hhid, s07bq01, s07bq02, s07bq03a, s07bq03b, s07bq03c)

sen_nsu <- nsu %>%
  select(produitID, uniteID, poids, tailleID)

sen_cons <- sen_cons %>%
  left_join(sen_nsu, by = c("s07bq01"="produitID", "s07bq03b"= "uniteID", "s07bq03c"= "tailleID"))

sen_menage <- sen_menage %>%
  mutate(hhid = paste0(grappe, "_", menage)) %>%
  select(hhid,hhsize )

sen_cons_df <- sen_cons %>%
  left_join(sen_menage, by = "hhid")

sen_cons_df <- sen_cons_df %>%
  mutate(quantity_g = (s07bq03a * poids)/7,
         quantity_100g = quantity_g / 100)

colSums(is.na(sen_cons_df))

sen_cons_df <- sen_cons_df %>%
  filter(!is.na(quantity_g))

sen_foods <- sen_foods %>%
  select(food_group, item_code, itemcode_sen, name_eng, edible_portion)


sen_cons_df <- sen_cons_df %>%
  left_join(sen_foods, by = c("s07bq01"="itemcode_sen")) %>%
  mutate(quantity_g = quantity_g * edible_portion,
         quantity_100g = quantity_100g * edible_portion) %>%
  select(hhid, s07bq01, item_code, food_group, name_eng, quantity_g, quantity_100g) %>%
  filter(quantity_g > 0)

sen_cons_df <- sen_cons_df %>%
  filter(!is.na(quantity_g))

sen_cons_csv <- sen_cons_df %>%
  select(hhid,item_code,quantity_g,quantity_100g)

#=============================================Cote d'Ivoire===========================================================

civ <- read.csv("s07b_me_civ2021.csv", stringsAsFactors = FALSE)
civ_menage <- read.csv("ehcvm_welfare_civ2021.csv")
nsu <- read.csv("ehcvm_nsu_civ2021.csv")
civ_foods <- read.csv("mapping.csv")

options(scipen = 99)

civ_cons <- civ %>%
  mutate(hhid = paste0(grappe, "_", menage)) %>%
  select(hhid, s07bq01, s07bq02, s07bq03a, s07bq03b, s07bq03c) %>%
  filter(s07bq02==1)

civ_nsu <- nsu %>%
  select(produitID, uniteID, poids, tailleID) %>%
  group_by(produitID, uniteID, tailleID) %>%
  summarise(poids = mean(poids, na.rm = TRUE), .groups = "drop")

civ_cons <- civ_cons %>%
  left_join(civ_nsu, by = c("s07bq01"="produitID", "s07bq03b"= "uniteID", "s07bq03c"= "tailleID"))

civ_menage <- civ_menage %>%
  mutate(hhid = paste0(grappe, "_", menage)) %>%
  select(hhid, hhsize )

civ_cons_df <- civ_cons %>%
  left_join(civ_menage, by = "hhid")

civ_cons_df <- civ_cons_df %>%
  mutate(quantity_g = (s07bq03a * poids)/7,
         quantity_100g = quantity_g / 100)

colSums(is.na(civ_cons_df))

civ_cons_df <- civ_cons_df %>%
  filter(!is.na(quantity_g))

civ_foods <- civ_foods %>%
  select(food_group,item_code, itemcode_civ, name_eng, edible_portion) 

civ_cons_df <- civ_cons_df %>%
  left_join(civ_foods, by = c("s07bq01"="itemcode_civ")) %>%
  mutate(quantity_g = quantity_g * edible_portion,
         quantity_100g = quantity_100g * edible_portion) %>%
  select(hhid, s07bq01, item_code, food_group,name_eng,quantity_g, quantity_100g) %>%
  filter(quantity_g > 0)

civ_cons_csv <- civ_cons_df %>%
  select(hhid,item_code,quantity_g,quantity_100g)



