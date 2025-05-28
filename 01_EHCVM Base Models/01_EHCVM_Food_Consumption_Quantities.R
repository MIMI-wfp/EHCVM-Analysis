# Author: Uchenna Agu
# Date:
# Desription: Reproduces the FCQ for the 8 EHCVM countries (per household per day)

#===============================Load required libraries============================================

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

#===============================Senegal===========================================================

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
  select(food_group, itemcode_sen, name_eng, edible_portion)


sen_cons_df <- sen_cons_df %>%
  left_join(sen_foods, by = c("s07bq01"="itemcode_sen")) %>%
  mutate(quantity_g = quantity_g * edible_portion,
         quantity_100g = quantity_100g * edible_portion) %>%
  select(hhid,s07bq01,food_group,name_eng,quantity_g,quantity_100g) %>%
  rename(item_code=s07bq01) %>%
  filter(quantity_g > 0)

sen_cons_df <- sen_cons_df %>%
  filter(!is.na(quantity_g))

sen_cons_csv <- sen_cons_df %>%
  select(hhid,item_code,quantity_g,quantity_100g)

colSums(is.na(sen_cons_csv))

#===========================================Cote d'Ivoire===========================================================

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
  select(food_group, itemcode_civ, name_eng, edible_portion) 

civ_cons_df <- civ_cons_df %>%
  left_join(civ_foods, by = c("s07bq01"="itemcode_civ")) %>%
  mutate(quantity_g = quantity_g * edible_portion,
         quantity_100g = quantity_100g * edible_portion) %>%
  select(hhid,s07bq01,food_group,name_eng,quantity_g, quantity_100g,) %>%
  rename(item_code=s07bq01) %>%
  filter(quantity_g > 0)

civ_cons_csv <- civ_cons_df %>%
  select(hhid,item_code,quantity_g,quantity_100g)

colSums(is.na(civ_cons_csv))

#==============================================Mali===========================================================

mli <- read.csv("s07b_me_mli2021.csv", stringsAsFactors = FALSE)
mli_menage <- read.csv("ehcvm_welfare_mli2021.csv")
nsu <- read.csv("ehcvm_nsu_mli2021.csv")
mli_foods <- read.csv("mapping.csv")

mli_cons <- mli %>%
  mutate(hhid = paste0(grappe, "_", menage)) %>%
  select(hhid, s07bq01, s07bq02, s07bq03a, s07bq03b, s07bq03c)


mli_nsu <- nsu %>%
  select(produitID, uniteID, poids, tailleID) %>%
  group_by(produitID, uniteID, tailleID) %>%
  summarise(poids = mean(poids, na.rm = TRUE), .groups = "drop")

mli_cons <- mli_cons %>%
  left_join(mli_nsu, by = c("s07bq01"="produitID", "s07bq03b"= "uniteID", "s07bq03c"= "tailleID"))

mli_menage <- mli_menage %>%
  mutate(hhid = paste0(grappe, "_", menage)) %>%
  select(hhid,hhsize)

mli_cons_df <- mli_cons %>%
  left_join(mli_menage, by = "hhid")

####
mli_cons_df <- mli_cons_df %>%
  mutate(quantity_g = (s07bq03a * poids)/7,
         quantity_100g = quantity_g / 100)

colSums(is.na(mli_cons_df))

mli_cons_df <- mli_cons_df %>%
  filter(!is.na(quantity_g))

mli_foods <- mli_foods %>%
  select(food_group,item_code,itemcode_mli,name_eng, edible_portion) 

mli_cons_df <- mli_cons_df %>%
  left_join(mli_foods, by = c("s07bq01"="itemcode_mli")) %>%
  mutate(quantity_g = quantity_g * edible_portion,
         quantity_100g = quantity_100g * edible_portion) %>%
  select(hhid,s07bq01,item_code,food_group,name_eng,quantity_g, quantity_100g) 

mli_cons_df <- mli_cons_df %>%
  mutate(s07bq01 = item_code) %>%
  select(hhid,s07bq01,food_group,name_eng,quantity_g, quantity_100g) %>%
  rename(item_code=s07bq01) %>%
  filter(quantity_g > 0)

mli_cons_df <- mli_cons_df %>%
  filter(!is.na(item_code))

mli_cons_csv <- mli_cons_df %>%
  select(hhid,item_code,quantity_g,quantity_100g)

colSums(is.na(mli_cons_csv))

#================================================Niger==============================================================
ner <- read.csv("s07b_me_ner2021.csv", stringsAsFactors = FALSE)
ner_menage <- read.csv("ehcvm_welfare_ner2021.csv")
nsu <- read.csv("ehcvm_nsu_ner2021.csv")
ner_foods <- read.csv("mapping.csv")

ner_cons <- ner %>%
  mutate(hhid = paste0(grappe, "_", menage)) %>%
  select(hhid, s07bq01, s07bq02, s07bq03a, s07bq03b, s07bq03c) %>%
  filter(s07bq02==1)

ner_nsu <- nsu %>%
  select(produit, unite, taille, mu) %>%
  group_by(produit, unite, taille) %>%
  summarise(mu = mean(mu, na.rm = TRUE), .groups = "drop") 


ner_cons <- ner_cons %>%
  left_join(ner_nsu, by = c("s07bq01"="produit", "s07bq03b"= "unite", "s07bq03c"= "taille"))

ner_menage <- ner_menage %>%
  mutate(hhid = paste0(grappe, "_", menage)) %>%
  select(hhid, hhsize )

ner_cons_df <- ner_cons %>%
  left_join(ner_menage, by = "hhid")

ner_cons_df <- ner_cons_df %>%
  mutate(quantity_g = (s07bq03a * mu)/ 7,
         quantity_100g = quantity_g / 100)

colSums(is.na(ner_cons_df))

ner_cons_df <- ner_cons_df %>%
  filter(!is.na(quantity_g))

ner_foods <- ner_foods %>%
  select(food_group, item_code, itemcode_ner, name_eng, edible_portion) 

ner_cons_df <- ner_cons_df %>%
  left_join(ner_foods, by = c("s07bq01"="itemcode_ner")) %>%
  mutate(quantity_g = quantity_g * edible_portion,
         quantity_100g = quantity_100g * edible_portion) %>%
  select(hhid,s07bq01,item_code,food_group,name_eng,quantity_g,quantity_100g) 

ner_cons_df <- ner_cons_df %>%
  mutate(s07bq01 = item_code) %>%
  select(hhid,s07bq01,food_group,name_eng,quantity_g,quantity_100g) %>%
  rename(item_code=s07bq01) %>%
  filter(quantity_g > 0)

ner_cons_df <- ner_cons_df %>%
  filter(!is.na(item_code))

ner_cons_csv <- ner_cons_df %>%
  select(hhid,item_code,quantity_g,quantity_100g)

colSums(is.na(ner_cons_csv))

#============================================Burkina Faso==============================================================
bfa <- read.csv("s07b_me_bfa2021.csv", stringsAsFactors = FALSE)
bfa_menage <- read.csv("ehcvm_welfare_2b_bfa2021.csv")
nsu <- read.csv("ehcvm_nsu_bfa2021.csv")
bfa_foods <- read.csv("mapping.csv")


bfa_cons <- bfa %>%
  mutate(hhid = paste0(grappe, "_", menage)) %>%
  select(hhid, s07bq01, s07bq02, s07bq03a, s07bq03b, s07bq03c) %>%
  filter(s07bq02 == "Oui",
         s07bq03c != " ",
         !is.na(s07bq03a))

bfa_nsu <- nsu %>%
  select(codpr, uniteID, tailleID, poids_moyen) %>%
  mutate(tailleID = if_else(str_detect(tailleID, "taille"), "Taille unique", tailleID)) %>%
  mutate(uniteID = str_replace_all(uniteID, c("BoÃ®te" = "Boite", 
                                              "CueillÃ¨re" = "Cueillere", 
                                              "RÃ©gime" = "Regime", 
                                              "UnitÃ©" = "Unite")))


bfa_cons <-bfa_cons %>%
  mutate(s07bq03b = str_replace_all(s07bq03b, c("Cueillère" = "Cueillere", 
                                                "Avec os au tas" = "Avec os au Tas", 
                                                "Régime" = "Regime",
                                                "Boîte" = "Boite",
                                                "Unité" = "Unite")))

translations <- c(
  "132000"=132,
  "Abats et tripes (foie, rognon, etc.)" =  31,  
  "Ail" = 101,  
  "Ananas" = 72,  
  "Arachide grillée" = 117,  
  "Arachides décortiquées" = 115,  
  "Arachides fraîches en coques" = 113,  
  "Arachides séchées en coques" = 114,  
  "Arôme (Maggi, Jumbo, etc.)" = 145,  
  "Attiéke" = 132,  
  "Aubergine" = 93,  
  "Autre légumes frais n.d.a" = 107,  
  "Autre poisson fumé" = 45,  
  "Autre riz importé (brisure, etc.)" = 4,  
  "Autre riz local (riz pluvial)" = 2,  
  "Autres agrumes" = 75,  
  "Autres céréales" = 11,  
  "Autres légumes secs n.d.a" = 111,  
  "Autres poissons frais" = 43,  
  "Autres produits laitiers" = 59,  
  "Autres semoules de céréales" = 18,  
  "Autres tisanes et infusions n.d.a. (quinquelibat, citronelle, etc.)" = 159,  
  "Autres tubercules n.d.a" = 129,  
  "Autres viandes n.d.a" = 39,  
  "Avocats" = 77,  
  "Banane douce" = 76,  
  "Banane plantain" = 125,  
  "Beignets, galettes" = 26,  
  "Beurre" = 61,  
  "Beurre de karité" = 62,  
  "Biscuits" = 24,  
  "Bières et vins traditionnels (dolo, vin de palme, vin de raphia, vin de)" = 164,  
  "Bières industrielles" = 165,  
  "Blé" = 9,  
  "Boissons gazeuses (coca, fanta, sprite, youki, etc.)" = 162,  
  "Café en poudre" = 155,  
  "Café soluble" = 156,  
  "Canne à sucre" = 82,  
  "Caramel, bonbons, confiseries, etc" = 138,  
  "Carotte" = 90,  
  "Charcuterie (jambon, saucisson), conserves de viandes" = 36,  
  "Chocolat en poudre" = 158,  
  "Chocolat à croquer, pâte à tartiner" = 137,  
  "Citrons" = 74,  
  "Concentré de tomate" = 108,  
  "Concombre" = 92,  
  "Conserves de poisson" = 51,  
  "Courge/Courgette" = 94,  
  "Croissants" = 23,  
  "Cube alimentaire (Maggi, Jumbo, Adja, etc.)" = 144,  
  "Dattes" = 80,  
  "Eau minérale/ filtrée" = 161,  
  "Farine de blé local ou importé" = 16,  
  "Farine de maïs" = 12,  
  "Farine de mil" = 14,  
  "Farines de manioc" = 130,  
  "Feuilles d'oseille" = 102,  
  "Feuilles de baobab" = 103,  
  "Feuilles de haricot" = 104,  
  "Feuilles de manioc, feuilles de taro et, autres feuilles" = 105,  
  "Feuilles locales (Boulvanka)" = 106,  
  "Fonio" = 10,  
  "Fromage (à base de lait)" = 57,  
  "Fromage à base de soja" = 119,  
  "Fruit de baobab (téodo, teedo)" = 85,  
  "Gingembre frais" = 142,  
  "Gombo frais" = 98,  
  "Gombo sec" = 99,  
  "Gâteaux" = 25,  
  "Haricot vert" = 91,  
  "Huile d'arachide artisanale" = 65,  
  "Huile d'arachide raffinée" = 64,  
  "Huile de coton" = 67,  
  "Huile de palme raffinée" = 68,  
  "Huile de palme rouge" = 63,  
  "Huile de soja" = 66,  
  "Igname" = 124,  
  "Jus de fruits (orange, bissap, gingembre, jus de cajou, etc.)" = 160,  
  "Jus en poudre" = 163,  
  "Lait caillé, yaourt" = 53,  
  "Lait concentré non sucré" = 55,  
  "Lait concentré sucré" = 54,  
  "Lait en poudre" = 56,  
  "Lait frais" = 52,  
  "Mangue" = 71,  
  "Manioc" = 123,  
  "Mayonnaise" = 147,  
  "Maïs en grain" = 6,  
  "Melon" = 79,  
  "Miel" = 136,  
  "Niébé/Haricots secs" = 112,  
  "Noix de cajou" = 121,  
  "Noix de coco" = 81,  
  "Noix de cola" = 153,  
  "Noix de karité" = 122,  
  "Noix/graine de palme" = 69,  
  "Néré" = 86,  
  "Oeufs" = 60,  
  "Oignon frais" = 100,  
  "Orange" = 73,  
  "Pain moderne" = 21,  
  "Pain traditionnel" = 22,  
  "Papaye" = 84,  
  "Pastèque" = 78,  
  "Patate douce" = 128,  
  "Petit Mil" = 7,  
  "Petit pois secs" = 110,  
  "Petits pois (frais)" = 109,  
  "Piment frais" = 141,  
  "Piment séché" = 140,  
  "Poisson frais carpe" = 40,  
  "Poisson frais chinchard" = 41,  
  "Poisson frais maquereau" = 42,  
  "Poisson fumé siliure/carpe" = 44,  
  "Poisson séché" = 46,  
  "Poivre" = 151,  
  "Poivron frais" = 95,  
  "Pomme de terre" = 126,  
  "Pommes" = 83,  
  "Poulet sur pied" = 33,  
  "Pâte d'arachide" = 118,  
  "Pâtes alimentaires" = 20,  
  "Riz importé long grain" = 3,  
  "Riz local (Bagré Sourou et Bama)" = 1,  
  "Salade (laitue)" = 88,  
  "Sel" = 139,  
  "Sorgho" = 8,  
  "Soumbala (moutarde africaine)" = 146,  
  "Sucre en poudre" = 134,  
  "Sésame" = 120,  
  "Taro, macabo" = 127,  
  "Thé" = 157,  
  "Tomate fraiche" = 96,  
  "Tomate séchée" = 97,  
  "Viande d'autres volailles domestiques" = 35,  
  "Viande de boeuf" = 27,  
  "Viande de chameau" = 28,  
  "Viande de chèvre" = 30,  
  "Viande de mouton" = 29,  
  "Viande de porc" = 32,  
  "Viande de poulet" = 34,  
  "Viande séchée (boeuf, mouton, chameau)" = 37,  
  "Vinaigre de citron" = 149,  
  "semoule de blé" = 17,  
  "semoule de maïs" = 13,  
  "semoule de mil" = 15 
)

# Add English column using translations
bfa_nsu <-bfa_nsu %>%
  mutate(codpr = translations[codpr])



# Define translation mapping
translations <- c(
  "Riz importé long grain"=3,  
  "Maïs en grain"=6,  
  "Petit Mil"=7,  
  "Pâtes alimentaires"=20,  
  "Pain moderne"=21,  
  "Poisson fumé siliure/carpe"=44,  
  "Huile de palme raffinée"=68,  
  "Mangue"=71,  
  "Gombo frais"=98,  
  "Oignon frais"=100,  
  "Niébé/Haricots secs"=112,  
  "Pâte d'arachide"=118,  
  "Sucre en poudre"=134,  
  "Sel"=139,  
  "Cube alimentaire (Maggi, Jumbo, Adja, etc.)"=144,  
  "Soumbala (moutarde africaine)"=146,  
  "Thé"=157,  
  "Viande de mouton"=29,  
  "Viande de chèvre"=30,  
  "Lait en poudre"=56,  
  "Poivron frais"=95,  
  "Tomate fraiche"=96,  
  "Gombo sec"=99,  
  "Feuilles de baobab"=103,  
  "Café en poudre"=155,  
  "Eau minérale/ filtrée"=161,  
  "Viande de porc"=32,  
  "Huile de palme rouge"=63,  
  "Piment séché"=140,  
  "Sorgho"=8,  
  "Aubergine"=93,  
  "Piment frais"=141,  
  "Café soluble"=156,  
  "Autre riz importé (brisure, etc.)"=4,  
  "Farine de mil"=14,  
  "Pain traditionnel"=22,  
  "Beignets, galettes"=26,  
  "Viande de boeuf"=27,  
  "Lait frais"=52,  
  "Citrons"=74,  
  "Choux"=89,  
  "Haricot vert"=91,  
  "Ail"=101,  
  "Concentré de tomate"=108,  
  "Petits pois (frais)"=109,  
  "Chocolat en poudre"=158,  
  "Autres tisanes et infusions n.d.a. (quinquelibat, citronelle, etc.)"=159,  
  "Boissons gazeuses (coca, fanta, sprite, youki, etc.)"=162,  
  "Poisson séché"=46,  
  "Feuilles d'oseille"=102,  
  "Noix de cola"=153,  
  "Autre riz local (riz pluvial)"=2,  
  "Autres farines de céréales"=18,  
  "Arachides séchées en coques"=114,  
  "Farine de maïs"=12,  
  "Poulet sur pied"=33,  
  "Viande séchée (boeuf, mouton, chameau)"=37,  
  "Poisson frais carpe"=40,  
  "Oeufs"=60,  
  "Beurre"=61,  
  "Avocats"=77,  
  "Concombre"=92,  
  "Jus de fruits (orange, bissap, gingembre, jus de cajou, etc.)"=160,  
  "Feuilles locales (Boulvanka)"=106,  
  "Beurre de karité"=62,  
  "Autres fruits (tamarin noir, liane sauvage, raisin, fraise, pomme sauvage, etc.)"=87,  
  "Tomate séchée"=97,  
  "Bières et vins traditionnels (dolo, vin de palme, vin de raphia, vin de cajou, etc.)"=164,  
  "Arachide grillée"=117,  
  "Arachides fraîches en coques"=113,  
  "Pomme de terre"=126,  
  "Gâteaux"=25,  
  "Lait concentré sucré"=54,  
  "Biscuits"=24,  
  "Autre poisson fumé"=45,  
  "Bières industrielles"=165,  
  "Riz local (Bagré Sourou et Bama)"=1,  
  "Sucre en morceaux"=135,  
  "Poisson frais chinchard"=41,  
  "Orange"=73,  
  "Vinaigre de citron"=148,  
  "Carotte"=90,  
  "Lait caillé, yaourt"=53,  
  "Huile de coton"=67,  
  "Banane douce"=76,  
  "Salade (laitue)"=88,  
  "Courge/Courgette"=94,  
  "Mayonnaise"=147,  
  "Autres vinaigres"=149,  
  "Moutarde"=150,  
  "Huile d'arachide raffinée"=64,  
  "Feuilles de manioc, feuilles de taro et, autres feuilles"=105,  
  "Fruit de Kapokier (voaga, )"=133,  
  "Papaye"=84,  
  "Attiéke"=132,  
  "Dattes"=80,  
  "Néré"=86,  
  "Feuilles de haricot"=104,  
  "Gingembre frais"=142,  
  "Autres produits alimentaires n.d.a"=154,  
  "Viande de poulet"=34,  
  "Maïs en épi"=5,  
  "Miel"=136,  
  "Gingembre moulu"=143,  
  "Poisson frais maquereau"=42,  
  "Pastèque"=78,  
  "Petit pois secs"=110,  
  "Patate douce"=128,  
  "Huile de karité"=175,  
  "Arachides décortiquées"=115,  
  "Igname"=124,  
  "Lait concentré non-sucré"=55,  
  "Caramel, bonbons, confiseries, etc"=138,  
  "Fonio"=10,  
  "Autres fruits de mer"=50,  
  "Autres viandes n.d.a"=39,  
  "Banane plantain"=125,  
  "Autre légumes frais n.d.a"=107,  
  "Viande d'autres volailles domestiques"=35,  
  "Sésame"=120,  
  "Gibiers (viandes d'animaux sauvages)"=38,  
  "Arachides pilées"=116,  
  "Huile d'arachide artisanale"=65,  
  "Autres condiments n.d.a"=152,  
  "Goyave"=176,  
  "Autres poissons frais"=43,  
  "Noix de karité"=122,  
  "Pommes"=83,  
  "Autres produits laitiers"=59,  
  "Semoule de mil"=15,  
  "Manioc"=123,  
  "Melon"=79,  
  "Semoule de maïs"=13,  
  "Croissants"=23,  
  "Abats et tripes (foie, rognon, etc.)"=31,  
  "Noix de coco"=81,  
  "Fruit de baobab (téodo, teedo)"=85,  
  "Fromage à base de soja"=119,  
  "Chocolat à croquer, pâte à tartiner"=137,  
  "Poivre"=151,  
  "Conserves de poisson"=51,  
  "Huile de soja"=66,  
  "Lait et farines pour bébé"=58,  
  "Noix/graine de palme"=69,  
  "Autres céréales"=11,  
  "Farine de blé local ou importé"=16,  
  "Ananas"=72,  
  "Noix de cajou"=121,  
  "Canne à sucre"=82,  
  "Jus en poudre"=163,  
  "Autres semoules de céréales"=19,  
  "Crevettes séchées"=49,  
  "Taro, macabo"=127,  
  "Charcuterie (jambon, saucisson), conserves de viandes"=36,  
  "Autres huiles n.d.a. (maïs, huile palmiste, huile d'olive, huile de tournesol, huile de lait de vache etc.)"=70,
  "Gari, tapioca" =        131,                                                                                     
  "Arôme (Maggi, Jumbo, etc.)"    =   145,                                                                          
  "Blé"  =              9  ,                                                                                      
  "Crevettes fraiches"      =   48,                                                                                
  "Farines de manioc"   =       130 ,                                                                               
  "Fromage (à base de lait)"   =   57,                                                                             
  "Autres agrumes"    =    75,                                                                                     
  "Autres tubercules n.d.a"    =    129,                                                                            
  "Pâte de manioc"     =     178,                                                                                   
  "Autres légumes secs n.d.a"     =      111 ,                                                                      
  "semoule de blé"    =         17    
)

# Apply translation to create English column
bfa_cons <-bfa_cons %>%
  mutate(s07bq01 = translations[s07bq01])


bfa_nsu <- bfa_nsu %>%
  select(codpr, uniteID, tailleID, poids_moyen) %>%
  group_by(codpr, uniteID, tailleID) %>%
  slice(1) %>%
  ungroup()

bfa_cons <-bfa_cons %>%
  left_join(bfa_nsu, by = c("s07bq01"="codpr", "s07bq03b"= "uniteID", "s07bq03c"= "tailleID"))


bfa_menage <- bfa_menage %>%
  mutate(hhid = paste0(grappe, "_", menage)) %>%
  select(hhid,hhsize)

bfa_cons_df <- bfa_cons %>%
  left_join(bfa_menage, by = "hhid")


bfa_cons_df <- bfa_cons_df %>%
  mutate(quantity_g= (s07bq03a * poids_moyen) / 7,
         quantity_100g = quantity_g / 100)

colSums(is.na(bfa_cons_df))

bfa_cons_df <- bfa_cons_df %>%
  filter(!is.na(quantity_g))

bfa_foods <- bfa_foods %>%
  select(food_group, item_code, name_eng, edible_portion) 

bfa_cons_df <- bfa_cons_df %>%
  left_join(bfa_foods, by = c("s07bq01"="item_code")) %>%
  mutate(quantity_g = quantity_g * edible_portion,
         quantity_100g = quantity_100g * edible_portion) %>%
  select(hhid,s07bq01,food_group,name_eng,quantity_g,quantity_100g) 

bfa_cons_df <- bfa_cons_df %>%
  select(hhid,s07bq01,food_group,name_eng,quantity_g,quantity_100g) %>%
  rename(item_code=s07bq01) %>%
  filter(quantity_g > 0)

bfa_cons_df <- bfa_cons_df %>%
  filter(!is.na(item_code))

bfa_cons_csv <- bfa_cons_df %>%
  select(hhid,item_code,quantity_g,quantity_100g)

colSums(is.na(bfa_cons_csv))

#============================================Guinea Bissau==============================================================

gnb <- read.csv("s07b_me_gnb2021.csv", stringsAsFactors = FALSE)
gnb_menage <- read.csv("ehcvm_welfare_gnb2021.csv")
nsu <- read.csv("ehcvm_nsu_gnb2021.csv")
gnb_foods <- read.csv("mapping.csv")

options(scipen = 99)

gnb_cons <- gnb %>%
  mutate(hhid = paste0(grappe, "_", menage)) %>%
  select(hhid, s07bq01, s07bq02, s07bq03a, s07bq03b, s07bq03c) %>%
  filter(s07bq02==1) %>%
  filter(s07bq01 !=134) %>%
  filter(s07bq01 !=121) %>%
  filter(s07bq01 !=55) %>%
  filter(s07bq01 !=98) 


gnb_nsu <- nsu %>%
  select(produitID, uniteID, poids, tailleID) %>%
  group_by(produitID, uniteID, tailleID) %>%
  summarise(poids = mean(poids, na.rm = TRUE), .groups = "drop")

gnb_cons <- gnb_cons %>%
  left_join(gnb_nsu, by = c("s07bq01"="produitID", "s07bq03b"= "uniteID", "s07bq03c"= "tailleID"))

gnb_menage <- gnb_menage %>%
  mutate(hhid = paste0(grappe, "_", menage)) %>%
  select(hhid,hhsize)

gnb_cons_df <- gnb_cons %>%
  left_join(gnb_menage, by = "hhid")

gnb_cons_df <- gnb_cons_df %>%
  mutate(quantity_g = (s07bq03a * poids) / 7,
         quantity_100g=quantity_g/100)

colSums(is.na(gnb_cons_df))

gnb_cons_df <- gnb_cons_df %>%
  filter(!is.na(quantity_g))

gnb_foods <- gnb_foods %>%
  select(food_group, item_code, itemcode_gnb, name_eng, edible_portion) 

gnb_cons_df <- gnb_cons_df %>%
  left_join(gnb_foods, by = c("s07bq01"="itemcode_gnb")) %>%
  mutate(quantity_g = quantity_g * edible_portion,
         quantity_100g= quantity_100g * edible_portion) %>%
  select(hhid,s07bq01,item_code,quantity_100g,food_group,name_eng,quantity_g) 

gnb_cons_df <- gnb_cons_df %>%
  mutate(s07bq01 = item_code) %>%
  select(hhid,hhid,s07bq01,quantity_100g,food_group,name_eng,quantity_g) %>%
  rename(item_code=s07bq01)

gnb_cons_df <- gnb_cons_df %>%
  filter(!is.na(item_code))

gnb_cons_df <- gnb_cons_df %>%
  arrange(desc(quantity_g)) %>%
  slice(-1:-50)  


gnb_cons_csv <- gnb_cons_df %>%
  select(hhid,item_code,quantity_g,quantity_100g)

colSums(is.na(gnb_cons_csv))

#============================================Togo==============================================================

tgo <- read.csv("s07b_me_tgo2021.csv", stringsAsFactors = FALSE)
tgo_menage <- read.csv("ehcvm_welfare_tgo2021.csv")
nsu <- read.csv("ehcvm_nsu_tgo2021.csv")
tgo_foods <- read.csv("mapping.csv")

options(scipen = 99)

tgo_cons <- tgo %>%
  mutate(hhid = paste0(grappe, "_", menage)) %>%
  select(hhid, s07bq01, s07bq02, s07bq03a, s07bq03b, s07bq03c) %>%
  filter(s07bq02==1) 

tgo_nsu <- nsu %>%
  select(produitID, uniteID, q110a, tailleID) %>%
  group_by(produitID, uniteID, tailleID) %>%
  summarise(q110a = mean(q110a, na.rm = TRUE), .groups = "drop")

tgo_cons <- tgo_cons %>%
  left_join(tgo_nsu, by = c("s07bq01"="produitID", "s07bq03b"= "uniteID", "s07bq03c"= "tailleID"))

tgo_menage <- tgo_menage %>%
  mutate(hhid = paste0(grappe, "_", menage)) %>%
  select(hhid,hhsize)

tgo_cons_df <- tgo_cons %>%
  left_join(tgo_menage, by = "hhid")

tgo_cons_df <- tgo_cons_df %>%
  mutate(quantity_g = q110a / 7,
         quantity_100g=quantity_g/100)

colSums(is.na(tgo_cons_df))

tgo_cons_df <- tgo_cons_df %>%
  filter(!is.na(quantity_g))

tgo_foods <- tgo_foods %>%
  select(food_group, item_code, itemcode_tgo, name_eng, edible_portion) 

tgo_cons_df <- tgo_cons_df %>%
  left_join(tgo_foods, by = c("s07bq01"="itemcode_tgo")) %>%
  mutate(quantity_g = quantity_g * edible_portion,
         quantity_100g= quantity_100g * edible_portion) %>%
  select(hhid,s07bq01,item_code,quantity_100g,food_group,name_eng,quantity_g) 

tgo_cons_df <- tgo_cons_df %>%
  mutate(s07bq01 = item_code) %>%
  select(hhid,s07bq01,quantity_100g,food_group,name_eng,quantity_g) %>%
  rename(item_code=s07bq01)

tgo_cons_df <- tgo_cons_df %>%
  filter(!is.na(item_code))

tgo_cons_csv <- tgo_cons_df %>%
  select(hhid,item_code,quantity_g,quantity_100g)


#==============================================Benin================================================================

ben <- read_sav("s07b_me_ben2021.sav")
ben_menage <- read_sav("ehcvm_welfare_ben2021.sav")
nsu <- read_sav("ehcvm_nsu_ben2021.sav")
ben_foods <- read.csv("mapping.csv")

ben_cons <- ben %>%
  mutate(hhid = paste0(grappe, "_", menage)) %>%
  select(hhid, s07bq01, s07bq02, s07bq03a, s07bq03b, s07bq03c) %>%
  filter(s07bq02==1)

ben_nsu <- nsu %>%
  select(produitID, uniteID, poids, tailleID) %>%
  group_by(produitID, uniteID, tailleID) %>%
  summarise(poids = mean(poids, na.rm = TRUE), .groups = "drop")

ben_cons <- ben_cons %>%
  left_join(ben_nsu, by = c("s07bq01"="produitID", "s07bq03b"= "uniteID", "s07bq03c"= "tailleID"))

ben_menage <- ben_menage %>%
  mutate(hhid = paste0(grappe, "_", menage)) %>%
  select(hhid,hhsize)

ben_cons_df <- ben_cons %>%
  left_join(ben_menage, by = "hhid")

ben_cons_df <- ben_cons_df %>%
  mutate(quantity_g = (s07bq03a * poids *1000) / 7,
         quantity_100g=quantity_g/100)

colSums(is.na(ben_cons_df))

ben_cons_df <- ben_cons_df %>%
  filter(!is.na(quantity_g))

ben_foods <- ben_foods %>%
  select(food_group, item_code, itemcode_ben, name_eng, edible_portion) 

ben_cons_df <- ben_cons_df %>%
  left_join(ben_foods, by = c("s07bq01"="itemcode_ben")) %>%
  mutate(quantity_g = quantity_g * edible_portion,
         quantity_100g = quantity_100g * edible_portion) %>%
  select(hhid,s07bq01,item_code,food_group,name_eng,quantity_g,quantity_100g) 

ben_cons_df <- ben_cons_df %>%
  mutate(s07bq01 = item_code) %>%
  select(hhid,s07bq01,food_group,name_eng,quantity_g,quantity_100g) %>%
  rename(item_code=s07bq01) %>%
  filter(quantity_g > 0)

ben_cons_df <- ben_cons_df %>%
  filter(!is.na(item_code))

ben_cons_csv <- ben_cons_df %>%
  select(hhid,item_code,quantity_g,quantity_100g)

#===================================================== EDA ==================================================================

check_missing_values <- function(...) {
  df_list <- list(...)
  df_names <- as.character(match.call(expand.dots = FALSE)[[2]])
  
  result <- lapply(seq_along(df_list), function(i) {
    data.frame(
      dataframe = df_names[i],
      variable = names(df_list[[i]]),
      missing_count = colSums(is.na(df_list[[i]]))
    )
  })
  
  do.call(rbind, result)
}

missing_values_summary <- check_missing_values(sen_cons_csv, civ_cons_csv, bfa_cons_csv, 
                                               mli_cons_csv, tgo_cons_csv, ner_cons_csv, 
                                               ben_cons_csv, gnb_cons_csv)

print(missing_values_summary)

#================================================== Save ======================================================================

# write.csv(sen_cons_csv, file = "sen_food_consumption.csv", row.names = F)
# write.csv(civ_cons_csv, file = "civ_food_consumption.csv", row.names = F)
# write.csv(bfa_cons_csv, file = "bfa_food_consumption.csv", row.names = F)
# write.csv(mli_cons_csv, file = "mli_food_consumption.csv", row.names = F)
# write.csv(tgo_cons_csv, file = "tgo_food_consumption.csv", row.names = F)
# write.csv(ner_cons_csv, file = "ner_food_consumption.csv", row.names = F)
# write.csv(ben_cons_csv, file = "ben_food_consumption.csv", row.names = F)
# write.csv(gnb_cons_csv, file = "gnb_food_consumption.csv", row.names = F)
