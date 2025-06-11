# Load required packages for this session
# Script to generate sample map from database
# Author: Uchenna A

rq_packages <- c("readr", "DBI", "RMySQL", "tidyverse", "getPass", "devtools", "wesanderson", "sf")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

# Establish the database connection
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "mimi_db",
                 host = "localhost",
                 port = 3306,
                 user = getPass("Enter username: "),
                 password = getPass("Enter password: "))

# Get map data from database (Senegal as an example)
sen_hungermap <- dbGetQuery(con, "SELECT * FROM hungermap_mimi WHERE iso3='SEN'")

# Get SEN shapefile from WFP
source_url("https://raw.githubusercontent.com/MIMI-wfp/MIMI-R-functions/refs/heads/main/WFP_geoAPI/get_shapefile.R")
sen_adm1 <- get_shapefile(278, "adm1")

# Merge shapefile with  hunger map data from database
sen_merged <- sen_hungermap %>%
  left_join(sen_adm1, by=c("wfp_adm1_code"="Code"))

#Set as sf object
sen_merged <- st_as_sf(sen_merged, sf_column_name = "geometry")

# Map function
plot_sf_choropleth <- function(
    merged_sf,
    outline_sf,
    fill_var       = "zn_mg_prop",
    palette        = "Zissou1",
    n_pal          = 100,
    limits         = c(0, 100),
    fill_name      = "Value",
    title          = "Choropleth Map",
    caption        = NULL
) {
  ggplot() +
    # subregions colored by fill_var (no borders)
    geom_sf(
      data = merged_sf,
      aes(fill = .data[[fill_var]]),
      color = NA
    ) +
    scale_fill_gradientn(
      colours = wes_palette(palette, n = n_pal, type = "continuous"),
      limits = limits,
      name   = fill_name
    ) +
    labs(
      title   = title,
      caption = caption
    ) +
    theme_minimal() +
    theme(
      plot.title       = element_text(hjust = 0.5),
      plot.caption     = element_text(hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title       = element_blank(),
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.title     = element_text(hjust = 0.5)
    )
}

# Example usage
plot_sf_choropleth(
  merged_sf  = sen_merged,
  fill_var   = "zn_ai",
  palette    = "Zissou1",
  limits     = c(0, 100),
  fill_name  = "Zinc (%)",
  title      = "Risk of Inadequate Intake of Zinc in Senegal",
  caption    = "EHCVM 21/22"
)
