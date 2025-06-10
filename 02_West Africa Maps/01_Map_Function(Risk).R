# Map function for single country MIMI maps
# Author: Uchenna A
# library(ggplot2)
# library(wesanderson)
# library(sf)

# Map function for individual countries
#
# merged_sf   An sf object of subregions with a numeric fill variable.
# outline_sf  An sf object of the country boundary.
# fill_var    String name of the numeric field in merged_sf to map.
# palette     Name of a wesanderson palette (e.g. "Zissou1").
# n_pal       Number of palette steps.
# limits      Numeric vector of length 2 giving the fill scale limits.
# fill_name   Legend title for the fill scale.
# title       Plot title.
# caption     Plot caption.


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
    # single outline border
    geom_sf(
      data = outline_sf,
      fill = NA,
      color = "black",
      size = 1
    ) +
    # continuous gradient from palette
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
  merged_sf  = my_merged_data(ADM+proportions),
  outline_sf = my_country_outline,
  fill_var   = "zn_mg_prop",
  palette    = "Zissou1",
  limits     = c(0, 100),
  fill_name  = "Zinc (%)",
  title      = "Zinc Proportion by Administrative Area",
  caption    = "Data source"
)
