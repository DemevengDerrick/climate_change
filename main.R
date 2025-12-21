#################################################################################################
#                                                                                               #
#                                CLIMATE CHANGE PROJECT                                         #
#                                 By Derrick DEMEVENG                                           #
#                                                                                               #
#################################################################################################

# LOAD LIBRARIES -------------------------------------------------------
if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  dplyr,
  stringr,
  tidyr,
  sf,
  ggplot2,
  geodata,
  terra
)

# INPUT VARIABLES ---------------------------------------------------------
ctry_code <- "CMR"
flood_dir <- "input/flood_layers_RP100/"
flood_tiles <- "input/flood_tiles/tile_extents.geojson"
admin0_dir <- "input/geoboundaries/geoBoundariesCGAZ_ADM0/geoBoundariesCGAZ_ADM0.shp"

# LOAD DATA ---------------------------------------------------------------
flood_tiles <- sf::read_sf(flood_tiles) # flood tiles
admin0 <- sf::read_sf(admin0_dir)

# DATA TRANSFORMATION -----------------------------------------------------

# filter admin0 to the country of interest
ctry_admin0 <- admin0 |>
  dplyr::filter(shapeGroup == ctry_code)

# intersect the country with the flood tiles
ctry_tiles <- terra::intersect(ctry_admin0, flood_tiles)

# DATA VISUALIZATION ------------------------------------------------------

# visualize tiles
ggplot2::ggplot(data = flood_tiles) +
  ggplot2::geom_sf()














