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
  geodata
)

# INPUT VARIABLES ---------------------------------------------------------
ctry <- "Country"
flood_dir <- "input/flood_layers_RP100/"
flood_tiles <- "input/flood_tiles/tile_extents.geojson"

# LOAD DATA ---------------------------------------------------------------

flood_tiles <- sf::read_sf(flood_tiles) # flood tiles


# DATA TRANSFORMATION -----------------------------------------------------


# DATA VISUALIZATION ------------------------------------------------------

# visualize tiles
ggplot2::ggplot(data = flood_tiles) +
  ggplot2::geom_sf()
















