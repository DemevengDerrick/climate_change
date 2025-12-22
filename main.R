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
pop_dir <- "input/pop/female_pop_15_49.tif"

# LOAD DATA ---------------------------------------------------------------
# vectors
flood_tiles <- sf::read_sf(flood_tiles) # flood tiles
admin0 <- sf::read_sf(admin0_dir)

# rasters
pop_15_49 <- terra::rast(pop_dir)

# DATA TRANSFORMATION -----------------------------------------------------

# i) filter admin0 to the country of interest
ctry_admin0 <- admin0 |>
  dplyr::filter(shapeGroup == ctry_code)

# ii) intersect the country with the flood tiles
ctry_admin0  <- st_make_valid(ctry_admin0)
flood_tiles  <- st_make_valid(flood_tiles)

ctry_admin0 <- sf::st_transform(ctry_admin0, sf::st_crs(flood_tiles))
ctry_tiles <- sf::st_intersects(ctry_admin0, flood_tiles)
idx <- unlist(ctry_tiles)

# iii) query the flood tiles
# a) Pull tile codes
ctry_tile_codes <- flood_tiles[idx, ] |>
  dplyr::mutate(
    tile_name = paste0("ID", id, "_", name)
  ) |>
  pull(tile_name)

# b) Select the right tiles
all_files <- list.files(flood_dir, full.names = TRUE)

selected_files <- all_files[
  stringr::str_detect(
    basename(all_files),
    paste(ctry_tile_codes, collapse = "|")
  )
]

selected_files <- selected_files[
  stringr::str_detect(selected_files, "_RP100_depth\\.tif$")
]

# iv) Mosaic the queried tiles
vrt_path <- file.path("output/vrt_flood/", paste0(ctry_code, "_RP100_depth.vrt"))
dir.create(dirname(vrt_path), recursive = TRUE, showWarnings = FALSE)

terra::vrt(selected_files, vrt_path, overwrite = TRUE)

rp100 <- terra::rast(vrt_path)

# v) Reclasify flood zones into 0 : No flood, and 1: flood depth >= 0.1
rp100_binary <- terra::ifel(rp100 >= 0.1, 1, 0)

# vi) Clip the pop and flood raster to the extend of country
ctry_v <- terra::vect(ctry_admin0) # covert the sf polygon to a spatvector polygon

rp100_binary_crop <- terra::crop(rp100_binary, ctry_v) # crop the raster
rp100_binary_clip <- terra::mask(rp100_bin_crop, ctry_v) # mask the raster

pop_15_49_crop <- terra::crop(pop_15_49, ctry_v) # crop the raster
pop_15_49_clip <- terra::mask(pop_15_49, ctry_v) # mask the raster

# vii) Align and resample to 1km the flood raster based on the pop raster
rp100_frac_1km <- resample(rp100_binary_clip, pop_15_49_clip, method = "average")

# DATA VISUALIZATION ------------------------------------------------------
plot(rp100_binary_clip)
plot(pop_15_49_clip)

# visualize tiles
ggplot2::ggplot(data = flood_tiles) +
  ggplot2::geom_sf() +
  ggplot2::geom_raster(data = rp100)














