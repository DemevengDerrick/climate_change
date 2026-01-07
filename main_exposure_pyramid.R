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
  terra,
  tibble,
  ggspatial,
  openxlsx,
  scales,
  lwgeom
)

# VARIABLES -------------------------------------------------------------
pop_folder_dir <- "input/pop/gambia/"
ctry_code <- "GMB"
flood_raster_folder_dir <- "input/flood_layers_RP100/"
flood_tiles_dir <- "input/flood_tiles/tile_extents.geojson"
admin0_dir <- "input/geoboundaries/geoBoundariesCGAZ_ADM0/geoBoundariesCGAZ_ADM0.shp"

# HELPER FUNCTIONS ------------------------------------------------------

extract_pop_meta <- function(file_path) {
  f <- basename(file_path)

  m <- str_match(
    f,
    # country_sex(_age)?_year_CN_resolution_....
    "^[a-z]{3}_((?:[fmt])|(?:T_[FM]))(?:_([0-9]{2}))?_([0-9]{4})_[A-Z]{2}_([0-9]+m)_"
  )

  tab <- tibble(
    file       = file_path,
    sex_code   = m[,2],
    age_group  = m[,3],          # NA for T_F / T_M (no age in filename)
    year       = as.integer(m[,4]),
    resolution = m[,5]
  ) |>
    mutate(
      sex = case_when(
        sex_code == "f"   ~ "female",
        sex_code == "m"   ~ "male",
        sex_code == "t"   ~ "total",
        sex_code == "T_F" ~ "female_total",
        sex_code == "T_M" ~ "male_total",
        TRUE              ~ NA_character_
      )
    )
  
  return(tab)
}

extract_pop_meta_multiple <- function(file_paths){
  pop_rasters_tab <- tibble::tibble()

  for(pop_raster in file_paths){
    tab <- extract_pop_meta(pop_raster)
    pop_rasters_tab <- dplyr::bind_rows(pop_rasters_tab, tab)
  }

  return(pop_rasters_tab)
}

flood_tiles_mosaic <- function(shp, flood_tiles_shp, flood_ras_dir){
  # ii) intersect the country with the flood tiles
  shp  <- sf::st_make_valid(shp)
  flood_tiles_shp  <- sf::st_make_valid(flood_tiles_shp)

  shp <- sf::st_transform(shp, sf::st_crs(flood_tiles_shp))
  ctry_tiles <- sf::st_intersects(shp, flood_tiles_shp)
  idx <- unlist(ctry_tiles)

  # iii) query the flood tiles
  # a) Pull tile codes
  ctry_tile_codes <- flood_tiles_shp[idx, ] |>
    dplyr::mutate(
      tile_name = paste0("ID", id, "_", name)
    ) |>
    dplyr::pull(tile_name)

  # b) Select the right tiles
  all_files <- list.files(flood_ras_dir, full.names = TRUE)

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
  vrt_path <- file.path("output/vrt_flood/flood_RP100_depth.vrt")
  dir.create(dirname(vrt_path), recursive = TRUE, showWarnings = FALSE)

  terra::vrt(selected_files, vrt_path, overwrite = TRUE)

  rp100 <- terra::rast(vrt_path)

  return(rp100)
}

# LOAD DATA -------------------------------------------------------------
pop_rasters <- dir(pop_folder_dir, recursive = T, full.names = T) # all pop raster files
pop_raster_meta <- extract_pop_meta_multiple(pop_rasters) # pop meta data dataframe
flood_tiles <- sf::read_sf(flood_tiles_dir) # flood tiles
admin0 <- sf::read_sf(admin0_dir)

# DATA TRANSFORMATION ---------------------------------------------------

# i) filter admin0 to the country of interest
ctry_admin0 <- admin0 |>
  dplyr::filter(shapeGroup == ctry_code)

rp100 <- flood_tiles_mosaic(ctry_admin0, flood_tiles, flood_raster_folder_dir)

