#################################################################################################
#                                                                                               #
#                                CLIMATE CHANGE PROJECT                                         #
#                                 By Derrick DEMEVENG                                           #
#                                                                                               #
#  Purpose: Population pyramid of flood exposure by age group and sex.                         #
#  Run:     source("main_exposure_pyramid.R")                                                   #
#                                                                                               #
#################################################################################################

# LOAD LIBRARIES -------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  dplyr, stringr, sf, ggplot2,
  terra, tibble, purrr,
  openxlsx, scales, cli, patchwork
)

# LOAD PACKAGE FUNCTIONS -----------------------------------------------
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
} else {
  source("R/flood_utils.R")
  source("R/pop_utils.R")
  source("R/exposure_utils.R")
  source("R/viz_utils.R")
}

# INPUT PARAMETERS -----------------------------------------------------
ctry_code       <- "KEN"
return_period   <- 100
flood_threshold <- 0.1

pop_folder_dir       <- "input/pop/gambia/"
flood_dir            <- "input/flood_layers_RP100/"
flood_tiles_path     <- "input/flood_tiles/tile_extents.geojson"
admin0_path          <- "input/geoboundaries/geoBoundariesCGAZ_ADM0/geoBoundariesCGAZ_ADM0.shp"
pyramid_out          <- "output/flood_maps/exposure_pyramid.jpg"

# LOAD DATA ------------------------------------------------------------
pop_files  <- dir(pop_folder_dir, recursive = TRUE, full.names = TRUE)
admin0     <- sf::read_sf(admin0_path)
ctry_admin0 <- admin0 |> dplyr::filter(shapeGroup == ctry_code)

# POPULATION METADATA --------------------------------------------------
pop_meta <- pop_parse_filenames(pop_files)

# FLOOD MOSAIC ---------------------------------------------------------
tile_files <- fld_select_tiles(ctry_admin0, flood_tiles_path, flood_dir,
                                return_period)
vrt_path   <- file.path("output/vrt_flood",
                         paste0(ctry_code, "_RP", return_period, "_depth.vrt"))
rp100      <- fld_build_vrt(tile_files, vrt_path)

# EXPOSURE LOOP --------------------------------------------------------
exposure_stat <- pop_meta
exposure_stat$pop_exposed <- 0.0
exposure_stat$pop_tot     <- 0.0

pb <- cli::cli_progress_bar("Processing rasters", total = nrow(exposure_stat))

for (i in seq_len(nrow(exposure_stat))) {
  pop_ras <- terra::rast(exposure_stat$file[i])

  pops <- exp_compute(
    pop_ras        = pop_clip(pop_ras, ctry_admin0),
    flood_frac_ras = {
      flood_clipped <- fld_clip(fld_binarise(rp100, flood_threshold), ctry_admin0)
      pop_c         <- pop_clip(pop_ras, ctry_admin0)
      fld_aggregate_to_pop(flood_clipped, pop_c)
    }
  )

  zones_v <- terra::vect(ctry_admin0)

  stat_exposed <- terra::zonal(pops$pop_exposed, zones_v,
                                fun = sum, na.rm = TRUE, as.polygons = FALSE)
  stat_total   <- terra::zonal(pops$pop_total,   zones_v,
                                fun = sum, na.rm = TRUE, as.polygons = FALSE)

  exposure_stat$pop_exposed[i] <- stat_exposed[[1]][1]
  exposure_stat$pop_tot[i]     <- stat_total[[1]][1]

  cli::cli_progress_update(id = pb, set = i)
}

cli::cli_progress_done(id = pb)

# PREPARE PYRAMID DATA -------------------------------------------------
pyramid_data <- viz_prep_pyramid_data(exposure_stat)

# VISUALISE ------------------------------------------------------------
viz_exposure_pyramid(
  pyramid_data = pyramid_data,
  ctry_code    = ctry_code,
  out_path     = pyramid_out
)
