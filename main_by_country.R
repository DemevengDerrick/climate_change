#################################################################################################
#                                                                                               #
#                                CLIMATE CHANGE PROJECT                                         #
#                                 By Derrick DEMEVENG                                           #
#                                                                                               #
#  Purpose: Run the flood exposure pipeline for one or more countries.                         #
#  Usage:   Edit the PARAMETERS section, then source("main_by_country.R").                     #
#                                                                                               #
#################################################################################################

# LOAD LIBRARIES -------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  dplyr, stringr, sf, ggplot2,
  terra, tibble, purrr,
  openxlsx, scales, ggspatial, lwgeom
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
# Add or remove country codes to run the pipeline for multiple countries.
ctry_codes      <- c("GMB")   # e.g. c("KEN", "GMB", "NGA")
return_period   <- 100
flood_threshold <- 0.1

# Shared input paths (same flood tiles and admin boundaries for all countries)
flood_dir        <- "input/flood_layers_RP100/"
flood_tiles_path <- "input/flood_tiles/tile_extents.geojson"
admin0_path      <- "input/geoboundaries/geoBoundariesCGAZ_ADM0/geoBoundariesCGAZ_ADM0.shp"
admin2_path      <- "input/geoboundaries/geoBoundariesCGAZ_ADM2/geoBoundariesCGAZ_ADM2.shp"

# Country-specific population folder: "input/pop/{tolower(ctry_code)}/"
# Override this named list if your folder names differ.
pop_dir_map <- list(
  GMB = "input/pop/gambia/",
  KEN = "input/pop/",
  NGA = "input/pop/nigeria/"
)

# LOAD SHARED BOUNDARIES -----------------------------------------------
admin0 <- sf::read_sf(admin0_path)
admin2 <- sf::read_sf(admin2_path)

# PER-COUNTRY PIPELINE -------------------------------------------------
all_indicators <- tibble::tibble()

for (ctry_code in ctry_codes) {

  message("\n===== Processing: ", ctry_code, " =====")

  ctry_admin0 <- admin0 |> dplyr::filter(shapeGroup == ctry_code)
  admin2_ctry <- admin2 |> dplyr::filter(shapeGroup == ctry_code)

  pop_dir  <- pop_dir_map[[ctry_code]]
  if (is.null(pop_dir)) {
    warning("No population directory mapped for ", ctry_code, ". Skipping.")
    next
  }
  pop_files <- list.files(pop_dir, full.names = TRUE)

  # Flood mosaic
  vrt_path   <- file.path("output/vrt_flood",
                           paste0(ctry_code, "_RP", return_period, "_depth.vrt"))
  tile_files <- fld_select_tiles(ctry_admin0, flood_tiles_path, flood_dir,
                                  return_period)
  rp100      <- fld_build_vrt(tile_files, vrt_path)

  # Flood fraction (computed once against the first population year as reference)
  pop_ref       <- terra::rast(pop_files[[1]])
  pop_ref_c     <- pop_clip(pop_ref, ctry_admin0)
  rp100_bin_c   <- fld_clip(fld_binarise(rp100, flood_threshold), ctry_admin0)
  flood_frac    <- fld_aggregate_to_pop(rp100_bin_c, pop_ref_c)

  # Exposure indicators
  indicators <- exp_run_country(
    pop_files      = pop_files,
    flood_frac_ras = flood_frac,
    ctry_boundary  = ctry_admin0,
    admin_zones    = admin2_ctry,
    indicator_name = "Women 15-49 exposed to RP100 floods",
    indicator_code = "wraf100",
    admin_level    = 2L
  )

  # Export country results
  out_xlsx <- file.path("output/zonal_stats",
                         paste0(ctry_code, "_indicators.xlsx"))
  dir.create(dirname(out_xlsx), recursive = TRUE, showWarnings = FALSE)
  openxlsx::write.xlsx(indicators, out_xlsx)

  # Visualise
  admin2_join     <- admin2_ctry |>
    dplyr::left_join(indicators, by = c("shapeID" = "admin.code"))
  ctry_indicators <- exp_summarise_by_country(indicators)

  viz_choropleth(
    admin_sf  = admin2_join,
    ctry_sf   = ctry_admin0,
    ctry_code = ctry_code,
    out_path  = file.path("output/flood_maps",
                           paste0(ctry_code, "_river_flood_exposure.jpg"))
  )

  viz_trend_histogram(
    ctry_indicators = ctry_indicators,
    out_path        = file.path("output/flood_maps/trend",
                                 paste0(ctry_code, "_river_flood_exposure_hist.jpg"))
  )

  all_indicators <- dplyr::bind_rows(all_indicators, indicators)
  message("Done: ", ctry_code)
}

message("\nAll countries processed. Combined indicator rows: ", nrow(all_indicators))
