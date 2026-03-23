#################################################################################################
#                                                                                               #
#                          WORLDPOP DATA DOWNLOAD & PROCESSING                                  #
#                                 By Derrick DEMEVENG                                           #
#                                                                                               #
#  Default (scope = "global"):                                                                   #
#    Downloads 1km unconstrained WorldPop data, sums into demographic groups                    #
#    at the full country extent (no clipping), saves to input/pop/.                             #
#                                                                                               #
#  Optional (scope = "country"):                                                                 #
#    Downloads 100m constrained UN-adjusted data, clips to country boundary.                    #
#    Uncomment the second call at the bottom to use this mode.                                   #
#                                                                                               #
#################################################################################################

# LIBRARIES --------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf, terra, dplyr, purrr, tibble, httr, stringr)

# LOAD PACKAGE FUNCTIONS -------------------------------------------------------
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
} else {
  source("R/pop_utils.R")
  source("R/worldpop_data.R")
}

# INPUT PARAMETERS -------------------------------------------------------------
ctry_code    <- "KEN"
year         <- 2020

admin0_path  <- "input/geoboundaries/geoBoundariesCGAZ_ADM0/geoBoundariesCGAZ_ADM0.shp"
download_dir <- "input/worldpop_raw"   # raw per-band .tif files
pop_dir      <- "input/pop"            # processed (summed) group rasters

# ============================================================
# MODE 1 — GLOBAL 1km (default, no clipping)
# ============================================================
message("\n========================================")
message(" WorldPop pipeline: ", ctry_code, " ", year, " | global 1km")
message("========================================\n")

pop_global <- wp_build_all_groups(
  iso3         = ctry_code,
  year         = year,
  download_dir = download_dir,
  pop_dir      = pop_dir,
  scope        = "global"       # 1km unconstrained, no boundary clipping
)

# ============================================================
# MODE 2 — COUNTRY 100m (uncomment if needed)
# ============================================================
# admin0      <- sf::read_sf(admin0_path)
# ctry_admin0 <- admin0 |> dplyr::filter(shapeGroup == ctry_code)
#
# pop_country <- wp_build_all_groups(
#   iso3         = ctry_code,
#   year         = year,
#   download_dir = download_dir,
#   pop_dir      = pop_dir,
#   scope        = "country",   # 100m constrained UN-adj, clipped to boundary
#   boundary     = ctry_admin0
# )

# SUMMARY ----------------------------------------------------------------------
message("\nAll group rasters saved to: ", pop_dir)
message("Groups produced:")
for (nm in names(pop_global)) {
  r <- pop_global[[nm]]

  message(sprintf("  %-30s  cells: %d  total pop: %.0f",
                  nm, terra::ncell(r),
                  sum(terra::values(r), na.rm = TRUE)))
}
