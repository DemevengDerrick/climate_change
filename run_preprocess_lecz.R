#################################################################################################
#                                                                                               #
#               LECZ PREPROCESSING — RESAMPLE TO WORLDPOP 1KM POPULATION GRID                 #
#                               By Derrick DEMEVENG                                            #
#                                                                                               #
#  Purpose: Resample the raw MERIT LECZ raster to the exact WorldPop 1km global grid,         #
#           producing a binary mask (1 = LECZ, NA = outside LECZ) ready for use in            #
#           run_lecz_exposure.R.                                                                #
#                                                                                               #
#  Input:   input/LECZ/.../merit_leczs.tif  — raw MERIT LECZ classification (~90m)            #
#  Template: input/pop_global/{year}/global_total_pop_{year}_CN_1km.tif                        #
#                                                                                               #
#  Output:  input/LECZ/lecz_1km_popgrid.tif                                                    #
#             Value 1  = Low Elevation Coastal Zone cell                                        #
#             Value NA = Not in LECZ                                                            #
#                                                                                               #
#  Method:  Raw LECZ is binarised (any LECZ class → 1, else 0), then projected to the         #
#           population grid using mode (majority) resampling to preserve the binary            #
#           classification without artefacts from averaging.                                    #
#                                                                                               #
#  Run: source("run_preprocess_lecz.R")                                                        #
#                                                                                               #
#################################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(terra)

# ======================================================================
# PATHS
# ======================================================================
raw_lecz_path <- paste0(
  "input/LECZ/lecz-urban-rural-population-land-area-estimates-v3-merit-leczs-geotiff/",
  "lecz_v3_spatial_data/data/merit_leczs.tif"
)
year           <- 2020
pop_dir        <- file.path("input/pop_global", year)
out_path       <- "input/LECZ/lecz_1km_popgrid.tif"

terra::terraOptions(memfrac = 0.3)

# ======================================================================
# STEP 1 — INSPECT RAW LECZ VALUES
# ======================================================================
message("Loading raw LECZ raster ...")
lecz_raw <- terra::rast(raw_lecz_path)

message("  Resolution : ", paste(round(terra::res(lecz_raw), 6), collapse = " × "), " deg")
message("  Dimensions : ", nrow(lecz_raw), " rows × ", ncol(lecz_raw), " cols")
message("  Extent     : ", paste(round(as.vector(terra::ext(lecz_raw)), 4), collapse = ", "))
message("  CRS        : ", terra::crs(lecz_raw, describe = TRUE)$name)

message("\nSampling unique values (may take a moment on large rasters) ...")
uvals <- terra::unique(lecz_raw)[[1]]
message("  Unique values found: ", paste(sort(uvals), collapse = ", "))
message("  (Pixel value == 5 will be treated as LECZ cells.)")

# ======================================================================
# STEP 2 — BINARY RECLASSIFY AT NATIVE RESOLUTION
# ======================================================================
# Value 5 = Low Elevation Coastal Zone → 1
# All other values (including NoData)  → 0
lecz_raw_value <- 5L
message("\nCreating binary LECZ mask (value == ", lecz_raw_value, " → 1, else → 0) ...")
lecz_binary <- terra::ifel(lecz_raw == lecz_raw_value, 1L, 0L)

# ======================================================================
# STEP 3 — PROJECT TO POPULATION GRID (mode resampling)
# ======================================================================
# Use the total_pop raster as the exact grid template.
pop_ref_candidates <- list.files(pop_dir,
  pattern = "^global_total_pop_.*_CN_1km\\.tif$", full.names = TRUE)
if (length(pop_ref_candidates) == 0)
  stop("No global total_pop raster found in: ", pop_dir,
       "\nRun run_build_pop_groups.R first.")

pop_template <- terra::rast(pop_ref_candidates[1])
message("\nPopulation grid template: ", basename(pop_ref_candidates[1]))
message("  Resolution : ", paste(round(terra::res(pop_template), 6), collapse = " × "), " deg")
message("  Extent     : ", paste(round(as.vector(terra::ext(pop_template)), 4), collapse = ", "))

message("\nProjecting binary LECZ mask to population grid (method = 'mode') ...")
message("  This may take several minutes for a global raster ...")

lecz_1km <- terra::project(lecz_binary, pop_template, method = "mode")

# Convert back to 1/NA: cells with mode value 1 stay as 1; 0-majority cells → NA
lecz_1km_mask <- terra::ifel(lecz_1km == 1L, 1L, NA_integer_)

# ======================================================================
# STEP 4 — WRITE OUTPUT
# ======================================================================
message("\nWriting output: ", out_path)
terra::writeRaster(
  lecz_1km_mask,
  out_path,
  overwrite = TRUE,
  gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BLOCKXSIZE=512", "BLOCKYSIZE=512")
)

# Quick validation
n_lecz <- terra::global(lecz_1km_mask, fun = "notNA")[[1]]
message("  -> Written: ", out_path)
message("  LECZ cells at 1km  : ", format(n_lecz, big.mark = ","))
message("  Extent             : ",
        paste(round(as.vector(terra::ext(lecz_1km_mask)), 4), collapse = ", "))
message("  Matches pop grid   : ",
        all(terra::ext(lecz_1km_mask) == terra::ext(pop_template)) &&
        all(terra::res(lecz_1km_mask) == terra::res(pop_template)))

message("\n=== Done ===")
message("Compare with your version at: input/LECZ/resampled_lecz.tif")
message("To use in exposure analysis, update run_lecz_exposure.R:")
message("  lecz_path  <- \"input/LECZ/lecz_1km_popgrid.tif\"")
message("  lecz_value <- 1")
