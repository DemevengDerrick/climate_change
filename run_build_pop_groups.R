#################################################################################################
#                                                                                               #
#               WORLDPOP GLOBAL DEMOGRAPHIC GROUP RASTERS — PRE-PROCESSING                     #
#                               By Derrick DEMEVENG                                            #
#                                                                                               #
#  Purpose: Download WorldPop global age-sex band rasters for a given year and sum them        #
#           into 9 standard demographic group rasters at 1km global resolution.                #
#                                                                                               #
#  Data     : WorldPop Global 1km Constrained UN-adjusted (R2025A)                            #
#  Source   : https://hub.worldpop.org/geodata/listing?id=75                                   #
#                                                                                               #
#  Pre-requisites: None. This script must be run before any hazard exposure script.            #
#                                                                                               #
#  Outputs:                                                                                     #
#    input/worldpop_raw/{year}/  — raw age-sex band GeoTIFF files (~15 GB, 36 files)           #
#    input/pop_global/{year}/   — summed demographic group rasters (9 groups)                  #
#      global_{group}_{year}_CN_1km.tif                                                        #
#                                                                                               #
#  Groups:                                                                                      #
#    total_pop, women_15_49, youth_15_24, adolescent_10_19, children_u5,                      #
#    youth_women_15_24, adolescent_girls_10_19, pop_65plus, women_65plus                       #
#                                                                                               #
#  Note: Already-existing files are skipped automatically. Safe to re-run.                     #
#                                                                                               #
#  Run: source("run_build_pop_groups.R")                                                       #
#                                                                                               #
#################################################################################################

# LOAD LIBRARIES -------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, purrr, terra)

# LOAD UTILITY FUNCTIONS -----------------------------------------------
source("R/worldpop_data.R")
source("R/pop_utils.R")

# ======================================================================
# INPUT PARAMETERS
# ======================================================================
year         <- 2020
download_dir <- file.path("input/worldpop_raw", year)
global_pop_dir <- file.path("input/pop_global", year)

dir.create(download_dir,   recursive = TRUE, showWarnings = FALSE)
dir.create(global_pop_dir, recursive = TRUE, showWarnings = FALSE)

# Tell terra to use at most 30% of RAM and spill the rest to temp files.
terra::terraOptions(memfrac = 0.3)


# ======================================================================
# STEP 0 — DOWNLOAD WORLDPOP BAND FILES FOR TARGET YEAR
# ======================================================================
# Downloads only missing files; already-downloaded files are skipped.
# ~36 global 1km GeoTIFF files, approximately 15 GB total.

message("\n=== STEP 0: Downloading WorldPop band files for year ", year, " ===")

dl_results <- wp_download(
  iso3         = NA,
  year         = year,
  download_dir = download_dir,
  scope        = "global"
)

n_downloaded <- sum(dl_results$status == "downloaded")
n_skipped    <- sum(dl_results$status == "skipped")
n_failed     <- sum(dl_results$status == "failed")

message(sprintf("  Downloaded: %d  |  Skipped (exists): %d  |  Failed: %d",
                n_downloaded, n_skipped, n_failed))

if (n_failed > 0) {
  message("  [WARN] Some files failed to download:")
  message(paste0("    ", dl_results$local_path[dl_results$status == "failed"],
                 collapse = "\n"))
  message("  Re-run to retry failed files. Continuing with available files ...")
}


# ======================================================================
# STEP 1 — BUILD GLOBAL WORLDPOP GROUP RASTERS (once per year)
# ======================================================================
# For each demographic group, sums the relevant age-sex band rasters into a
# single global raster at 1km resolution.  Output files are named:
#   global_{group}_{year}_CN_1km.tif

message("\n=== STEP 1: Building global WorldPop group rasters ===")

build_global_group_raster <- function(group_name, sex, ages,
                                       download_dir, out_dir, year,
                                       overwrite = FALSE) {
  out_path <- file.path(
    out_dir,
    sprintf("global_%s_%d_CN_1km.tif", group_name, as.integer(year))
  )

  if (file.exists(out_path) && !overwrite) {
    message("  [skip] ", basename(out_path), " already exists.")
    return(invisible(out_path))
  }

  combos <- expand.grid(sex = sex, age = as.integer(ages),
                        stringsAsFactors = FALSE)
  paths  <- purrr::pmap_chr(combos, function(sex, age) {
    file.path(download_dir,
              sprintf("global_%s_%02d_%d_CN_1km_R2025A_UA_v1.tif", sex, age, year))
  })

  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) {
    message("  [WARN] ", group_name, ": ", length(missing),
            " band file(s) missing — skipping group.")
    return(invisible(NULL))
  }

  message("  Summing ", length(paths), " bands -> ", basename(out_path), " ...")
  summed        <- Reduce("+", purrr::map(paths, terra::rast))
  names(summed) <- group_name
  terra::writeRaster(summed, out_path, overwrite = TRUE,
                     gdal = c("COMPRESS=DEFLATE", "TILED=YES",
                               "BLOCKXSIZE=512", "BLOCKYSIZE=512"))
  message("  -> Written: ", out_path)
  invisible(out_path)
}

global_group_paths <- purrr::imap(WP_GROUPS, function(grp, gname) {
  build_global_group_raster(
    group_name   = gname,
    sex          = grp$sex,
    ages         = grp$ages,
    download_dir = download_dir,
    out_dir      = global_pop_dir,
    year         = year
  )
}) |> purrr::compact()

if (length(global_group_paths) == 0)
  stop("No global group rasters could be built. Check band files in: ", download_dir)

message("\nGlobal group rasters ready: ", length(global_group_paths), " / ",
        length(WP_GROUPS), " groups.")
message("Location: ", normalizePath(global_pop_dir))
message("\nGroups written:")
for (nm in names(global_group_paths)) {
  message("  ", basename(global_group_paths[[nm]]))
}
message("\n=== Done ===")
message("You can now run any hazard exposure script:")
message("  source(\"run_flood_exposure.R\")")
message("  source(\"run_lecz_exposure.R\")")
