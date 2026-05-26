#################################################################################################
#                                                                                               #
#              GLOBAL LOW ELEVATION COASTAL ZONE (LECZ) EXPOSURE — ADMIN0 LEVEL                #
#                               By Derrick DEMEVENG                                            #
#                                                                                               #
#  Purpose: Compute population exposure to Low Elevation Coastal Zones (LECZ) for all         #
#           countries globally at admin0 level across 9 WorldPop demographic groups.           #
#                                                                                               #
#  Hazard    : LECZ — pixel value 1 in lecz_1km_popgrid.tif (built by run_preprocess_lecz.R)  #
#  Boundaries: UNFPA/WHO Official Boundaries  (unfpa_admin0.shp)  — key: ISO3CD               #
#  Population: WorldPop Global 1km Constrained UN-adjusted (R2025A)  — year configurable      #
#                                                                                               #
#  Pre-requisites:                                                                              #
#    Run run_build_pop_groups.R first to build population group rasters.                       #
#    Run run_flood_exposure.R first (Steps 3+) to build the admin0 zone raster.               #
#    input/pop_global/{year}/global_{group}_{year}_CN_1km.tif   — group rasters               #
#    output/global/vrt/unfpa0_zones.tif                          — zone raster                 #
#                                                                                               #
#  Strategy:                                                                                    #
#    Step 1 — Build binary LECZ mask (pixel == 5 → 1, else NA).                               #
#    Step 2 — Build LECZ-exposed population rasters (pop × mask) per group (once per year).   #
#    Step 3 — Run raster × raster zonal statistics (tile-by-tile, memory efficient).          #
#    Step 4 — Join results to admin0 geometry → GeoPackage + Excel.                           #
#                                                                                               #
#  Key outputs (in output/global/lecz/):                                                        #
#    global_unfpa0_lecz_exposure.gpkg   (wide format, mapping-ready)                          #
#    global_unfpa0_lecz_exposure.xlsx   (long format, one row per country×group)              #
#                                                                                               #
#  Run: source("run_lecz_exposure.R")                                                          #
#                                                                                               #
#################################################################################################

# LOAD LIBRARIES -------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, stringr, sf, purrr, tibble, tidyr,
  terra, openxlsx
)

# LOAD UTILITY FUNCTIONS -----------------------------------------------
source("R/worldpop_data.R")
source("R/pop_utils.R")
source("R/exposure_utils.R")

# ======================================================================
# INPUT PARAMETERS
# ======================================================================
year         <- 2020
hazard_name  <- "Low Elevation Coastal Zone"
hazard_code  <- "lecz"
lecz_value   <- 1      # pixel value that identifies LECZ cells (1 = LECZ in lecz_1km_popgrid.tif)

lecz_path      <- "input/LECZ/lecz_1km_popgrid.tif"
admin0_path    <- "input/unfpa_who_boundaries/shp/unfpa_admin0.shp"
global_pop_dir <- file.path("input/pop_global", year)
zone_raster_path <- "output/global/vrt/unfpa0_zones.tif"   # built by run_flood_exposure.R

out_global <- "output/global/lecz"
out_gpkg   <- file.path(out_global, "global_unfpa0_lecz_exposure.gpkg")
out_excel  <- file.path(out_global, "global_unfpa0_lecz_exposure.xlsx")

exposed_lecz_dir <- file.path(out_global, "exposed_rasters", year)
dir.create(out_global,        recursive = TRUE, showWarnings = FALSE)
dir.create(exposed_lecz_dir,  recursive = TRUE, showWarnings = FALSE)

# Tell terra to use at most 30% of RAM and spill the rest to temp files.
terra::terraOptions(memfrac = 0.3)


# ======================================================================
# CHECK PRE-REQUISITES
# ======================================================================
if (!file.exists(zone_raster_path))
  stop("Zone raster not found: ", zone_raster_path,
       "\nRun run_flood_exposure.R (Step 3) first to build the admin0 zone raster.")

pop_files <- list.files(global_pop_dir, pattern = "\\.tif$", full.names = TRUE)
if (length(pop_files) == 0)
  stop("No population group rasters found in: ", global_pop_dir,
       "\nRun run_build_pop_groups.R first to build the global group rasters.")

# Build named list of group raster paths
global_group_paths <- purrr::imap(
  stats::setNames(names(WP_GROUPS), names(WP_GROUPS)),
  function(gname, ...) {
    p <- file.path(global_pop_dir,
                   sprintf("global_%s_%d_CN_1km.tif", gname, as.integer(year)))
    if (file.exists(p)) p else NULL
  }
) |> purrr::compact()

if (length(global_group_paths) == 0)
  stop("No matching group rasters found in: ", global_pop_dir)

message("Population group rasters found: ", length(global_group_paths), " groups.")


# ======================================================================
# STEP 1 — BUILD BINARY LECZ MASK
# ======================================================================
message("\n=== STEP 1: Building LECZ binary mask ===")

lecz_raw  <- terra::rast(lecz_path)
lecz_mask <- terra::ifel(lecz_raw == lecz_value, 1L, NA_integer_)

message("  LECZ raster loaded: ", nrow(lecz_raw), " rows × ", ncol(lecz_raw), " cols")
message("  Mask: cells with value ", lecz_value, " → 1; all others → NA")


# ======================================================================
# STEP 2 — BUILD LECZ-EXPOSED POPULATION RASTERS PER GROUP (once)
# ======================================================================
# For each demographic group: exposed_pop = population × lecz_mask
# LECZ mask is binary (1 = in LECZ, NA = outside), so the product gives
# population counts only within LECZ cells.

message("\n=== STEP 2: Building LECZ-exposed population rasters ===")

exposed_lecz_paths <- purrr::imap(global_group_paths, function(grp_path, gname) {
  out_path <- file.path(exposed_lecz_dir,
                         sprintf("lecz_exposed_%s_%d.tif", gname, as.integer(year)))

  if (file.exists(out_path)) {
    message("  [skip] ", basename(out_path), " already exists.")
    return(invisible(out_path))
  }

  message("  Computing: ", basename(out_path), " ...")
  terra::lapp(
    terra::sds(terra::rast(grp_path), lecz_mask),
    fun       = function(p, m) p * m,
    filename  = out_path,
    overwrite = TRUE,
    wopt      = list(gdal = c("COMPRESS=DEFLATE", "TILED=YES",
                               "BLOCKXSIZE=512", "BLOCKYSIZE=512"))
  )
  message("  -> Written: ", out_path)
  invisible(out_path)
})

message("LECZ-exposed rasters ready in: ", exposed_lecz_dir)


# ======================================================================
# STEP 3 — ZONAL STATISTICS (raster × raster — memory efficient)
# ======================================================================
message("\n=== STEP 3: Running zonal statistics ===")

zone_ras <- terra::rast(zone_raster_path)

# Load admin0 table for joining results
admin0 <- sf::read_sf(admin0_path) |>
  dplyr::mutate(shapeGroup = ISO3CD, shapeID = ISO3CD, shapeName = name,
                zone_id = dplyr::row_number())

all_indicators <- purrr::imap_dfr(exposed_lecz_paths, function(exp_path, gname) {
  meta_row <- dplyr::filter(WP_GROUP_META, group_name == gname)
  label    <- if (nrow(meta_row) > 0) meta_row$label[1]     else gname
  sex_val  <- if (nrow(meta_row) > 0) meta_row$sex[1]       else NA_character_
  age_val  <- if (nrow(meta_row) > 0) meta_row$age_group[1] else NA_character_

  message("  -> ", label)

  pop_path <- global_group_paths[[gname]]

  # Raster × raster zonal sums — tile-by-tile, low RAM
  z_exposed <- terra::zonal(terra::rast(exp_path), zone_ras,
                             fun = "sum", na.rm = TRUE)
  z_total   <- terra::zonal(terra::rast(pop_path), zone_ras,
                             fun = "sum", na.rm = TRUE)

  # Join back to admin0 via zone_id
  result <- admin0 |>
    sf::st_drop_geometry() |>
    dplyr::select(zone_id, shapeGroup, shapeID, shapeName) |>
    dplyr::left_join(
      dplyr::rename(z_total,   total.pop   = 2), by = c("zone_id" = names(z_total)[1])
    ) |>
    dplyr::left_join(
      dplyr::rename(z_exposed, pop.exposed = 2), by = c("zone_id" = names(z_exposed)[1])
    )

  tibble::tibble(
    scenario         = "lecz_exposure",
    hazard.name      = hazard_name,
    hazard.code      = hazard_code,
    return.period    = NA_integer_,
    indicator.name   = paste0(label, " living in ", hazard_name),
    indicator.code   = paste0(gname, "_", hazard_code),
    group.label      = label,
    sex              = sex_val,
    age.group        = age_val,
    country.code     = result$shapeGroup,
    admin.level      = 0L,
    admin.code       = result$shapeID,
    admin.name       = result$shapeName,
    year             = as.integer(year),
    total.pop        = result$total.pop,
    pop.exposed      = result$pop.exposed,
    perc.pop.exposed = ifelse(result$total.pop > 0,
                               (result$pop.exposed / result$total.pop) * 100,
                               NA_real_)
  )
})

all_indicators <- dplyr::mutate(all_indicators, id = dplyr::row_number())
message("Total rows: ", nrow(all_indicators),
        "  (", length(unique(all_indicators$country.code)), " countries × ",
        length(unique(all_indicators$group.label)), " groups)")


# ======================================================================
# STEP 4 — JOIN TO ADMIN0 GEOMETRY AND EXPORT
# ======================================================================
message("\n=== STEP 4: Exporting results ===")

# ---- Excel -----------------------------------------------------------
message("Writing Excel: ", out_excel)
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Admin0 LECZ Exposure")
openxlsx::writeData(wb, "Admin0 LECZ Exposure", all_indicators)

openxlsx::saveWorkbook(wb, out_excel, overwrite = TRUE)
message("  -> ", out_excel)

# ---- Wide pivot for GeoPackage ---------------------------------------
safe_colname <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("_+", "_") |>
    stringr::str_remove("^_|_$")
}

wide <- all_indicators |>
  dplyr::select(admin.code, group.label, total.pop, pop.exposed, perc.pop.exposed) |>
  dplyr::mutate(grp = safe_colname(group.label)) |>
  tidyr::pivot_wider(
    id_cols     = admin.code,
    names_from  = grp,
    values_from = c(total.pop, pop.exposed, perc.pop.exposed),
    names_glue  = "{.value}_{grp}"
  )

# ---- GeoPackage ------------------------------------------------------
message("Joining to UNFPA admin0 geometry and writing GeoPackage: ", out_gpkg)
admin0_export <- admin0 |>
  dplyr::select(ISO3CD, name, m49_code, unfpa_regi, un_develop, is_ldc) |>
  dplyr::left_join(wide, by = c("ISO3CD" = "admin.code"))

sf::write_sf(admin0_export, out_gpkg,
             layer        = "unfpa0_lecz_exposure",
             driver       = "GPKG",
             delete_layer = TRUE)
message("  -> ", out_gpkg)

message("\n=== Done ===")
message("Outputs:")
message("  Excel:      ", normalizePath(out_excel))
message("  GeoPackage: ", normalizePath(out_gpkg))
message("\nGeoPackage layer : 'unfpa0_lecz_exposure'  |  Key: ISO3CD")
message("Column convention:")
message("  total_pop_{group}        = total group population")
message("  pop_exposed_{group}      = population living in LECZ")
message("  perc_pop_exposed_{group} = % of group living in LECZ")
