#################################################################################################
#                                                                                               #
#                   GLOBAL FLOOD EXPOSURE ANALYSIS — ADMIN0 LEVEL                              #
#                               By Derrick DEMEVENG                                            #
#                                                                                               #
#  Purpose: Compute river flood exposure (9 WorldPop demographic groups) for all countries     #
#           globally at admin0 level using a memory-efficient raster zonal statistics          #
#           approach.                                                                           #
#                                                                                               #
#  Boundaries : UNFPA/WHO Official Boundaries  (unfpa_admin0.shp)                             #
#  Population : WorldPop Global 1km Constrained UN-adjusted (R2025A)  — year configurable     #
#  Flood      : GloFAS River Flood Hazard RP100                                                #
#  Admin key  : ISO3CD                                                                         #
#                                                                                               #
#  Pre-requisite: Run run_build_pop_groups.R first to download WorldPop and build group        #
#                 rasters in input/pop_global/{year}/.                                         #
#                                                                                               #
#  Strategy:                                                                                    #
#    Step 1 — Build global flood fraction raster aligned to population grid (once).            #
#    Step 2 — Build exposed population rasters (pop × flood_frac) per group (once per year).  #
#    Step 3 — Rasterise UNFPA admin0 zones to integer ID raster (once).                       #
#    Step 4 — Run raster × raster zonal statistics (tile-by-tile, memory efficient).          #
#    Step 5 — Join results to admin0 geometry → GeoPackage + Excel.                           #
#                                                                                               #
#  Re-use:  Step 2 only needs re-running when the population year changes.                     #
#           Steps 3-5 only need re-running when boundaries or hazard change.                   #
#                                                                                               #
#  Key outputs (in output/global/flood/):                                                       #
#    global_unfpa0_flood_exposure.gpkg   (wide format, mapping-ready)                         #
#    global_unfpa0_flood_exposure.xlsx   (long format, one row per country × group)            #
#                                                                                               #
#  Run: source("run_flood_exposure.R")                                                         #
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
source("R/flood_utils.R")
source("R/pop_utils.R")
source("R/exposure_utils.R")

# ======================================================================
# INPUT PARAMETERS
# ======================================================================
return_period   <- 100
flood_threshold <- 0
year            <- 2020
hazard_name     <- "River Flood"
hazard_code     <- "riv_flood"

admin0_path    <- "input/unfpa_who_boundaries/shp/unfpa_admin0.shp"
flood_dir      <- "input/flood_layers_RP100/"
global_pop_dir <- file.path("input/pop_global", year)

out_global <- file.path("output/global/flood")
out_gpkg   <- file.path(out_global, "global_unfpa0_flood_exposure.gpkg")
out_excel  <- file.path(out_global, "global_unfpa0_flood_exposure.xlsx")

exposed_raster_dir <- file.path(out_global, "exposed_rasters", year)

dir.create(out_global,          recursive = TRUE, showWarnings = FALSE)
dir.create(exposed_raster_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create("output/global/vrt", recursive = TRUE, showWarnings = FALSE)

# Tell terra to use at most 30% of RAM and spill the rest to temp files.
terra::terraOptions(memfrac = 0.3)


# ======================================================================
# CHECK PRE-REQUISITES
# ======================================================================
pop_files <- list.files(global_pop_dir, pattern = "\\.tif$", full.names = TRUE)
if (length(pop_files) == 0)
  stop("No population group rasters found in: ", global_pop_dir,
       "\nRun run_build_pop_groups.R first.")

global_group_paths <- purrr::imap(
  stats::setNames(names(WP_GROUPS), names(WP_GROUPS)),
  function(gname, ...) {
    p <- file.path(global_pop_dir,
                   sprintf("global_%s_%d_CN_1km.tif", gname, as.integer(year)))
    if (file.exists(p)) p else NULL
  }
) |> purrr::compact()

if (length(global_group_paths) == 0)
  stop("No matching group rasters found in: ", global_pop_dir,
       "\nRun run_build_pop_groups.R first.")

message("Population group rasters found: ", length(global_group_paths), " groups.")


# ======================================================================
# STEP 1 — BUILD GLOBAL FLOOD FRACTION RASTER (once, year-independent)
# ======================================================================
# The flood hazard layer (GloFAS RP100) does not change between years.
# The WorldPop global 1km grid is also stable across years.
# => flood_frac.tif from a previous run is fully reusable as-is.

message("\n=== STEP 1: Building global flood fraction raster ===")

all_flood_tiles <- list.files(flood_dir, pattern = "\\.tif$", full.names = TRUE)
if (length(all_flood_tiles) == 0)
  stop("No flood tiles found in: ", flood_dir)
message("Found ", length(all_flood_tiles), " flood tiles.")

global_vrt_path <- file.path("output/global/vrt",
                              paste0("global_RP", return_period, "_depth.vrt"))
flood_frac_path <- file.path("output/global/vrt",
                              paste0("global_RP", return_period, "_flood_frac.tif"))

if (!file.exists(global_vrt_path)) {
  message("Building global flood mosaic VRT ...")
  terra::vrt(all_flood_tiles, filename = global_vrt_path, overwrite = TRUE)
  message("  -> ", global_vrt_path)
} else {
  message("  [skip] Global flood VRT already exists.")
}

if (!file.exists(flood_frac_path)) {
  message("Computing global flood fraction raster ...")
  pop_ref_candidates <- list.files(global_pop_dir,
    pattern = "^global_total_pop_.*_CN_1km\\.tif$", full.names = TRUE)
  if (length(pop_ref_candidates) == 0)
    stop("No global total_pop raster found in: ", global_pop_dir,
         "\nRun run_build_pop_groups.R first.")
  pop_ref_global <- terra::rast(pop_ref_candidates[1])
  flood_frac <- fld_aggregate_to_pop(
    fld_binarise(terra::rast(global_vrt_path), flood_threshold),
    pop_ref_global
  )
  terra::writeRaster(flood_frac, flood_frac_path, overwrite = TRUE,
                     gdal = c("COMPRESS=DEFLATE", "TILED=YES",
                               "BLOCKXSIZE=512", "BLOCKYSIZE=512"))
  message("  -> Written: ", flood_frac_path)
} else {
  message("  [skip] Global flood fraction raster already exists.")
}

flood_frac_global <- terra::rast(flood_frac_path)


# ======================================================================
# STEP 2 — BUILD EXPOSED POPULATION RASTERS PER GROUP (once, reusable)
# ======================================================================
# Pre-compute pop × flood_frac for every demographic group and save to disk.
# Any future admin-level zonal stats (admin0, admin1, admin2) skip this step.

message("\n=== STEP 2: Building exposed population rasters ===")

exposed_raster_paths <- purrr::imap(global_group_paths, function(grp_path, gname) {
  out_path <- file.path(exposed_raster_dir,
                         sprintf("exposed_%s_%d_RP%d.tif",
                                 gname, as.integer(year), return_period))

  if (file.exists(out_path)) {
    message("  [skip] ", basename(out_path), " already exists.")
    return(invisible(out_path))
  }

  message("  Computing exposed raster: ", basename(out_path), " ...")
  terra::lapp(
    terra::sds(terra::rast(grp_path), flood_frac_global),
    fun       = function(p, f) p * f,
    filename  = out_path,
    overwrite = TRUE,
    wopt      = list(gdal = c("COMPRESS=DEFLATE", "TILED=YES",
                               "BLOCKXSIZE=512", "BLOCKYSIZE=512"))
  )
  message("  -> Written: ", out_path)
  invisible(out_path)
})

message("Exposed rasters ready in: ", exposed_raster_dir)


# ======================================================================
# STEP 3 — RASTERISE ADMIN ZONES (once, reusable across hazards)
# ======================================================================
# Pre-rasterising zones to an integer ID raster lets terra::zonal() work
# with two rasters (tile-by-tile) instead of polygon overlay — far less RAM.

message("\n=== STEP 3: Rasterising admin0 zones ===")

admin0 <- sf::read_sf(admin0_path) |>
  dplyr::mutate(shapeGroup = ISO3CD, shapeID = ISO3CD, shapeName = name,
                zone_id = dplyr::row_number())

message("  -> ", nrow(admin0), " countries loaded.")

zone_raster_path <- "output/global/vrt/unfpa0_zones.tif"

if (!file.exists(zone_raster_path)) {
  message("  Rasterising admin0 zones ...")
  template <- terra::rast(file.path(
    global_pop_dir,
    sprintf("global_total_pop_%d_CN_1km.tif", as.integer(year))
  ))
  zones_v  <- terra::vect(admin0)
  zone_ras <- terra::rasterize(zones_v, template, field = "zone_id",
                                filename  = zone_raster_path,
                                overwrite = TRUE,
                                wopt = list(gdal = c("COMPRESS=DEFLATE",
                                                      "TILED=YES")))
  message("  -> Written: ", zone_raster_path)
} else {
  message("  [skip] Zone raster already exists.")
  zone_ras <- terra::rast(zone_raster_path)
}


# ======================================================================
# STEP 4 — ZONAL STATISTICS (raster × raster — memory efficient)
# ======================================================================
message("\n=== STEP 4: Running zonal statistics ===")

all_indicators <- purrr::imap_dfr(exposed_raster_paths, function(exp_path, gname) {
  meta_row <- dplyr::filter(WP_GROUP_META, group_name == gname)
  label    <- if (nrow(meta_row) > 0) meta_row$label[1]     else gname
  sex_val  <- if (nrow(meta_row) > 0) meta_row$sex[1]       else NA_character_
  age_val  <- if (nrow(meta_row) > 0) meta_row$age_group[1] else NA_character_

  message("  -> ", label)

  pop_path <- global_group_paths[[gname]]

  # Raster × raster zonal — terra streams tiles, never loads full raster
  z_exposed <- terra::zonal(terra::rast(exp_path), zone_ras,
                             fun = "sum", na.rm = TRUE)
  z_total   <- terra::zonal(terra::rast(pop_path), zone_ras,
                             fun = "sum", na.rm = TRUE)

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
    scenario         = "raw_exposure",
    hazard.name      = hazard_name,
    hazard.code      = hazard_code,
    return.period    = as.integer(return_period),
    indicator.name   = paste0(label, " exposed to ", hazard_name,
                               " (RP", return_period, ")"),
    indicator.code   = paste0(gname, "_", hazard_code, "_RP", return_period),
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
# STEP 5 — JOIN TO ADMIN0 GEOMETRY AND EXPORT
# ======================================================================
message("\n=== STEP 5: Exporting results ===")

# ---- Excel -----------------------------------------------------------
message("Writing Excel: ", out_excel)
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Admin0 Flood Exposure")
openxlsx::writeData(wb, "Admin0 Flood Exposure", all_indicators)
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
  dplyr::group_by(admin.code, grp) |>
  dplyr::summarise(
    total.pop        = sum(total.pop,   na.rm = TRUE),
    pop.exposed      = sum(pop.exposed, na.rm = TRUE),
    perc.pop.exposed = ifelse(sum(total.pop, na.rm = TRUE) > 0,
                               sum(pop.exposed, na.rm = TRUE) /
                               sum(total.pop,   na.rm = TRUE) * 100,
                               NA_real_),
    .groups = "drop"
  ) |>
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
             layer        = "unfpa0_flood_exposure",
             driver       = "GPKG",
             delete_layer = TRUE)
message("  -> ", out_gpkg)

message("\n=== Done ===")
message("Outputs:")
message("  Excel:      ", normalizePath(out_excel))
message("  GeoPackage: ", normalizePath(out_gpkg))
message("\nGeoPackage layer: 'unfpa0_flood_exposure'  |  Key: ISO3CD")
message("Column convention:")
message("  total_pop_{group}        = total group population")
message("  pop_exposed_{group}      = flood-exposed population count")
message("  perc_pop_exposed_{group} = % of group exposed to flooding")
