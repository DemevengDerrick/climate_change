#################################################################################################
#                                                                                               #
#                       DEGREE OF URBANISATION (DEGURBA) UTILITY FUNCTIONS                     #
#                                 By Derrick DEMEVENG                                           #
#                                                                                               #
#  Source: GHS-DUG (Global Human Settlement - Degree of Urbanisation), R2023A                  #
#  Grids: GHS-DUG_GRID_L1.tif (3 classes) and GHS-DUG_GRID_L2.tif (8 classes)                 #
#  Native CRS: Mollweide (ESRI:54009), 1 km resolution                                         #
#                                                                                               #
#################################################################################################


# ---------------------------------------------------------------------------
# CLASS LOOKUP TABLES
# ---------------------------------------------------------------------------

#' DEGURBA Level 1 class labels (code → label)
DEG_L1_LABELS <- c(
  "1" = "Rural",
  "2" = "Urban Cluster",
  "3" = "Urban Centre"
)

#' DEGURBA Level 2 class labels (code → label)
DEG_L2_LABELS <- c(
  "10" = "Water",
  "11" = "Very Low Density Rural",
  "12" = "Low Density Rural",
  "13" = "Rural Cluster",
  "21" = "Suburban / Peri-Urban",
  "22" = "Semi-Dense Urban Cluster",
  "23" = "Dense Urban Cluster",
  "30" = "Urban Centre"
)


# ---------------------------------------------------------------------------
# ALIGNMENT
# ---------------------------------------------------------------------------

#' Align a DEGURBA categorical raster to a population raster grid
#'
#' Reprojects from Mollweide (native DEGURBA CRS) to the population CRS, then
#' resamples using nearest-neighbour — mandatory for categorical integer data.
#' Bilinear resampling would corrupt class codes by averaging integers.
#'
#' @param dug_path Character. Path to the DEGURBA GeoTIFF.
#' @param pop_ref  SpatRaster. Reference population raster (defines target grid).
#'
#' @return SpatRaster aligned to `pop_ref` grid, integer class codes preserved.
#' @export
deg_align_to_pop <- function(dug_path, pop_ref) {
  dug <- terra::rast(dug_path)
  if (!terra::same.crs(dug, pop_ref))
    dug <- terra::project(dug, pop_ref, method = "near")
  terra::resample(dug, pop_ref, method = "near")
}


# ---------------------------------------------------------------------------
# CORE EXPOSURE ANALYSIS
# ---------------------------------------------------------------------------

#' Run DEGURBA-stratified flood exposure analysis for all demographic groups
#'
#' For every demographic group raster in `pop_dir`, computes country-level
#' population, raw flood exposure, and (optionally) poor-population flood
#' exposure — stratified by DEGURBA urbanisation class.
#'
#' The DEGURBA raster is used as the zone variable in `terra::zonal()`.
#' Cells outside the country boundary are masked to NA through `pop_clip()`,
#' so they never contribute to any zone sum.
#'
#' @param pop_dir        Character. Directory with processed group rasters
#'   (`{iso3}_{group}_{year}_{scope}_{res}.tif`).
#' @param flood_frac_ras SpatRaster. Hazard fraction raster (0–1).
#' @param dug_ras        SpatRaster. Aligned DEGURBA raster (integer class codes).
#'   Must share the same CRS and resolution as the population rasters.
#' @param dep_mask       SpatRaster or NULL. Binary poverty mask (1 = in poverty).
#'   When provided, poor-population columns are populated.
#' @param level_labels   Named character vector. Maps class codes (as character)
#'   to human-readable labels. Use [DEG_L1_LABELS] or [DEG_L2_LABELS].
#' @param ctry_boundary  sf or SpatVector. Country boundary for clipping.
#' @param group_meta     Tibble. Demographic group metadata. Defaults to
#'   [WP_GROUP_META].
#' @param hazard_name    Character. Full hazard name (default `"River Flood"`).
#' @param return_period  Integer. Return period in years (default `100L`).
#' @param year           Integer. Population reference year (default `2020L`).
#'
#' @return Tibble with one row per demographic group × DEGURBA class, with
#'   columns: `group_label`, `dug_class`, `dug_label`, `total_pop`,
#'   `pop_exposed`, `perc_exposed`, `poor_pop`, `poor_exposed`,
#'   `perc_poor_exposed`, `hazard_name`, `return_period`, `year`.
#' @export
deg_run_exposure <- function(pop_dir, flood_frac_ras, dug_ras,
                               dep_mask      = NULL,
                               level_labels,
                               ctry_boundary,
                               group_meta    = WP_GROUP_META,
                               hazard_name   = "River Flood",
                               return_period = 100L,
                               year          = 2020L) {

  pop_files <- list.files(pop_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(pop_files) == 0) stop("No .tif files found in: ", pop_dir)

  file_meta <- pop_parse_group_filenames(pop_files) |>
    dplyr::filter(!is.na(group_name)) |>
    dplyr::left_join(group_meta, by = "group_name")

  if (nrow(file_meta) == 0)
    stop("No files matching the expected naming pattern found in: ", pop_dir)

  message("Running DEGURBA exposure [", hazard_name, " RP", return_period, "] \u2014 ",
          nrow(file_meta), " demographic groups ...")

  purrr::map_dfr(seq_len(nrow(file_meta)), function(i) {
    row   <- file_meta[i, ]
    label <- if (is.na(row$label)) row$group_name else row$label
    message("  -> ", label)

    pop_ras <- terra::rast(row$file)
    pop_c   <- pop_clip(pop_ras, ctry_boundary)

    # Snap DEGURBA to the group raster extent (same CRS/res; crop only)
    dug_c <- terra::resample(dug_ras, pop_c, method = "near")

    # --- Zonal: total population by DEGURBA class ---
    z_total <- terra::zonal(pop_c, dug_c, fun = "sum", na.rm = TRUE) |>
      as.data.frame()
    names(z_total) <- c("dug_class", "total_pop")

    # --- Zonal: raw exposed population by DEGURBA class ---
    exposed_raw        <- pop_c * flood_frac_ras
    names(exposed_raw) <- "pop_exposed"
    z_exposed <- terra::zonal(exposed_raw, dug_c, fun = "sum", na.rm = TRUE) |>
      as.data.frame()
    names(z_exposed) <- c("dug_class", "pop_exposed")

    result <- dplyr::left_join(z_total, z_exposed, by = "dug_class") |>
      dplyr::mutate(
        perc_exposed  = (pop_exposed / total_pop) * 100,
        group_label   = label,
        hazard_name   = hazard_name,
        return_period = as.integer(return_period),
        year          = as.integer(year)
      )

    # --- Poor-population exposure (if dep_mask provided) ---
    if (!is.null(dep_mask)) {
      dep_c    <- pop_clip(dep_mask, ctry_boundary)
      pop_poor <- pop_c * dep_c

      z_poor_total <- terra::zonal(pop_poor, dug_c, fun = "sum", na.rm = TRUE) |>
        as.data.frame()
      names(z_poor_total) <- c("dug_class", "poor_pop")

      exposed_poor        <- pop_poor * flood_frac_ras
      names(exposed_poor) <- "poor_exposed"
      z_poor_exp <- terra::zonal(exposed_poor, dug_c, fun = "sum", na.rm = TRUE) |>
        as.data.frame()
      names(z_poor_exp) <- c("dug_class", "poor_exposed")

      result <- result |>
        dplyr::left_join(z_poor_total, by = "dug_class") |>
        dplyr::left_join(z_poor_exp,   by = "dug_class") |>
        dplyr::mutate(perc_poor_exposed = (poor_exposed / poor_pop) * 100)

    } else {
      result <- result |>
        dplyr::mutate(poor_pop          = NA_real_,
                      poor_exposed      = NA_real_,
                      perc_poor_exposed = NA_real_)
    }

    # Attach human-readable class label; drop NA zone rows (cells outside grid)
    result |>
      dplyr::filter(!is.na(dug_class)) |>
      dplyr::mutate(
        dug_label = dplyr::coalesce(
          level_labels[as.character(dug_class)],
          paste0("Class ", dug_class)
        )
      )
  })
}
