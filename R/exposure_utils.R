#################################################################################################
#                                                                                               #
#                          FLOOD EXPOSURE UTILITY FUNCTIONS                                     #
#                                 By Derrick DEMEVENG                                           #
#                                                                                               #
#################################################################################################

#' Compute exposed population from hazard fraction and population rasters
#'
#' @param pop_ras              SpatRaster. Population counts per cell.
#' @param flood_frac_ras       SpatRaster. Hazard fraction per cell (0–1).
#' @param pop_layer_name       Character. Layer name for total pop (default `"pop_total"`).
#' @param exposed_layer_name   Character. Layer name for exposed pop (default `"pop_exposed"`).
#'
#' @return Named list: `pop_total` and `pop_exposed` SpatRasters.
#' @export
exp_compute <- function(pop_ras, flood_frac_ras,
                        pop_layer_name     = "pop_total",
                        exposed_layer_name = "pop_exposed") {
  exposure        <- pop_ras * flood_frac_ras
  names(pop_ras)  <- pop_layer_name
  names(exposure) <- exposed_layer_name
  list(pop_total = pop_ras, pop_exposed = exposure)
}


#' Compute zonal statistics for population exposure at an admin level
#'
#' @param exposed_ras SpatRaster. Exposed population raster.
#' @param pop_ras     SpatRaster. Total population raster.
#' @param zones       sf or SpatVector. Zone polygons.
#'
#' @return sf data frame with zone attributes plus `pop_exposed`, `pop_total`,
#'   `perc_exposed`.
#' @export
exp_zonal_stats <- function(exposed_ras, pop_ras, zones) {
  if (inherits(zones, "sf")) zones_v <- terra::vect(zones) else zones_v <- zones

  stat_exposed <- terra::zonal(exposed_ras, zones_v,
                                fun = sum, na.rm = TRUE, as.polygons = TRUE) |>
    sf::st_as_sf()

  stat_total <- terra::zonal(pop_ras, zones_v,
                              fun = sum, na.rm = TRUE, as.polygons = FALSE)

  stat_exposed |>
    dplyr::bind_cols(dplyr::select(stat_total, pop_total)) |>
    dplyr::mutate(perc_exposed = (pop_exposed / pop_total) * 100)
}


#' Build a standardised indicator tibble from zonal statistics
#'
#' @param zonal_sf       sf. Output of [exp_zonal_stats()].
#' @param year           Integer. Reference year.
#' @param indicator_name Character. Human-readable indicator description.
#' @param indicator_code Character. Short indicator code.
#' @param group_label    Character. Human-readable demographic group label.
#' @param sex            Character. `"female"` or `"both"`.
#' @param age_group      Character. Age range string (e.g. `"15–49"`).
#' @param hazard_name    Character. Full hazard name (e.g. `"River Flood"`).
#' @param hazard_code    Character. Short hazard code (e.g. `"riv_flood"`).
#' @param return_period  Integer or `NA`. Return period in years (e.g. `100`).
#'   Use `NA` for hazards without a return period (heat, aridity).
#' @param admin_level    Integer. Administrative level (default `2L`).
#'
#' @return Tibble with standardised indicator columns.
#' @export
exp_build_indicator_tibble <- function(zonal_sf, year, indicator_name,
                                       indicator_code,
                                       group_label   = NA_character_,
                                       sex           = NA_character_,
                                       age_group     = NA_character_,
                                       hazard_name   = "River Flood",
                                       hazard_code   = "riv_flood",
                                       return_period = 100L,
                                       admin_level   = 2L,
                                       scenario      = "raw_exposure") {
  tibble::tibble(
    id               = NA_integer_,
    scenario         = scenario,
    hazard.name      = hazard_name,
    hazard.code      = hazard_code,
    return.period    = as.integer(return_period),
    indicator.name   = indicator_name,
    indicator.code   = indicator_code,
    group.label      = group_label,
    sex              = sex,
    age.group        = age_group,
    country.code     = zonal_sf$shapeGroup,
    admin.level      = as.integer(admin_level),
    admin.code       = zonal_sf$shapeID,
    admin.name       = zonal_sf$shapeName,
    year             = as.integer(year),
    total.pop        = zonal_sf$pop_total,
    pop.exposed      = zonal_sf$pop_exposed,
    perc.pop.exposed = zonal_sf$perc_exposed
  )
}


#' Summarise indicators at country level
#'
#' Aggregates admin-level rows to country × hazard × year × group.
#'
#' @param indicators Tibble. Output from [exp_build_indicator_tibble()] calls.
#'
#' @return Tibble with one row per country × hazard × year × demographic group.
#' @export
exp_summarise_by_country <- function(indicators) {
  indicators |>
    dplyr::group_by(scenario, country.code, hazard.name, hazard.code,
                    return.period, year, indicator.name, indicator.code,
                    group.label, sex, age.group) |>
    dplyr::summarise(
      total.pop        = sum(total.pop,   na.rm = TRUE),
      pop.exposed      = sum(pop.exposed, na.rm = TRUE),
      perc.pop.exposed = (pop.exposed / total.pop) * 100,
      .groups = "drop"
    )
}


#' Run exposure analysis for all demographic groups in a processed pop directory
#'
#' Scans `pop_dir` for processed group rasters, runs the full exposure pipeline
#' for each, and returns a unified indicator tibble (one row per admin zone per
#' demographic group).
#'
#' @param pop_dir        Character. Directory with processed group rasters
#'   (`{iso3}_{group}_{year}_{scope}_{res}.tif`).
#' @param flood_frac_ras SpatRaster. Hazard fraction raster (from
#'   [fld_aggregate_to_pop()]).
#' @param ctry_boundary  sf or SpatVector. Country boundary for clipping.
#' @param admin_zones    sf or SpatVector. Admin zones for zonal statistics.
#' @param hazard_name    Character. Full hazard name (default `"River Flood"`).
#' @param hazard_code    Character. Short hazard code (default `"riv_flood"`).
#' @param return_period  Integer. Return period in years (default `100`).
#' @param group_meta     Tibble. Defaults to [WP_GROUP_META].
#' @param admin_level    Integer. Admin level of `admin_zones` (default `2L`).
#'
#' @return Tibble of indicators with sequential `id` column.
#' @export
exp_run_all_groups <- function(pop_dir, flood_frac_ras, ctry_boundary,
                                admin_zones,
                                dep_mask      = NULL,
                                hazard_name   = "River Flood",
                                hazard_code   = "riv_flood",
                                return_period = 100L,
                                group_meta    = WP_GROUP_META,
                                admin_level   = 2L) {

  pop_files <- list.files(pop_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(pop_files) == 0) stop("No .tif files found in: ", pop_dir)

  file_meta <- pop_parse_group_filenames(pop_files) |>
    dplyr::filter(!is.na(group_name)) |>
    dplyr::left_join(group_meta, by = "group_name")

  if (nrow(file_meta) == 0)
    stop("No files matching the expected naming pattern found in: ", pop_dir)

  n_scen <- if (!is.null(dep_mask)) 3L else 1L
  message("Running exposure [", hazard_name, " RP", return_period, "] — ",
          n_scen, " scenario(s) × ", nrow(file_meta), " demographic groups ...")

  results <- purrr::map_dfr(seq_len(nrow(file_meta)), function(i) {
    row   <- file_meta[i, ]
    label <- if (is.na(row$label)) row$group_name else row$label
    message("  -> ", label)

    pop_ras <- terra::rast(row$file)
    pop_c   <- pop_clip(pop_ras, ctry_boundary)

    shared_args <- list(
      year          = row$year,
      group_label   = label,
      sex           = if (is.na(row$sex))       NA_character_ else row$sex,
      age_group     = if (is.na(row$age_group)) NA_character_ else row$age_group,
      hazard_name   = hazard_name,
      hazard_code   = hazard_code,
      return_period = as.integer(return_period),
      admin_level   = admin_level
    )

    # ------------------------------------------------------------------
    # Scenario 1 — Raw population exposure (no deprivation filter)
    # denominator = full group pop; numerator = full group pop × flood frac
    # ------------------------------------------------------------------
    pops_raw  <- exp_compute(pop_c, flood_frac_ras)
    zonal_raw <- exp_zonal_stats(pops_raw$pop_exposed, pops_raw$pop_total,
                                 admin_zones)
    ind_1 <- do.call(exp_build_indicator_tibble, c(
      list(zonal_sf       = zonal_raw,
           indicator_name = paste0(label, " exposed to ", hazard_name,
                                   " (RP", return_period, ")"),
           indicator_code = paste0(row$group_name, "_", hazard_code,
                                   "_RP", return_period),
           scenario       = "raw_exposure"),
      shared_args
    ))

    if (is.null(dep_mask)) return(ind_1)

    # Deprivation-filtered (vulnerable) sub-population
    dep_c    <- pop_clip(dep_mask, ctry_boundary)
    pop_vuln <- pop_c * dep_c

    # ------------------------------------------------------------------
    # Scenario 2 — Vulnerable pop exposed, denominator = vulnerable pop
    # "Of the deprived, what % are also flood-exposed?"
    # ------------------------------------------------------------------
    pops_v2  <- exp_compute(pop_vuln, flood_frac_ras)
    zonal_v2 <- exp_zonal_stats(pops_v2$pop_exposed, pops_v2$pop_total,
                                admin_zones)
    ind_2 <- do.call(exp_build_indicator_tibble, c(
      list(zonal_sf       = zonal_v2,
           indicator_name = paste0("Vulnerable ", label, " exposed to ",
                                   hazard_name, " (% of deprived) (RP",
                                   return_period, ")"),
           indicator_code = paste0(row$group_name, "_vuln_",
                                   hazard_code, "_RP", return_period),
           scenario       = "vuln_vs_vuln"),
      shared_args
    ))

    # ------------------------------------------------------------------
    # Scenario 3 — Vulnerable pop exposed, denominator = full group pop
    # "Of the whole group, what % are both deprived AND flood-exposed?"
    # ------------------------------------------------------------------
    exposed_v3 <- pop_vuln * flood_frac_ras
    names(exposed_v3) <- "pop_exposed"
    total_v3   <- pop_c
    names(total_v3) <- "pop_total"
    zonal_v3 <- exp_zonal_stats(exposed_v3, total_v3, admin_zones)
    ind_3 <- do.call(exp_build_indicator_tibble, c(
      list(zonal_sf       = zonal_v3,
           indicator_name = paste0("Vulnerable ", label, " exposed to ",
                                   hazard_name, " (% of total group) (RP",
                                   return_period, ")"),
           indicator_code = paste0(row$group_name, "_vuln_of_total_",
                                   hazard_code, "_RP", return_period),
           scenario       = "vuln_vs_total"),
      shared_args
    ))

    dplyr::bind_rows(ind_1, ind_2, ind_3)
  })

  results$id <- seq_len(nrow(results))
  results
}


#' Run the full country-level exposure pipeline for multiple population years
#'
#' Loops over population raster files (one per year). Use [exp_run_all_groups()]
#' when looping over demographic groups instead.
#'
#' @param pop_files       Character vector. Paths to population raster files.
#' @param flood_frac_ras  SpatRaster. Hazard fraction raster.
#' @param ctry_boundary   sf or SpatVector. Country boundary.
#' @param admin_zones     sf or SpatVector. Admin-level zones.
#' @param indicator_name  Character. Human-readable indicator name.
#' @param indicator_code  Character. Short indicator code.
#' @param group_label     Character. Human-readable group label.
#' @param sex             Character. Sex classification.
#' @param age_group       Character. Age range string.
#' @param hazard_name     Character. Full hazard name.
#' @param hazard_code     Character. Short hazard code.
#' @param return_period   Integer. Return period in years.
#' @param admin_level     Integer. Administrative level (default `2L`).
#'
#' @return Tibble of indicators with sequential `id` column.
#' @export
exp_run_country <- function(pop_files, flood_frac_ras, ctry_boundary,
                             admin_zones,
                             indicator_name = "Women 15-49 exposed to River Flood (RP100)",
                             indicator_code = "women_15_49_riv_flood_RP100",
                             group_label    = "Women 15\u201349",
                             sex            = "female",
                             age_group      = "15\u201349",
                             hazard_name    = "River Flood",
                             hazard_code    = "riv_flood",
                             return_period  = 100L,
                             admin_level    = 2L) {

  indicators <- tibble::tibble(
    id               = integer(),
    hazard.name      = character(),
    hazard.code      = character(),
    return.period    = integer(),
    indicator.name   = character(),
    indicator.code   = character(),
    group.label      = character(),
    sex              = character(),
    age.group        = character(),
    country.code     = character(),
    admin.level      = integer(),
    admin.code       = character(),
    admin.name       = character(),
    year             = integer(),
    total.pop        = numeric(),
    pop.exposed      = numeric(),
    perc.pop.exposed = numeric()
  )

  for (pop_file in pop_files) {
    year    <- stringr::str_extract(basename(pop_file), "\\d{4}")
    pop_ras <- terra::rast(pop_file)
    pop_c   <- pop_clip(pop_ras, ctry_boundary)

    pops  <- exp_compute(pop_c, flood_frac_ras)
    zonal <- exp_zonal_stats(pops$pop_exposed, pops$pop_total, admin_zones)

    year_df <- exp_build_indicator_tibble(
      zonal_sf       = zonal,
      year           = as.integer(year),
      indicator_name = indicator_name,
      indicator_code = indicator_code,
      group_label    = group_label,
      sex            = sex,
      age_group      = age_group,
      hazard_name    = hazard_name,
      hazard_code    = hazard_code,
      return_period  = as.integer(return_period),
      admin_level    = admin_level
    )

    indicators <- dplyr::bind_rows(indicators, year_df)
  }

  indicators$id <- seq_len(nrow(indicators))
  indicators
}
