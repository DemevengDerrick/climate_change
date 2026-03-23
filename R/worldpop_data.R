#################################################################################################
#                                                                                               #
#                          WORLDPOP DATA DOWNLOAD & AGGREGATION UTILITIES                       #
#                                 By Derrick DEMEVENG                                           #
#                                                                                               #
#  Two download modes:                                                                           #
#    "global"  (default) — WorldPop Global 1 km CONSTRAINED UN-adjusted (R2025A)               #
#                          Global mosaic files — downloaded once, NOT per-country.              #
#                          File: global_{sex}_{age:02d}_{year}_CN_1km_R2025A_UA_v1.tif         #
#                          Clipping to a country boundary happens at the processing step.       #
#                                                                                               #
#    "country"           — WorldPop 100 m CONSTRAINED UN-adjusted (R2025A), per country        #
#                          Groups are summed then clipped to a user-supplied boundary.          #
#                          File: {iso3}_{sex}_{age:02d}_{year}_CN_100m_R2025A_v1.tif           #
#                                                                                               #
#  NOTE: Both base URLs (WP_BASE_URL_GLOBAL_1KM_CN and WP_BASE_URL_COUNTRY_100M_CN)           #
#  should be verified against the WorldPop data portal before running:                          #
#    https://data.worldpop.org/GIS/AgeSex_structures/                                          #
#  Update the constants below if the server path has changed.                             #
#                                                                                               #
#  Typical workflow:                                                                             #
#    wp_build_all_groups()             — orchestrates download + clip + save                    #
#    wp_check_global_downloaded()      — reports which raw band files already exist             #
#    wp_download()                     — download individual band files only                    #
#    wp_sum_group()                    — sum downloaded bands into one group raster             #
#                                                                                               #
#  Age bands: 0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80            #
#  Sex codes: "m" (male), "f" (female)                                                          #
#                                                                                               #
#################################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr, terra, purrr, tibble, stringr, dplyr)

# ---------------------------------------------------------------------------
# CONSTANTS
# ---------------------------------------------------------------------------

#' Base URL root for all WorldPop R2025A age-sex datasets
#'
#' Both global 1km and country 100m constrained UN-adjusted files live under
#' this root.  Full paths follow:
#'   Global 1km : {root}{year}/0_Mosaicked/v1/1km_ua/constrained/global_{sex}_{age:02d}_{year}_CN_1km_R2025A_UA_v1.tif
#'   Country 100m: {root}{year}/{ISO3}/v1/100m/constrained/{iso3}_{sex}_{age:02d}_{year}_CN_100m_R2025A_v1.tif
#'
#' @export
WP_BASE_URL_R2025A <- paste0(
  "https://data.worldpop.org/GIS/AgeSex_structures/",
  "Global_2015_2030/R2025A/"
)

# Keep old names as aliases for backwards compatibility
WP_BASE_URL_GLOBAL_1KM_CN   <- WP_BASE_URL_R2025A
WP_BASE_URL_COUNTRY_100M_CN <- WP_BASE_URL_R2025A

#' Pre-defined WorldPop demographic group specifications
#'
#' A named list, each entry having `sex` (character vector) and
#' `ages` (integer vector of WorldPop age-band lower bounds to sum).
#'
#' @export
WP_GROUPS <- list(
  total_pop              = list(sex = c("m", "f"), ages = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80)),
  women_15_49            = list(sex = "f",         ages = c(15, 20, 25, 30, 35, 40, 45)),
  youth_15_24            = list(sex = c("m", "f"), ages = c(15, 20)),
  adolescent_10_19       = list(sex = c("m", "f"), ages = c(10, 15)),
  children_u5            = list(sex = c("m", "f"), ages = c(0, 1)),
  youth_women_15_24      = list(sex = "f",         ages = c(15, 20)),
  adolescent_girls_10_19 = list(sex = "f",         ages = c(10, 15)),
  pop_65plus             = list(sex = c("m", "f"), ages = c(65, 70, 75, 80)),
  women_65plus           = list(sex = "f",         ages = c(65, 70, 75, 80))
)


# ---------------------------------------------------------------------------
# INTERNAL HELPERS
# ---------------------------------------------------------------------------

#' Build the WorldPop download URL for one age-sex band
#' @keywords internal
.wp_url <- function(iso3, year, sex, age, scope = "global") {
  iso3_upper <- toupper(iso3)
  iso3_lower <- tolower(iso3)
  yr         <- as.integer(year)
  ag         <- as.integer(age)

  if (scope == "global") {
    # Global constrained UN-adjusted 1km (R2025A)
    # Path: {root}{year}/0_Mosaicked/v1/1km_ua/constrained/global_{sex}_{age:02d}_{year}_CN_1km_R2025A_UA_v1.tif
    sprintf(
      "%s%d/0_Mosaicked/v1/1km_ua/constrained/global_%s_%02d_%d_CN_1km_R2025A_UA_v1.tif",
      WP_BASE_URL_R2025A, yr, sex, ag, yr
    )
  } else {
    # Country-level constrained UN-adjusted 100m (R2025A)
    # Path: {root}{year}/{ISO3}/v1/100m/constrained/{iso3}_{sex}_{age:02d}_{year}_CN_100m_R2025A_UA_v1.tif
    # NOTE: verify the _UA_ infix exists for country 100m files on the WorldPop server
    sprintf(
      "%s%d/%s/v1/100m/constrained/%s_%s_%02d_%d_CN_100m_R2025A_UA_v1.tif",
      WP_BASE_URL_R2025A, yr, iso3_upper, iso3_lower, sex, ag, yr
    )
  }
}

#' Build the local raw-download file path
#' @keywords internal
.wp_local_path <- function(iso3, year, sex, age, destdir, scope = "global") {
  yr <- as.integer(year)
  ag <- as.integer(age)

  filename <- if (scope == "global") {
    # Global mosaic file — no country prefix, age zero-padded
    sprintf("global_%s_%02d_%d_CN_1km_R2025A_UA_v1.tif", sex, ag, yr)
  } else {
    # Country-level 100m R2025A — age zero-padded
    sprintf("%s_%s_%02d_%d_CN_100m_R2025A_UA_v1.tif", tolower(iso3), sex, ag, yr)
  }
  file.path(destdir, filename)
}

#' Build the output path for a processed demographic-group raster
#' @keywords internal
.wp_group_path <- function(iso3, year, group_name, pop_dir, scope = "global") {
  suffix   <- if (scope == "global") "global_1km_CN" else "country_100m_CN"
  filename <- sprintf("%s_%s_%d_%s.tif",
                      tolower(iso3), group_name, as.integer(year), suffix)
  file.path(pop_dir, filename)
}


# ---------------------------------------------------------------------------
# CHECK FUNCTION
# ---------------------------------------------------------------------------

#' Report which global constrained 1km band files are already downloaded
#'
#' Scans `download_dir` for the raw global band files required by `groups`
#' and returns a summary tibble. Use this in the main script to inform the
#' user before starting a country-level exposure analysis.
#'
#' @param year         Integer. Year (2000–2020).
#' @param download_dir Character. Directory where raw files are stored.
#' @param groups       Named list. Defaults to [WP_GROUPS].
#'
#' @return Tibble with columns `file`, `sex`, `age`, `exists`.
#' @export
wp_check_global_downloaded <- function(year, download_dir,
                                       groups = WP_GROUPS) {
  all_combos <- purrr::map_dfr(groups, function(g) {
    expand.grid(sex = g$sex, age = as.integer(g$ages),
                stringsAsFactors = FALSE)
  }) |> dplyr::distinct()

  purrr::pmap_dfr(all_combos, function(sex, age) {
    path <- .wp_local_path(NA, year, sex, age, download_dir, "global")
    tibble::tibble(
      file   = basename(path),
      sex    = sex,
      age    = age,
      exists = file.exists(path)
    )
  })
}


#' Report which country-level constrained 100m band files are already downloaded
#'
#' Scans `download_dir` for the raw country band files required by `groups`
#' and returns a summary tibble. Use this in the main script to inform the
#' user before starting a country-level exposure analysis with 100m data.
#'
#' @param iso3         Character. ISO 3166-1 alpha-3 code.
#' @param year         Integer. Year (2000–2020).
#' @param download_dir Character. Directory where raw files are stored.
#' @param groups       Named list. Defaults to [WP_GROUPS].
#'
#' @return Tibble with columns `file`, `sex`, `age`, `exists`.
#' @export
wp_check_country_downloaded <- function(iso3, year, download_dir,
                                        groups = WP_GROUPS) {
  all_combos <- purrr::map_dfr(groups, function(g) {
    expand.grid(sex = g$sex, age = as.integer(g$ages),
                stringsAsFactors = FALSE)
  }) |> dplyr::distinct()

  purrr::pmap_dfr(all_combos, function(sex, age) {
    path <- .wp_local_path(iso3, year, sex, age, download_dir, "country")
    tibble::tibble(
      file   = basename(path),
      sex    = sex,
      age    = age,
      exists = file.exists(path)
    )
  })
}


# ---------------------------------------------------------------------------
# DOWNLOAD FUNCTION
# ---------------------------------------------------------------------------

#' Download WorldPop age-sex band rasters
#'
#' **`scope = "global"` (default)** — downloads the WorldPop global 1km
#' constrained UN-adjusted (R2025A) mosaic files.  These are global files
#' (not country-specific) and are stored directly in `download_dir` without
#' a country subfolder.  Clipping to a country boundary happens later in
#' [wp_sum_group()].
#'
#' **`scope = "country"`** — downloads the 100 m constrained UN-adjusted
#' country-level files.
#'
#' @param iso3         Character. ISO 3166-1 alpha-3 code (used only for
#'   `scope = "country"`).
#' @param year         Integer. Year 2000–2020.
#' @param download_dir Character. Directory for raw `.tif` files.
#' @param sex          Character vector or `NULL`. `"m"`, `"f"`, or both.
#' @param ages         Integer vector or `NULL`. Age-band lower bounds.
#' @param scope        Character. `"global"` (default) or `"country"`.
#' @param overwrite    Logical. Re-download existing files (default `FALSE`).
#' @param quiet        Logical. Suppress messages (default `FALSE`).
#'
#' @return Invisibly, a tibble with download results.
#' @export
wp_download <- function(iso3   = NULL,
                        year,
                        download_dir,
                        sex       = NULL,
                        ages      = NULL,
                        scope     = "global",
                        overwrite = FALSE,
                        quiet     = FALSE) {

  scope <- match.arg(scope, c("global", "country"))

  if (scope == "country" && is.null(iso3))
    stop("'iso3' must be supplied when scope = 'country'.")

  all_ages <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80)
  all_sex  <- c("m", "f")

  if (is.null(sex))  sex  <- all_sex
  if (is.null(ages)) ages <- all_ages

  sex  <- match.arg(sex, all_sex, several.ok = TRUE)
  ages <- as.integer(ages)
  bad  <- setdiff(ages, all_ages)
  if (length(bad) > 0)
    stop("Invalid age band(s): ", paste(bad, collapse = ", "),
         "\nValid: ", paste(all_ages, collapse = ", "))

  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

  combos  <- expand.grid(sex = sex, age = ages, stringsAsFactors = FALSE)
  results <- purrr::pmap_dfr(combos, function(sex, age) {
    url        <- .wp_url(iso3, year, sex, age, scope)
    local_path <- .wp_local_path(iso3, year, sex, age, download_dir, scope)

    if (file.exists(local_path) && !overwrite) {
      if (!quiet) message("  [skip] ", basename(local_path))
      return(tibble::tibble(year = as.integer(year), sex = sex, age = age,
                            scope = scope, url = url,
                            local_path = local_path, status = "skipped"))
    }

    if (!quiet) message("  [dl]   ", basename(local_path))
    resp <- tryCatch(
      httr::GET(url,
                httr::write_disk(local_path, overwrite = TRUE),
                httr::progress()),
      error = function(e) e
    )

    status <- if (inherits(resp, "error") || httr::http_error(resp)) {
      if (!quiet) message("         FAILED")
      if (file.exists(local_path)) file.remove(local_path)
      "failed"
    } else {
      "downloaded"
    }

    tibble::tibble(year = as.integer(year), sex = sex, age = age,
                   scope = scope, url = url,
                   local_path = local_path, status = status)
  })

  n_ok   <- sum(results$status == "downloaded")
  n_skip <- sum(results$status == "skipped")
  n_fail <- sum(results$status == "failed")
  label  <- if (scope == "global") "global 1km CN" else paste0(toupper(iso3), " 100m")
  message(sprintf("wp_download [%s %d]: %d downloaded, %d skipped, %d failed",
                  label, year, n_ok, n_skip, n_fail))
  invisible(results)
}


# ---------------------------------------------------------------------------
# RASTER SUM FUNCTION
# ---------------------------------------------------------------------------

#' Sum WorldPop age-sex band rasters into one composite group raster
#'
#' Reads raw band files from `download_dir`, sums them cell-by-cell,
#' clips to `boundary`, and writes the result to `pop_dir`.
#'
#' For `scope = "global"`, `boundary` is **required** — the global mosaic is
#' clipped to the country extent before saving.  For `scope = "country"`,
#' `boundary` is also required (files are already country-level but clipping
#' ensures exact alignment).
#'
#' @param iso3         Character. ISO 3166-1 alpha-3 code.
#' @param year         Integer.
#' @param sex          Character vector. Sex codes.
#' @param ages         Integer vector. Age-band lower bounds.
#' @param download_dir Character. Directory with raw band files.
#' @param pop_dir      Character. Output directory for processed rasters.
#' @param scope        Character. `"global"` (default) or `"country"`.
#' @param boundary     sf or SpatVector. Country boundary for clipping.
#'   **Required for both scopes.**
#' @param group_name   Character or `NULL`. Label for the output filename.
#' @param overwrite    Logical. Overwrite existing output (default `FALSE`).
#'
#' @return Invisibly, the summed [terra::SpatRaster].
#' @export
wp_sum_group <- function(iso3, year, sex, ages,
                         download_dir,
                         pop_dir,
                         scope      = "global",
                         boundary   = NULL,
                         group_name = NULL,
                         overwrite  = FALSE) {

  scope <- match.arg(scope, c("global", "country"))

  if (is.null(boundary))
    stop("'boundary' must be supplied for both scope = 'global' and scope = 'country'.")

  if (is.null(group_name)) {
    sex_label  <- if (length(unique(sex)) == 2) "both" else unique(sex)
    group_name <- sprintf("%s_a%s", sex_label,
                          paste(sort(unique(as.integer(ages))), collapse = "_"))
  }

  out_path <- .wp_group_path(iso3, year, group_name, pop_dir, scope)

  if (file.exists(out_path) && !overwrite) {
    message("  [skip] ", basename(out_path), " already exists.")
    return(invisible(terra::rast(out_path)))
  }

  combos <- expand.grid(sex = sex, age = as.integer(ages),
                        stringsAsFactors = FALSE)
  paths  <- purrr::pmap_chr(combos, function(sex, age) {
    .wp_local_path(iso3, year, sex, age, download_dir, scope)
  })

  missing_files <- paths[!file.exists(paths)]
  if (length(missing_files) > 0)
    stop(length(missing_files), " band file(s) not found in '", download_dir, "':\n",
         paste0("  ", basename(missing_files), collapse = "\n"),
         "\nRun wp_download() first.")

  summed        <- Reduce("+", purrr::map(paths, terra::rast))
  names(summed) <- group_name

  # Clip to country boundary (required for both scopes)
  summed <- pop_clip(summed, boundary)

  dir.create(pop_dir, recursive = TRUE, showWarnings = FALSE)
  terra::writeRaster(summed, out_path, overwrite = overwrite)
  message("  [out]  ", basename(out_path))

  invisible(summed)
}


# ---------------------------------------------------------------------------
# CONVENIENCE WRAPPER
# ---------------------------------------------------------------------------

#' Download and build all standard demographic group rasters for a country
#'
#' Full pipeline: downloads every age-sex band required by `groups`, clips
#' each group raster to the country `boundary`, and saves to `pop_dir`.
#'
#' **`scope = "global"` (default)**: downloads the global constrained
#' UN-adjusted 1km (R2025A) mosaic.  Each global file is shared across
#' countries — if already downloaded it is reused.  The result is clipped
#' to `boundary` before saving to `pop_dir`.
#'
#' **`scope = "country"`**: downloads the 100m constrained UN-adjusted
#' country files and clips to `boundary`.
#'
#' @param iso3         Character. ISO 3166-1 alpha-3 code.
#' @param year         Integer. Year (2000–2020).
#' @param download_dir Character. Directory for raw band files.
#' @param pop_dir      Character. Output directory for group rasters.
#' @param boundary     sf or SpatVector. Country boundary. **Required.**
#' @param scope        Character. `"global"` (default) or `"country"`.
#' @param groups       Named list in the format of [WP_GROUPS].
#' @param overwrite    Logical. Re-download and re-process (default `FALSE`).
#'
#' @return Invisibly, a named list of [terra::SpatRaster] objects.
#' @export
wp_build_all_groups <- function(iso3, year,
                                download_dir,
                                pop_dir,
                                boundary,
                                scope     = "global",
                                groups    = WP_GROUPS,
                                overwrite = FALSE) {

  scope <- match.arg(scope, c("global", "country"))

  all_combos <- purrr::map_dfr(groups, function(g) {
    expand.grid(sex = g$sex, age = as.integer(g$ages), stringsAsFactors = FALSE)
  }) |> dplyr::distinct()

  res_label <- if (scope == "global") "1km constrained UN-adj (global mosaic)"
               else                   "100m constrained UN-adj (country)"
  message("--- Step 1: Download raw band files [", toupper(iso3), " ", year,
          " | ", res_label, "] ---")

  wp_download(
    iso3         = if (scope == "country") iso3 else NULL,
    year         = year,
    download_dir = download_dir,
    sex          = unique(all_combos$sex),
    ages         = unique(all_combos$age),
    scope        = scope,
    overwrite    = overwrite,
    quiet        = FALSE
  )

  message("--- Step 2: Build demographic group rasters (clipped to country) ---")
  out <- purrr::imap(groups, function(g, group_name) {
    message("  -> ", group_name)
    wp_sum_group(
      iso3         = iso3,
      year         = year,
      sex          = g$sex,
      ages         = g$ages,
      download_dir = download_dir,
      pop_dir      = pop_dir,
      scope        = scope,
      boundary     = boundary,
      group_name   = group_name,
      overwrite    = overwrite
    )
  })

  message("Done. Processed rasters saved to: ", pop_dir)
  invisible(out)
}
