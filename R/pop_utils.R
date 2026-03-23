#################################################################################################
#                                                                                               #
#                          POPULATION DATA UTILITY FUNCTIONS                                    #
#                                 By Derrick DEMEVENG                                           #
#                                                                                               #
#################################################################################################

# ---------------------------------------------------------------------------
# CONSTANTS -- demographic group metadata
# ---------------------------------------------------------------------------

#' Metadata for the standard WorldPop demographic groups
#'
#' A tibble with one row per group providing the machine name, human-readable
#' label, sex classification, and age range.  Used to annotate indicator
#' tables and visualisations.
#'
#' @export
WP_GROUP_META <- tibble::tribble(
  ~group_name,              ~label,                   ~sex,     ~age_group,
  "total_pop",              "Total Population",        "both",   "All ages",
  "women_15_49",            "Women 15\u201349",        "female", "15\u201349",
  "youth_15_24",            "Youth 15\u201324",        "both",   "15\u201324",
  "adolescent_10_19",       "Adolescents 10\u201319",  "both",   "10\u201319",
  "children_u5",            "Children under 5",        "both",   "0\u20134",
  "youth_women_15_24",      "Young Women 15\u201324",  "female", "15\u201324",
  "adolescent_girls_10_19", "Adolescent Girls 10\u201319", "female", "10\u201319",
  "pop_65plus",             "Population 65+",          "both",   "65+",
  "women_65plus",           "Women 65+",               "female", "65+"
)

# Ordered factor levels for consistent plot ordering
WP_GROUP_LEVELS <- c(
  "Total Population",
  "Women 15\u201349",
  "Youth 15\u201324",
  "Young Women 15\u201324",
  "Adolescents 10\u201319",
  "Adolescent Girls 10\u201319",
  "Children under 5",
  "Population 65+",
  "Women 65+"
)


# ---------------------------------------------------------------------------
# FILENAME PARSERS
# ---------------------------------------------------------------------------

#' Parse metadata from a WorldPop raw-band raster filename
#'
#' Handles the legacy naming convention used inside the raw download directory:
#' `{iso3}_{sex}_{age}_{year}_{suffix}.tif`
#'
#' @param file_path Character. Path to a WorldPop band `.tif` file.
#'
#' @return A one-row tibble with columns `file`, `sex_code`, `age_group`,
#'   `year`, `resolution`, `sex`.
#' @export
pop_parse_filename <- function(file_path) {
  f <- basename(file_path)

  m <- stringr::str_match(
    f,
    "^[a-z]{3}_((?:[fmt])|(?:T_[FM]))(?:_([0-9]{2}))?_([0-9]{4})_[A-Z]{2}_([0-9]+[mk]?m)_"
  )

  tibble::tibble(
    file       = file_path,
    sex_code   = m[, 2],
    age_group  = m[, 3],
    year       = as.integer(m[, 4]),
    resolution = m[, 5]
  ) |>
    dplyr::mutate(
      sex = dplyr::case_when(
        sex_code == "f"   ~ "female",
        sex_code == "m"   ~ "male",
        sex_code == "t"   ~ "total",
        sex_code == "T_F" ~ "female_total",
        sex_code == "T_M" ~ "male_total",
        TRUE              ~ NA_character_
      )
    )
}


#' Parse metadata from multiple WorldPop raw-band filenames
#'
#' Vectorised wrapper around [pop_parse_filename()].
#'
#' @param file_paths Character vector. Paths to WorldPop band raster files.
#'
#' @return Tibble with one row per file.
#' @export
pop_parse_filenames <- function(file_paths) {
  purrr::map_dfr(file_paths, pop_parse_filename)
}


#' Parse metadata from a processed demographic-group raster filename
#'
#' Handles files produced by [wp_sum_group()], named
#' `{iso3}_{group_name}_{year}_{scope}_{resolution}.tif`.
#'
#' @param file_path Character. Path to a processed group raster.
#'
#' @return A one-row tibble with columns `file`, `iso3`, `group_name`,
#'   `year`, `scope`, `resolution`.
#' @export
pop_parse_group_filename <- function(file_path) {
  f <- basename(file_path)
  m <- stringr::str_match(
    f,
    "^([a-z]{3})_(.+)_(\\d{4})_(global|country)_(1km|100m)(?:_CN)?\\.tif$"
  )
  tibble::tibble(
    file       = file_path,
    iso3       = m[, 2],
    group_name = m[, 3],
    year       = as.integer(m[, 4]),
    scope      = m[, 5],
    resolution = m[, 6]
  )
}


#' Parse metadata from multiple processed group raster filenames
#'
#' Vectorised wrapper around [pop_parse_group_filename()].
#'
#' @param file_paths Character vector. Paths to processed group rasters.
#'
#' @return Tibble with one row per file.
#' @export
pop_parse_group_filenames <- function(file_paths) {
  purrr::map_dfr(file_paths, pop_parse_group_filename)
}


# ---------------------------------------------------------------------------
# SPATIAL HELPERS
# ---------------------------------------------------------------------------

#' Clip a population raster to a spatial boundary (crop + mask)
#'
#' @param pop_ras  SpatRaster. Population raster.
#' @param boundary sf or SpatVector. Boundary polygon. Reprojected to raster
#'   CRS automatically.
#'
#' @return Clipped and masked SpatRaster.
#' @export
pop_clip <- function(pop_ras, boundary) {
  if (inherits(boundary, "sf")) boundary <- terra::vect(boundary)
  boundary <- terra::project(boundary, terra::crs(pop_ras))
  terra::crop(pop_ras, boundary) |> terra::mask(boundary)
}
