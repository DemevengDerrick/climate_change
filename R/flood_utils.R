#################################################################################################
#                                                                                               #
#                            FLOOD DATA UTILITY FUNCTIONS                                       #
#                                 By Derrick DEMEVENG                                           #
#                                                                                               #
#################################################################################################

#' Select flood raster tile files for a country
#'
#' Intersects the country boundary with the flood tile index (a polygon layer
#' where each feature represents one downloadable tile) and returns the paths
#' of raster files whose tile codes match the intersection.
#'
#' @param ctry_shp   sf object. Country boundary (admin0).
#' @param v_tiles_path Character. Path to the tile-index GeoJSON/shapefile with
#'   `id` and `name` columns.
#' @param r_tiles_dir  Character. Directory that contains the downloaded flood
#'   raster tiles.
#' @param return_period Integer. Return period in years (default 100). Used to
#'   filter files matching `_RP{return_period}_depth.tif`.
#'
#' @return Character vector of matched file paths.
#' @export
fld_select_tiles <- function(ctry_shp, v_tiles_path, r_tiles_dir,
                              return_period = 100) {
  flood_tiles <- sf::read_sf(v_tiles_path)

  ctry_shp    <- sf::st_make_valid(ctry_shp)
  flood_tiles <- sf::st_make_valid(flood_tiles)
  ctry_shp    <- sf::st_transform(ctry_shp, sf::st_crs(flood_tiles))

  idx <- unlist(sf::st_intersects(ctry_shp, flood_tiles))

  if (length(idx) == 0) {
    stop("No flood tiles intersect the provided country boundary.")
  }

  tile_codes <- flood_tiles[idx, ] |>
    dplyr::mutate(tile_name = paste0("ID", id, "_", name)) |>
    dplyr::pull(tile_name)

  all_files <- list.files(r_tiles_dir, full.names = TRUE)

  matched <- all_files[
    stringr::str_detect(basename(all_files), paste(tile_codes, collapse = "|"))
  ]

  pattern <- paste0("_RP", return_period, "_depth\\.tif$")
  matched <- matched[stringr::str_detect(matched, pattern)]

  if (length(matched) == 0) {
    stop(
      "No flood raster files matched tile codes for return period RP",
      return_period, " in: ", r_tiles_dir
    )
  }

  matched
}


#' Build a VRT mosaic from flood raster tile files
#'
#' @param tile_paths Character vector. Paths to individual raster tile files.
#' @param vrt_path   Character. Output path for the VRT file (including
#'   filename). Parent directory is created automatically.
#' @param overwrite  Logical. Whether to overwrite an existing VRT (default TRUE).
#'
#' @return A `SpatRaster` (terra) pointing at the VRT.
#' @export
fld_build_vrt <- function(tile_paths, vrt_path, overwrite = TRUE) {
  if (length(tile_paths) == 0) stop("`tile_paths` must contain at least one file.")
  dir.create(dirname(vrt_path), recursive = TRUE, showWarnings = FALSE)
  terra::vrt(tile_paths, vrt_path, overwrite = overwrite)
  terra::rast(vrt_path)
}


#' Reclassify a flood depth raster to binary (flooded / not flooded)
#'
#' @param flood_ras SpatRaster. Flood depth raster (any units, metres assumed).
#' @param threshold  Numeric. Depth threshold in metres (default 0.1).
#'
#' @return Binary SpatRaster: 1 = flooded, 0 = not flooded, NA preserved.
#' @export
fld_binarise <- function(flood_ras, threshold = 0.1) {
  terra::ifel(flood_ras > threshold, 1, 0)
}


#' Clip a raster to a spatial boundary (crop + mask)
#'
#' @param ras      SpatRaster. Input raster.
#' @param boundary sf or SpatVector. Boundary polygon. Reprojected to raster
#'   CRS automatically.
#'
#' @return Clipped and masked SpatRaster.
#' @export
fld_clip <- function(ras, boundary) {
  if (inherits(boundary, "sf")) boundary <- terra::vect(boundary)
  boundary <- terra::project(boundary, terra::crs(ras))
  terra::crop(ras, boundary) |> terra::mask(boundary)
}


#' Aggregate a fine-resolution flood binary raster to population raster resolution
#'
#' Computes the **fraction** of each population cell that is classified as
#' flooded, then resamples to align exactly to the population grid. NA cells
#' in the flood raster are treated as 0 (not flooded) before aggregation.
#'
#' @param flood_bin_ras SpatRaster. Binary flood raster (0/1, fine resolution).
#' @param pop_ras       SpatRaster. Population raster defining the target grid.
#'
#' @return SpatRaster of flood fraction at population raster resolution,
#'   values in [0, 1].
#' @export
fld_aggregate_to_pop <- function(flood_bin_ras, pop_ras) {
  # Replace NAs with 0 so they don't inflate the flooded fraction
  flood_bin_ras <- terra::ifel(is.na(flood_bin_ras), 0, flood_bin_ras)

  # Align CRS to population raster before computing factor
  if (!terra::same.crs(flood_bin_ras, pop_ras)) {
    flood_bin_ras <- terra::project(flood_bin_ras, pop_ras, method = "near")
  }

  fact <- floor(terra::res(pop_ras) / terra::res(flood_bin_ras))
  if (any(fact < 2)) {
    stop(
      "fact < 2: flood raster is not finer than population raster. ",
      "Check resolutions or CRS alignment."
    )
  }

  flood_agg <- terra::aggregate(flood_bin_ras, fact = fact, fun = mean, na.rm = TRUE)
  terra::resample(flood_agg, pop_ras, method = "near")
}


#' Download flood raster tiles from the Copernicus GLOFAS server
#'
#' @param base_url      Character. URL to the server directory listing containing
#'   the .tif files (e.g. `"https://jeodpp.jrc.ec.europa.eu/ftp/.../RP100/"`).
#' @param out_dir       Character. Directory where downloaded tiles will be saved.
#' @param return_period Integer. Return period in years. Used only for the
#'   informational message; filtering happens via `base_url`. Default 100.
#'
#' @return Invisibly returns `NULL`. Tiles are written to `out_dir`.
#' @export
fld_download_tiles <- function(base_url, out_dir, return_period = 100) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  html      <- readLines(base_url, warn = FALSE)
  tif_files <- unique(
    gsub('.*href=["\']([^"\']+\\.tif)["\'].*', "\\1",
         html[grepl("\\.tif", html, ignore.case = TRUE)])
  )
  tif_files <- tif_files[grepl("\\.tif$", tif_files, ignore.case = TRUE)]

  if (length(tif_files) == 0) {
    stop("No .tif files found at: ", base_url,
         ". The server listing format may have changed.")
  }

  message("Found ", length(tif_files), " GeoTIFF tiles for RP", return_period, ".")

  for (f in tif_files) {
    dest <- file.path(out_dir, f)
    if (file.exists(dest)) { message("[SKIP] ", f); next }
    message("[GET ] ", f)
    ok <- tryCatch({
      utils::download.file(paste0(base_url, f), destfile = dest,
                           mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) {
      message("[FAIL] ", f, " -> ", conditionMessage(e))
      FALSE
    })
  }

  message("Done. Files saved in: ", normalizePath(out_dir))
  invisible(NULL)
}
