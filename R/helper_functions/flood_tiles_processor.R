#################################################################################################
#                                                                                               #
#                            FLOOD TILES PROCESSING FUNCTIONS                                   #
#                                 By Derrick DEMEVENG                                           #
#                                                                                               #
#################################################################################################

#' Flood tiles download
#' 
#' @description
#' Downloads flood extent or depth raster tiles from the Copernicus
#' Emergency Management Service for a given return period.
#'
#' @param return_period Numeric flood return period in years.
#'   Default is 100. Available values are 30, 100, and 500.
#' @param out_dir Character. Directory where downloaded tiles will be saved.
#'
#' @returns
#' Invisibly returns `NULL`. Raster files are written to `out_dir`.
#' 
#' @export
#'
#' @examples
#' 
#' 
fld_tiles_downloader <- function(return_period = 100, out_dir) {
  out_dir  <- paste("data/flood/flood_layers_RP", return_period, "/")
  if (!dir.exists(out_dir))
    dir.create(out_dir, recursive = TRUE)
  
  # ---- Read directory listing HTML ----
  html <- readLines(base_url, warn = FALSE)
  
  # ---- Extract .tif links ----
  # This matches href="something.tif" or href='something.tif'
  tif_files <- unique(gsub('.*href=["\']([^"\']+\\.tif)["\'].*', "\\1", html[grepl("\\.tif", html, ignore.case = TRUE)]))
  
  # Defensive: keep only .tif and drop weird entries
  tif_files <- tif_files[grepl("\\.tif$", tif_files, ignore.case = TRUE)]
  if (length(tif_files) == 0)
    stop("No .tif files found. The server listing format may have changed.")
  
  message("Found ", length(tif_files), " GeoTIFF tiles.")
  
  # ---- Download files (skips existing) ----
  for (f in tif_files) {
    url <- paste0(base_url, f)
    dest <- file.path(out_dir, f)
    
    if (file.exists(dest)) {
      message("[SKIP] ", f)
      next
    }
    
    message("[GET ] ", f)
    ok <- tryCatch({
      utils::download.file(url,
                           destfile = dest,
                           mode = "wb",
                           quiet = TRUE)
      TRUE
    }, error = function(e) {
      message("[FAIL] ", f, " -> ", conditionMessage(e))
      FALSE
    })
  }
  message("Done. Files saved in: ", normalizePath(out_dir))
  
}



#' Title
#'
#' @param shp 
#' @param v_tiles_dir 
#' @param r_tiles_dir 
#' @param out_dir 
#'
#' @returns
#' @export
#'
#' @examples
#' 
#' 
fld_tiles_mosaic <- function(shp, v_tiles_dir, r_tiles_dir, out_dir) {
  flood_tiles <- sf::read_sf(v_tiles_dir) # flood tiles

  # i) intersect the country with the flood tiles
  ctry_admin0  <- sf::st_make_valid(shp)
  flood_tiles  <- sf::st_make_valid(flood_tiles)

  ctry_admin0 <- sf::st_transform(ctry_admin0, sf::st_crs(flood_tiles))
  ctry_tiles <- sf::st_intersects(ctry_admin0, flood_tiles)
  idx <- unlist(ctry_tiles)

  # iii) query the flood tiles
  # a) Pull tile codes
  ctry_tile_codes <- flood_tiles[idx, ] |>
    dplyr::mutate(
      tile_name = paste0("ID", id, "_", name)
    ) |>
    dplyr::pull(tile_name)

  # b) Select the right tiles
  all_files <- list.files(flood_dir, full.names = TRUE)

  selected_files <- all_files[
    stringr::str_detect(
      basename(all_files),
      paste(ctry_tile_codes, collapse = "|")
    )
  ]

  selected_files <- selected_files[
    stringr::str_detect(selected_files, "_RP100_depth\\.tif$")
  ]

  # iv) Mosaic the queried tiles
  vrt_path <- file.path(out_dir, paste0(ctry_code, "_RP100_depth.vrt"))
  dir.create(dirname(vrt_path), recursive = TRUE, showWarnings = FALSE)

  terra::vrt(selected_files, vrt_path, overwrite = TRUE)

  rp100 <- terra::rast(vrt_path)

  return(rp100)
}