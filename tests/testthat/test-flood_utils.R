# tests/testthat/test-flood_utils.R
# Tests for R/flood_utils.R
# Run via: source("tests/run_tests.R")

# ---------------------------------------------------------------------------
# fld_binarise()
# ---------------------------------------------------------------------------

test_that("fld_binarise: values below threshold become 0", {
  r      <- terra::rast(nrows = 2, ncols = 2, vals = c(0.05, 0.09, 0.0, 0.0))
  result <- fld_binarise(r, threshold = 0.1)
  expect_true(all(terra::values(result) == 0, na.rm = TRUE))
})

test_that("fld_binarise: values at or above threshold become 1", {
  r      <- terra::rast(nrows = 2, ncols = 2, vals = c(0.1, 0.5, 1.0, 2.0))
  result <- fld_binarise(r, threshold = 0.1)
  expect_true(all(terra::values(result) == 1, na.rm = TRUE))
})

test_that("fld_binarise: NA values are preserved", {
  r      <- terra::rast(nrows = 1, ncols = 3, vals = c(0.05, 0.2, NA))
  result <- fld_binarise(r, threshold = 0.1)
  vals   <- as.vector(terra::values(result))
  expect_equal(vals[1], 0)
  expect_equal(vals[2], 1)
  expect_true(is.na(vals[3]))
})

test_that("fld_binarise: custom threshold is respected", {
  r      <- terra::rast(nrows = 1, ncols = 2, vals = c(0.5, 1.5))
  result <- fld_binarise(r, threshold = 1.0)
  vals   <- as.vector(terra::values(result))
  expect_equal(vals[1], 0)
  expect_equal(vals[2], 1)
})

# ---------------------------------------------------------------------------
# fld_clip()
# ---------------------------------------------------------------------------

test_that("fld_clip: output is a SpatRaster", {
  r <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(r) <- 1

  poly <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(0.2, 0.2, 0.8, 0.2, 0.8, 0.8, 0.2, 0.8, 0.2, 0.2),
        ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  )

  result <- fld_clip(r, poly)
  expect_true(inherits(result, "SpatRaster"))
})

test_that("fld_clip: accepts SpatVector boundary", {
  r <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(r) <- 1

  poly_v <- terra::vect(
    "POLYGON ((0.2 0.2, 0.8 0.2, 0.8 0.8, 0.2 0.8, 0.2 0.2))",
    crs = "EPSG:4326"
  )

  result <- fld_clip(r, poly_v)
  expect_true(inherits(result, "SpatRaster"))
})

# ---------------------------------------------------------------------------
# fld_aggregate_to_pop()
# ---------------------------------------------------------------------------

test_that("fld_aggregate_to_pop: output aligns to pop raster extent and resolution", {
  pop <- terra::rast(
    nrows = 5, ncols = 5,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(pop) <- 100

  flood <- terra::rast(
    nrows = 50, ncols = 50,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(flood) <- 1

  result <- fld_aggregate_to_pop(flood, pop)

  expect_equal(terra::nrow(result), terra::nrow(pop))
  expect_equal(terra::ncol(result), terra::ncol(pop))
})

test_that("fld_aggregate_to_pop: fully flooded binary gives fraction ~1", {
  pop <- terra::rast(
    nrows = 4, ncols = 4,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(pop) <- 100

  flood <- terra::rast(
    nrows = 40, ncols = 40,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(flood) <- 1  # fully flooded

  result <- fld_aggregate_to_pop(flood, pop)
  vals   <- terra::values(result, na.rm = TRUE)
  expect_true(all(vals >= 0.9))
})

test_that("fld_aggregate_to_pop: not-flooded binary gives fraction ~0", {
  pop <- terra::rast(
    nrows = 4, ncols = 4,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(pop) <- 100

  flood <- terra::rast(
    nrows = 40, ncols = 40,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(flood) <- 0  # not flooded

  result <- fld_aggregate_to_pop(flood, pop)
  vals   <- terra::values(result, na.rm = TRUE)
  expect_true(all(vals == 0))
})

test_that("fld_aggregate_to_pop: stops when flood is not finer than pop", {
  pop <- terra::rast(
    nrows = 5, ncols = 5,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  flood <- terra::rast(
    nrows = 4, ncols = 4,   # coarser than pop
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  expect_error(fld_aggregate_to_pop(flood, pop), "fact < 2")
})

# ---------------------------------------------------------------------------
# fld_build_vrt()
# ---------------------------------------------------------------------------

test_that("fld_build_vrt: creates VRT file and returns SpatRaster", {
  tmp_tif <- tempfile(fileext = ".tif")
  r       <- terra::rast(nrows = 5, ncols = 5, vals = runif(25))
  terra::writeRaster(r, tmp_tif, overwrite = TRUE)

  vrt_path <- tempfile(fileext = ".vrt")
  result   <- fld_build_vrt(tmp_tif, vrt_path)

  expect_true(inherits(result, "SpatRaster"))
  expect_true(file.exists(vrt_path))
})

test_that("fld_build_vrt: stops on empty tile_paths", {
  expect_error(fld_build_vrt(character(0), tempfile(fileext = ".vrt")),
               "tile_paths")
})

# ---------------------------------------------------------------------------
# fld_select_tiles()
# ---------------------------------------------------------------------------

test_that("fld_select_tiles: stops when no tiles intersect boundary", {
  ocean <- sf::st_sf(
    shapeGroup = "TST",
    geometry   = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(-170, -80, -160, -80, -160, -70, -170, -70, -170, -80),
        ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  )

  tiles <- sf::st_sf(
    id   = 1L,
    name = "tile_a",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(30, 0, 40, 0, 40, 10, 30, 10, 30, 0),
        ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  )

  tile_dir <- tempdir()

  # Write a fake GeoJSON for v_tiles_path
  tmp_geojson <- tempfile(fileext = ".geojson")
  sf::write_sf(tiles, tmp_geojson, delete_dsn = TRUE)

  expect_error(
    fld_select_tiles(ocean, tmp_geojson, tile_dir),
    "No flood tiles intersect"
  )
})
