# tests/testthat/test-exposure_utils.R
# Tests for R/exposure_utils.R
# Run via: source("tests/run_tests.R")

# Helper: build a minimal SpatRaster with given constant values and CRS
make_rast <- function(val, nrows = 5, ncols = 5,
                       xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                       crs = "EPSG:4326") {
  r <- terra::rast(nrows = nrows, ncols = ncols,
                   xmin = xmin, xmax = xmax,
                   ymin = ymin, ymax = ymax,
                   crs = crs)
  terra::values(r) <- val
  r
}

# Helper: build a minimal sf polygon covering [0,1] x [0,1]
make_poly_sf <- function() {
  sf::st_sf(
    shapeGroup = "TST",
    shapeID    = "TST-001",
    shapeName  = "TestZone",
    geometry   = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
        ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  )
}

# ---------------------------------------------------------------------------
# exp_compute()
# ---------------------------------------------------------------------------

test_that("exp_compute: returns named list with two SpatRasters", {
  pop   <- make_rast(100)
  frac  <- make_rast(0.5)

  result <- exp_compute(pop, frac)

  expect_type(result, "list")
  expect_named(result, c("pop_total", "pop_exposed"))
  expect_true(inherits(result$pop_total,   "SpatRaster"))
  expect_true(inherits(result$pop_exposed, "SpatRaster"))
})

test_that("exp_compute: exposed = pop * flood_fraction", {
  pop  <- make_rast(200)
  frac <- make_rast(0.25)

  result <- exp_compute(pop, frac)
  exposed_vals <- terra::values(result$pop_exposed, na.rm = TRUE)
  expect_equal(unique(exposed_vals), 50)
})

test_that("exp_compute: flood fraction = 0 gives 0 exposure", {
  pop  <- make_rast(1000)
  frac <- make_rast(0)

  result <- exp_compute(pop, frac)
  expect_true(all(terra::values(result$pop_exposed, na.rm = TRUE) == 0))
})

test_that("exp_compute: flood fraction = 1 gives exposure equal to population", {
  pop  <- make_rast(500)
  frac <- make_rast(1)

  result <- exp_compute(pop, frac)
  pop_vals     <- terra::values(result$pop_total,   na.rm = TRUE)
  exposed_vals <- terra::values(result$pop_exposed, na.rm = TRUE)
  expect_equal(pop_vals, exposed_vals)
})

test_that("exp_compute: custom layer names are applied", {
  pop   <- make_rast(100)
  frac  <- make_rast(0.5)

  result <- exp_compute(pop, frac,
                        pop_layer_name     = "my_pop",
                        exposed_layer_name = "my_exposed")

  expect_equal(names(result$pop_total),   "my_pop")
  expect_equal(names(result$pop_exposed), "my_exposed")
})

# ---------------------------------------------------------------------------
# exp_build_indicator_tibble()
# ---------------------------------------------------------------------------

test_that("exp_build_indicator_tibble: has all required columns", {
  zonal_sf <- make_poly_sf() |>
    dplyr::mutate(pop_total = 1000, pop_exposed = 300, perc_exposed = 30)

  result <- exp_build_indicator_tibble(
    zonal_sf       = zonal_sf,
    year           = 2025L,
    indicator_name = "Test indicator",
    indicator_code = "tst",
    admin_level    = 2L
  )

  expected_cols <- c("id", "indicator.name", "indicator.code", "country.code",
                     "admin.level", "admin.code", "admin.name", "year",
                     "total.pop", "pop.exposed", "perc.pop.exposed")
  expect_named(result, expected_cols)
})

test_that("exp_build_indicator_tibble: values are correctly mapped", {
  zonal_sf <- make_poly_sf() |>
    dplyr::mutate(pop_total = 2000, pop_exposed = 400, perc_exposed = 20)

  result <- exp_build_indicator_tibble(
    zonal_sf       = zonal_sf,
    year           = 2026L,
    indicator_name = "Women exposed",
    indicator_code = "wraf",
    admin_level    = 2L
  )

  expect_equal(result$country.code,     "TST")
  expect_equal(result$admin.code,       "TST-001")
  expect_equal(result$admin.name,       "TestZone")
  expect_equal(result$year,             2026L)
  expect_equal(result$total.pop,        2000)
  expect_equal(result$pop.exposed,      400)
  expect_equal(result$perc.pop.exposed, 20)
})

test_that("exp_build_indicator_tibble: admin_level is stored as integer", {
  zonal_sf <- make_poly_sf() |>
    dplyr::mutate(pop_total = 1, pop_exposed = 0, perc_exposed = 0)

  result <- exp_build_indicator_tibble(zonal_sf, 2020L, "x", "x", admin_level = 2L)
  expect_type(result$admin.level, "integer")
  expect_equal(result$admin.level, 2L)
})

# ---------------------------------------------------------------------------
# exp_summarise_by_country()
# ---------------------------------------------------------------------------

test_that("exp_summarise_by_country: aggregates two districts into one country row", {
  tbl <- tibble::tibble(
    country.code   = c("KEN", "KEN"),
    year           = c(2025L, 2025L),
    indicator.name = c("test", "test"),
    total.pop      = c(1000, 2000),
    pop.exposed    = c(100,  400)
  )

  result <- exp_summarise_by_country(tbl)

  expect_equal(nrow(result), 1L)
  expect_equal(result$total.pop,   3000)
  expect_equal(result$pop.exposed, 500)
  expect_equal(round(result$perc.pop.exposed, 6),
               round(500 / 3000 * 100, 6))
})

test_that("exp_summarise_by_country: produces one row per country-year combination", {
  tbl <- tibble::tibble(
    country.code   = c("KEN", "KEN", "GMB", "GMB"),
    year           = c(2025L, 2026L, 2025L, 2026L),
    indicator.name = rep("test", 4),
    total.pop      = c(1000, 1100, 500, 520),
    pop.exposed    = c(100,  110,   50,  52)
  )

  result <- exp_summarise_by_country(tbl)
  expect_equal(nrow(result), 4L)
})

test_that("exp_summarise_by_country: perc.pop.exposed is recomputed correctly", {
  tbl <- tibble::tibble(
    country.code   = "KEN",
    year           = 2025L,
    indicator.name = "test",
    total.pop      = c(100, 900),
    pop.exposed    = c(10,  90)
  )

  result <- exp_summarise_by_country(tbl)
  expect_equal(result$perc.pop.exposed, 10)  # 100/1000 * 100
})
