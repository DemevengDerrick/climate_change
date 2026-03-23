# tests/testthat/test-viz_utils.R
# Tests for R/viz_utils.R
# Run via: source("tests/run_tests.R")

# Helper: build a minimal indicator tibble for a single country-year
make_indicators <- function(country = "KEN", years = 2025:2026,
                             total = 1000, exposed = 100) {
  tibble::tibble(
    country.code     = country,
    year             = as.integer(years),
    indicator.name   = "test",
    total.pop        = as.numeric(total),
    pop.exposed      = as.numeric(exposed),
    perc.pop.exposed = (exposed / total) * 100
  )
}

# Helper: build a minimal sf for the pyramid
make_pyramid_input <- function() {
  tibble::tibble(
    sex         = c("female", "male", "total"),
    age_group   = c("15", "15", "15"),
    year        = c(2025L, 2025L, 2025L),
    pop_exposed = c(500,   600,   1100),
    pop_tot     = c(1000,  1200,  2200)
  )
}

# ---------------------------------------------------------------------------
# viz_prep_pyramid_data()
# ---------------------------------------------------------------------------

test_that("viz_prep_pyramid_data: filters out 'total' rows", {
  result <- viz_prep_pyramid_data(make_pyramid_input())
  expect_false("total" %in% result$sex)
  expect_equal(nrow(result), 2L)
})

test_that("viz_prep_pyramid_data: female pop_exposed is negated", {
  result <- viz_prep_pyramid_data(make_pyramid_input())
  female_row <- result[result$sex == "female", ]
  expect_true(female_row$pop_exposed < 0)
})

test_that("viz_prep_pyramid_data: male pop_exposed is positive", {
  result <- viz_prep_pyramid_data(make_pyramid_input())
  male_row <- result[result$sex == "male", ]
  expect_true(male_row$pop_exposed > 0)
})

test_that("viz_prep_pyramid_data: female perc_exposed is negated", {
  result <- viz_prep_pyramid_data(make_pyramid_input())
  female_row <- result[result$sex == "female", ]
  expect_true(female_row$perc_exposed < 0)
})

test_that("viz_prep_pyramid_data: perc_exposed computed correctly for male", {
  result   <- viz_prep_pyramid_data(make_pyramid_input())
  male_row <- result[result$sex == "male", ]
  # 600 / 1200 * 100 = 50
  expect_equal(male_row$perc_exposed, 50)
})

test_that("viz_prep_pyramid_data: returns tibble", {
  result <- viz_prep_pyramid_data(make_pyramid_input())
  expect_s3_class(result, "tbl_df")
})

test_that("viz_prep_pyramid_data: pop_tot is negated for female", {
  result     <- viz_prep_pyramid_data(make_pyramid_input())
  female_row <- result[result$sex == "female", ]
  expect_true(female_row$pop_tot < 0)
})

# ---------------------------------------------------------------------------
# viz_trend_histogram()
# ---------------------------------------------------------------------------

test_that("viz_trend_histogram: returns a ggplot object", {
  ind    <- make_indicators()
  result <- viz_trend_histogram(ind)
  expect_s3_class(result, "gg")
})

test_that("viz_trend_histogram: does not save when out_path is NULL", {
  ind      <- make_indicators()
  tmp_path <- tempfile(fileext = ".jpg")
  viz_trend_histogram(ind)              # no out_path
  expect_false(file.exists(tmp_path))   # nothing written
})

test_that("viz_trend_histogram: saves file when out_path is provided", {
  ind      <- make_indicators()
  tmp_path <- file.path(tempdir(), "trend_test.jpg")
  viz_trend_histogram(ind, out_path = tmp_path)
  expect_true(file.exists(tmp_path))
  file.remove(tmp_path)
})

# ---------------------------------------------------------------------------
# viz_choropleth()
# ---------------------------------------------------------------------------

test_that("viz_choropleth: returns a ggplot object", {
  # Minimal sf with required columns
  admin_sf <- sf::st_sf(
    perc.pop.exposed = c(10, 20),
    year             = c(2025L, 2025L),
    geometry         = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0,0,1,0,1,1,0,1,0,0), ncol=2, byrow=TRUE))),
      sf::st_polygon(list(matrix(c(1,0,2,0,2,1,1,1,1,0), ncol=2, byrow=TRUE))),
      crs = 4326
    )
  )
  ctry_sf <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0,0,2,0,2,1,0,1,0,0), ncol=2, byrow=TRUE))),
      crs = 4326
    )
  )

  result <- viz_choropleth(admin_sf, ctry_sf, "TST")
  expect_s3_class(result, "gg")
})

test_that("viz_choropleth: saves file when out_path is provided", {
  admin_sf <- sf::st_sf(
    perc.pop.exposed = 15,
    year             = 2025L,
    geometry         = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0,0,1,0,1,1,0,1,0,0), ncol=2, byrow=TRUE))),
      crs = 4326
    )
  )
  ctry_sf <- admin_sf

  tmp_path <- file.path(tempdir(), "choropleth_test.jpg")
  viz_choropleth(admin_sf, ctry_sf, "TST", out_path = tmp_path)
  expect_true(file.exists(tmp_path))
  file.remove(tmp_path)
})

# ---------------------------------------------------------------------------
# viz_exposure_pyramid()
# ---------------------------------------------------------------------------

test_that("viz_exposure_pyramid: returns a patchwork/gg object", {
  pyramid_data <- viz_prep_pyramid_data(make_pyramid_input())
  result       <- viz_exposure_pyramid(pyramid_data, "TST")
  expect_true(inherits(result, "gg") || inherits(result, "patchwork"))
})

test_that("viz_exposure_pyramid: saves file when out_path is provided", {
  pyramid_data <- viz_prep_pyramid_data(make_pyramid_input())
  tmp_path     <- file.path(tempdir(), "pyramid_test.jpg")
  viz_exposure_pyramid(pyramid_data, "TST", out_path = tmp_path, dpi = 72)
  expect_true(file.exists(tmp_path))
  file.remove(tmp_path)
})
