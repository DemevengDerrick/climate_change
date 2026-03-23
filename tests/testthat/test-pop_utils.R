# tests/testthat/test-pop_utils.R
# Tests for R/pop_utils.R
# Run via: source("tests/run_tests.R")

# ---------------------------------------------------------------------------
# pop_parse_filename()
# ---------------------------------------------------------------------------

test_that("pop_parse_filename: parses female age-band file correctly", {
  f      <- "ken_f_15_2020_UN_100m_population_v1.tif"
  result <- pop_parse_filename(f)

  expect_equal(result$sex_code,   "f")
  expect_equal(result$sex,        "female")
  expect_equal(result$age_group,  "15")
  expect_equal(result$year,       2020L)
  expect_equal(result$resolution, "100m")
})

test_that("pop_parse_filename: parses male age-band file correctly", {
  f      <- "ken_m_20_2025_UN_100m_population_v1.tif"
  result <- pop_parse_filename(f)

  expect_equal(result$sex_code,   "m")
  expect_equal(result$sex,        "male")
  expect_equal(result$age_group,  "20")
  expect_equal(result$year,       2025L)
})

test_that("pop_parse_filename: parses total-female (T_F) file — no age group", {
  f      <- "gmb_T_F_2022_CN_100m_population_v1.tif"
  result <- pop_parse_filename(f)

  expect_equal(result$sex_code,  "T_F")
  expect_equal(result$sex,       "female_total")
  expect_true(is.na(result$age_group))
  expect_equal(result$year,      2022L)
})

test_that("pop_parse_filename: parses total-male (T_M) file — no age group", {
  f      <- "gmb_T_M_2023_CN_100m_population_v1.tif"
  result <- pop_parse_filename(f)

  expect_equal(result$sex_code,  "T_M")
  expect_equal(result$sex,       "male_total")
  expect_true(is.na(result$age_group))
})

test_that("pop_parse_filename: parses total (t) file — no age group", {
  f      <- "ken_t_2021_UN_1km_population_v1.tif"
  result <- pop_parse_filename(f)

  expect_equal(result$sex_code,  "t")
  expect_equal(result$sex,       "total")
  expect_true(is.na(result$age_group))
})

test_that("pop_parse_filename: unknown sex code gives NA sex", {
  f      <- "ken_x_15_2020_UN_100m_pop.tif"
  result <- pop_parse_filename(f)
  expect_true(is.na(result$sex))
})

test_that("pop_parse_filename: preserves full file path in `file` column", {
  path   <- "input/pop/ken_f_15_2020_UN_100m_population_v1.tif"
  result <- pop_parse_filename(path)
  expect_equal(result$file, path)
})

test_that("pop_parse_filename: returns a one-row tibble", {
  f      <- "ken_f_15_2020_UN_100m_population_v1.tif"
  result <- pop_parse_filename(f)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
})

# ---------------------------------------------------------------------------
# pop_parse_filenames()
# ---------------------------------------------------------------------------

test_that("pop_parse_filenames: returns correct row count", {
  files <- c(
    "ken_f_15_2020_UN_100m_pop_v1.tif",
    "ken_m_15_2020_UN_100m_pop_v1.tif",
    "ken_f_20_2021_UN_100m_pop_v1.tif"
  )
  result <- pop_parse_filenames(files)
  expect_equal(nrow(result), 3L)
})

test_that("pop_parse_filenames: sex values are correctly assigned per row", {
  files  <- c(
    "ken_f_15_2020_UN_100m_pop_v1.tif",
    "ken_m_15_2020_UN_100m_pop_v1.tif"
  )
  result <- pop_parse_filenames(files)
  expect_equal(result$sex, c("female", "male"))
})

test_that("pop_parse_filenames: years are correctly assigned per row", {
  files  <- c(
    "ken_f_15_2025_UN_100m_pop_v1.tif",
    "ken_f_15_2026_UN_100m_pop_v1.tif"
  )
  result <- pop_parse_filenames(files)
  expect_equal(result$year, c(2025L, 2026L))
})

# ---------------------------------------------------------------------------
# pop_clip()
# ---------------------------------------------------------------------------

test_that("pop_clip: returns a SpatRaster", {
  pop <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(pop) <- runif(100)

  boundary <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(0.1, 0.1, 0.9, 0.1, 0.9, 0.9, 0.1, 0.9, 0.1, 0.1),
        ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  )

  result <- pop_clip(pop, boundary)
  expect_true(inherits(result, "SpatRaster"))
})

test_that("pop_clip: values outside boundary are masked (NA)", {
  pop <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(pop) <- 1

  # Boundary covers only the top-left quarter
  boundary <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(0, 0.5, 0.5, 0.5, 0.5, 1.0, 0, 1.0, 0, 0.5),
        ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  )

  result <- pop_clip(pop, boundary)
  # Some cells outside the boundary should be NA
  expect_true(any(is.na(terra::values(result))))
})
