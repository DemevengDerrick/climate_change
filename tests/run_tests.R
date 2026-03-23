#################################################################################################
#                                                                                               #
#                                   TEST RUNNER                                                 #
#                                 By Derrick DEMEVENG                                           #
#                                                                                               #
#  Usage:                                                                                       #
#    From RStudio:  source("tests/run_tests.R")                                                 #
#    From terminal: Rscript tests/run_tests.R                                                   #
#                                                                                               #
#################################################################################################

# Ensure working directory is the project root
if (!file.exists("R/flood_utils.R")) {
  stop(
    "Run this script from the project root.\n",
    "  In RStudio: Session > Set Working Directory > To Source File Location\n",
    "  In terminal: cd <project_root> && Rscript tests/run_tests.R"
  )
}

# Install/load required packages
if (!require("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  testthat,
  # Spatial
  terra, sf,
  # Data wrangling
  dplyr, stringr, tibble, purrr,
  # Visualisation (needed by viz_utils.R)
  ggplot2, scales, ggspatial, patchwork
)

# Load all utility functions
source("R/flood_utils.R")
source("R/pop_utils.R")
source("R/exposure_utils.R")
source("R/viz_utils.R")

# Run all tests
cat("\n======================================================\n")
cat("  Running Climate Change Project Test Suite\n")
cat("======================================================\n\n")

results <- testthat::test_dir(
  "tests/testthat",
  reporter = testthat::ProgressReporter$new(show_praise = TRUE)
)

cat("\n======================================================\n")
summary(results)
