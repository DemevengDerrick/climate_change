# Standard testthat package runner — called by R CMD check
library(testthat)
library(climatechange)

test_check("climatechange")
