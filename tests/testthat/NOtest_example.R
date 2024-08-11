# tests/testthat/test_example.R
library(testthat)

# Source the R script that contains the function to be tested
# source("../path/to/your_script.R")

example_function <- function(a, b) {
  return(a + b)
}

test_that("example_function works correctly", {
  result <- example_function(1, 2)
  expect_equal(result, 3)
})

test_that("intentionally wrong example", {
  result <- example_function(1, 2)
  expect_equal(result, 4)
})


# run the test with
# library(testthat)
# test_dir("tests/testthat")

# useful info:
# vignette("special-files")
