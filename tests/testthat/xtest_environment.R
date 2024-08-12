# tests/testthat/test_environment.R

library(testthat)
library(withr)

test_that("environment is loaded", {
  # Check if oldws is an environment
  expect_true(is.environment(oldws))
  
  # Schedule cleanup of the environment
  withr::defer(rm(list = ls(envir = oldws), envir = oldws))
})