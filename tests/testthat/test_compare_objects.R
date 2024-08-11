# tests/testthat/test_compare_objects.R

library(testthat)

test_that("Compare baseline_funding", {
  old_object <- get("baseline_funding", envir = oldws)
  new_object <- get("baseline_funding", envir = newws)
  expect_equal(old_object, new_object)
})

test_that("Compare baseline_liability", {
  old_object <- get("baseline_liability", envir = oldws)
  new_object <- get("baseline_liability", envir = newws)
  expect_equal(old_object, new_object)
})

# test_that("Object comparison test 2", {
#   old_object <- get("another_object_name", envir = old_env)
#   new_object <- get("another_object_name", envir = new_env)
#   expect_equal(old_object, new_object)
# })

# run with testthat::test_dir("tests/testthat")
# or testthat::test_dir("tests/testthat", reporter = "progress")
# testthat::test_dir("tests/testthat", reporter = "summary")
# testthat::test_dir("tests/testthat", reporter = ListReporter)
# or devtools::test()

