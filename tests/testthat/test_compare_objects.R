# tests/testthat/test_compare_objects.R

library(testthat)

test_that("Compare baseline_funding expect_equal", {
  old_object <- get("baseline_funding", envir = oldws)
  new_object <- get("baseline_funding", envir = newws)
  expect_equal(new_object, old_object)
})

test_that("Compare baseline_liability expect_equal", {
  old_object <- get("baseline_liability", envir = oldws)
  new_object <- get("baseline_liability", envir = newws)
  expect_equal(new_object, old_object)
})

# test_that("Compare baseline_funding expect_identical", {
#   old_object <- get("baseline_funding", envir = oldws)
#   new_object <- get("baseline_funding", envir = newws)
#   print(all.equal(old_object, new_object))
#   expect_identical(old_object, new_object)
# })

# run with 
#   testthat::test_dir("tests/testthat", reporter = ListReporter)
#   testthat::test_file("tests/testthat/test_compare_objects.R", reporter = ListReporter)

