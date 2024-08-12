# tests/testthat/test_compare_baseline.R

library(testthat)

test_that("Compare male_mp_final_table expect_equal", {
  objname <- "male_mp_final_table"
  cat("\nTesting object: ", objname, "\n")
  old_object <- get(objname, envir = oldws)
  new_object <- get(objname, envir = newws)
  expect_equal(new_object, old_object, info = paste("Mismatch in", objname))
})

test_that("Compare female_mp_final_table expect_equal", {
  objname <- "female_mp_final_table"
  cat("\nTesting object: ", objname, "\n")
  old_object <- get(objname, envir = oldws)
  new_object <- get(objname, envir = newws)
  expect_equal(new_object, old_object, info = paste("Mismatch in", objname))
})

classes <- c("regular", "special", "admin", "eco", "eso", "judges", "senior_management")

walk(classes, function(class) {
  test_that(sprintf("Compare mortality table for class: %s", class), {
    objname <- paste0(class, "_mort_table")
    cat("\nTesting object: ", objname, "\n")
    old_object <- get(objname, envir = oldws)
    new_object <- get(objname, envir = newws)
    
    expect_equal(new_object, old_object, info = paste("Mismatch in", objname))
  })
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

