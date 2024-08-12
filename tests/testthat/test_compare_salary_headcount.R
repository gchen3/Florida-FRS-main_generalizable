# tests/testthat/test_compare_salary_headcount.R

library(testthat)
library(purrr)

classes <- c("regular", "special", "admin", "eco", "eso", "judges", "senior_management")

walk(classes, function(class) {
  test_that(sprintf("Compare salary headcount table for class: %s", class), {
    objname <- paste0(class, "_salary_headcount_table")
    cat("\nTesting object: ", objname, "\n")
    old_object <- get(objname, envir = oldws)
    new_object <- get(objname, envir = newws)
    
    expect_equal(new_object, old_object, info = paste("Mismatch in", objname))
  })
})


walk(classes, function(class) {
  test_that(sprintf("Compare salary headcount table for class: %s", class), {
    objname <- paste0(class, "_entrant_profile_table")
    cat("\nTesting object: ", objname, "\n")
    old_object <- get(objname, envir = oldws)
    new_object <- get(objname, envir = newws)
    
    expect_equal(new_object, old_object, info = paste("Mismatch in", objname))
  })
})




# objname <- paste0(class, "_entrant_profile_table")
# old_object <- get(objname, envir = oldws)
# new_object <- get(objname, envir = newws)
# expect_equal(new_object, old_object)





# test_that("Compare salary headcount tables", {
#   f <- function(class){
#     objname <- paste0(class, "_salary_headcount_table")
#     old_object <- get(objname, envir = oldws)
#     new_object <- get(objname, envir = newws)
#     expect_equal(new_object, old_object)
#     
#     objname <- paste0(class, "_entrant_profile_table")
#     old_object <- get(objname, envir = oldws)
#     new_object <- get(objname, envir = newws)
#     expect_equal(new_object, old_object)
#   }
#   
#   f("regular")
#   f("special")
#   f("admin")
#   f("eco")
#   f("eso")
#   f("judges")
#   f("senior_management")
#   
# })


# test_that("Compare baseline_funding expect_identical", {
#   old_object <- get("baseline_funding", envir = oldws)
#   new_object <- get("baseline_funding", envir = newws)
#   print(all.equal(old_object, new_object))
#   expect_identical(old_object, new_object)
# })

# run with 
#   testthat::test_dir("tests/testthat", reporter = ListReporter)
#   testthat::test_file("tests/testthat/test_compare_objects.R", reporter = ListReporter)

