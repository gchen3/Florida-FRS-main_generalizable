# tests/testthat/test_compare_baseline.R

library(testthat)

cat("\n\n")
print("running separation-rate table tests")

classes <- c("regular", "special", "admin", "eco", "eso", "judges", "senior_management")

cat("\n\n")
print("separation-rate table tests")
walk(classes, function(class) {
  test_that(sprintf("Compare separation-rate table for class: %s", class), {
    objname <- paste0(class, "_separation_rate_table")
    cat("\nTesting object: ", objname, "\n")
    old_object <- get(objname, envir = oldws)
    new_object <- get(objname, envir = newws)
    
    expect_equal(new_object, old_object, info = paste("Mismatch in", objname))
  })
})

