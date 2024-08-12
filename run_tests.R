# run_tests.R

library(testthat)

run_tests <- function() {
  # Load setup once
  source("tests/testthat/initial_setup.R")

  # Run tests
  test_file("tests/testthat/test_compare_baseline.R")
  test_file("tests/testthat/test_compare_salary_headcount.R")
  # Add more test files as needed
}

# Run the tests
run_tests()


# Cleanup code
# Remove objects or environments created during setup
rm(list = ls(envir = oldws), envir = oldws)
rm(oldws)

rm(list = ls(envir = newws), envir = newws)
rm(newws)




# test_file("tests/testthat/test_compare_baseline.R")
# test_file("tests/testthat/test_compare_salary_headcount.R")
# test_dir("tests/testthat")