# run_tests.R

library(testthat)

# test_file("tests/testthat/test_compare_baseline.R")
# test_file("tests/testthat/test_compare_salary_headcount.R")
test_dir("tests/testthat")


# how to create a test runner

# run_tests <- function() {
#   # Load setup once
#   source("path/to/your/setup/script.R")
#   
#   # Run tests
#   test_file("tests/test_file1.R")
#   test_file("tests/test_file2.R")
#   # Add more test files as needed
# }
# 
# # Run the tests
# run_tests()