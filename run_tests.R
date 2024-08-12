# run_tests.R

library(testthat)

# test_file("tests/testthat/test_compare_baseline.R")
# test_file("tests/testthat/test_compare_salary_headcount.R")
test_dir("tests/testthat")


# Run tests with Summary reporter
# test_dir("tests/testthat", reporter = ProgressReporter)
# test_results <- test_file("tests/testthat/test_compare_baseline.R")
# print(test_results)
# 
# test_results <- test_file("tests/testthat/test_compare_salary_headcount.R")
# print(test_results)

# run this with source("run_tests.R") in the console

# source("tests/testthat.R")

# here's a way to run individual files without rerunning setup
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
