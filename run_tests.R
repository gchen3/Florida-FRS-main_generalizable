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

#   # Load setup once