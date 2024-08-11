# run_tests.R

library(testthat)

# Run tests with Summary reporter
# test_dir("tests/testthat", reporter = ProgressReporter)
test_results <- test_file("tests/testthat/test_compare_objects.R")
print(test_results)

# run this with source("run_tests.R") in the console
