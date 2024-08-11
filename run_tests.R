# run_tests.R

library(testthat)

# Run tests with Summary reporter
test_dir("tests/testthat", reporter = "progress")

# run this with source("run_tests.R") in the console
