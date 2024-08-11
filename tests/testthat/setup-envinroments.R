# tests/testthat/setup-environments.R

# make old workspace environment (Reason results) and new workspace environment (new results) available to all tests


# text-fixtures is supposed to be a better / newer way to do this.
# explore it: https://testthat.r-lib.org/articles/test-fixtures.html

library(testthat)

print("loading the Reason results and new results environments...")

# Load the old workspace environment
oldpath <- here::here("reason_results", "reason_workspace.RData")
load(oldpath, oldws <- new.env())

# Load the new workspace environment
newpath <- here::here("new_results", "new_workspace.RData")
load(newpath, newws <- new.env())

print("beginning tests...")


# Make the environments available to all tests
testthat::setup({
  assign("oldws", oldws, envir = .GlobalEnv)
  assign("newws", newws, envir = .GlobalEnv)
})

# Clean up after tests
testthat::teardown({
  rm(oldws, envir = .GlobalEnv)
  rm(newws, envir = .GlobalEnv)
})
