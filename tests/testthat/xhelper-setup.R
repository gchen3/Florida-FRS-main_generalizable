# tests/testthat/helper-setup.R

library(testthat)

testthat::local_edition(3)

print("Defining setup and teardown for the test environment...")

# Setup: Load the environments
setup({
  print("Loading the Reason results and new results environments...")
  
  # Load the old workspace environment
  oldpath <- here::here("reason_results", "reason_workspace.RData")
  load(oldpath, oldws <- new.env())
  
  # Load the new workspace environment
  newpath <- here::here("new_results", "new_workspace.RData")
  load(newpath, newws <- new.env())
  
  # Make the environments available to all tests
  assign("oldws", oldws, envir = .GlobalEnv)
  assign("newws", newws, envir = .GlobalEnv)
  
  print("Environments loaded and ready for tests.")
})

# Teardown: Clean up the environments
teardown({
  print("Cleaning up the environments...")
  
  rm(oldws, envir = .GlobalEnv)
  rm(newws, envir = .GlobalEnv)
  
  print("Environments cleaned up.")
})

