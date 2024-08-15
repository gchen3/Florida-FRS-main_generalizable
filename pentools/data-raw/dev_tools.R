
# load packages needed for development process ----
library(devtools)
library(usethis)
library(pkgload)
library(fs)
library(lintr)
library(styler)
library(roxygen2)
library(testthat)

# prepare package for development ----
use_mit_license()
usethis::use_data_raw()
usethis::use_testthat(3)

# list all packages to be used replace pname with package name
use_package("pname")

use_package("zoo") # doesn't look like this is actually required


# update namespace for functions used replace fname with function name
usethis::use_import_from("pname", "fname")


# load package to be developed ----
load_all()

# write and document functions ----

# Once the function definition exists, put your cursor somewhere in it and do
# Code > Insert Roxygen Skeleton to get a great head start on the roxygen
# comment. Alt-Ctrl-Shift-R

devtools::document()


# create tests ----
use_test("functions")


# run tests ----

# .rs.restartR() # if needed

devtools::load_all()
testthat::test_file("tests/testthat/test-functions.R")

devtools::test()

# before build - if needed - delete Rcheck ----

# Define the path to the .Rcheck directory
rcheck_dir <- "../pentools.Rcheck"

clean_rcheck_directory <- function(path) {
  # Function to remove the .Rcheck directory if it exists
  if (dir.exists(path)) {
    message("Removing existing .Rcheck directory...")
    unlink(path, recursive = TRUE)
  }
}

# Clean up any previous .Rcheck directory
clean_rcheck_directory(rcheck_dir)

# check without tarball ----
check(build_args = c("--no-build-vignettes", "--no-manual"), args = "--no-build-vignettes --no-manual")

# build and install ----
remove.packages("pentools")
clean_rcheck_directory(rcheck_dir)
devtools::document()
build(path = tempdir())  # don't create tarball
install() # install from source

# end ----

