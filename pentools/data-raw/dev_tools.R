
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

# create tests ----
use_test("pv")


# before build - if needed - delete Rcheck ----

# Define the path to the .Rcheck directory
rcheck_dir <- "../pentools.Rcheck"

# Function to remove the .Rcheck directory if it exists
clean_rcheck_directory <- function(path) {
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
build(path = tempdir())  # don't create tarball
install() # install from source



