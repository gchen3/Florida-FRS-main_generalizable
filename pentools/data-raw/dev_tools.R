
# load packages needed for development process ----
library(devtools)
library(usethis)
library(pkgload)
library(fs)
library(lintr)
library(styler)
library(roxygen2)
library(testthat)

# load package to be developed ----
load_all()

use_mit_license()
