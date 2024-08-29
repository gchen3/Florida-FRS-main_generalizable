
# NOTES:

#   1) To force execution of all calculations, even if .qmd files have not changed
#      since last quarto render, edit the relevant _quarto.yml file in the
#      folder that is going to be rendered to make sure the
#      execution: freeze: parameter is set to false.

#   2) To force re-downloading or initial downloading of SOA files, do #1 above
#      AND make sure download="true" in the quarto_render() call below. To
#      prevent downloading, set download="false"


# setup -------------------------------------------------------------------

rm(list = ls())

draw <- here::here("data-raw")

source(fs::path(draw, "libraries.r"))

# Render the entire project

a <- proc.time()
quarto_render(draw, execute_params=list(download="true"), as_job = FALSE)
b <- proc.time()

b - a


# Build and install the updated package ----

(package_root <- usethis::proj_get())
devtools::document(package_root)
devtools::check(package_root) # not essential to check for CRAN requirements but good practice
devtools::build(package_root)
devtools::install(package_root)


# look at newly installed package ----
library(pendata)
# data(package="pendata")
data(frs)
names(frs)
