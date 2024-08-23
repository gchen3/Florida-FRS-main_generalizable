


source("E:/R_projects/projects/Florida-FRS-main_generalizable/refactor/tools/libraries.R")

# source("E:/R_projects/projects/Florida-FRS-main_generalizable/refactor/tests/testthat/initial_setup.R")

oldpath <- here::here("refactor", "reason_results", "reason_workspace.RData")
load(oldpath, oldws <- new.env())

objname <- "regular_wf_data"
objname <- "funding_list"
objname <- "current_amort_layers_table"


f <- function(objname){
  test_that(paste0("Compare ", objname), {
    # objname <- "male_mp_final_table"
    cat("Testing object: ", objname, "\n")
    old_object <- get(objname, envir = oldws)
    new_object <- get(objname, envir = .GlobalEnv)
    expect_equal(new_object, old_object, info = paste("Mismatch in", objname))
    cat("\n")
  })
}
f(objname)

prefixes <- str_replace(params$class_names_no_drop_frs_, " ", "_")
(objnames <- paste0(prefixes, "_wf_data"))

purrr::walk(objnames, f)


