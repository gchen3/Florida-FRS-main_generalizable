# tests/testthat/test_compare_salary_headcount_entrants.R

# library(testthat)
library(purrr)
library(stringr)
library(tidyverse)

cat("\n\n")
print("running test on ALL Reason objects, except functions and selected other objects")

reason_object_names <- ls(envir = oldws)
newws_object_names <- ls(envir = newws)

# Filter out functions
reason_functions <- sapply(reason_object_names, function(x) is.function(get(x, envir = oldws)))
non_function_reason_names <- setdiff(reason_object_names, names(which(reason_functions)))

reason_names_in_newws <- intersect(non_function_reason_names, newws_object_names)

# look at globals to see if there are any we want to exclude
# reason_globals <- stringr::str_subset(reason_names_in_newws, "_$")

# Pre-process old data
cat("\n")
print("Preprocessing Reason results to change 'senior management' to 'senior_management'")
print("..changing class column of init_funding_data in Reason results")
oldws$init_funding_data <- oldws$init_funding_data |>
  mutate(class = ifelse(class == "senior management",
                        "senior_management",
                        class))

print("..changing element name of baseline_funding in Reason results")
names(oldws$baseline_funding)[names(oldws$baseline_funding) == "senior management"] <- "senior_management"

print("..changing class column of current_amort_layers_table in Reason results")
oldws$current_amort_layers_table <- oldws$current_amort_layers_table |>
  mutate(class = ifelse(class == "senior management",
                        "senior_management",
                        class))

print("..changing class column of current_amort_layers_table_ in Reason results")
oldws$current_amort_layers_table_ <- oldws$current_amort_layers_table_ |>
  mutate(class = ifelse(class == "senior management",
                        "senior_management",
                        class))

print("..changing element name of funding_list in Reason results")
names(oldws$funding_list)[names(oldws$funding_list) == "senior management"] <- "senior_management"


# done modifying Reason results

excludes <- c("FileName")

names_to_compare <- setdiff(reason_names_in_newws, excludes)


cat("\n")
print("Reason non-function objects that are not in new workspace and thus not compared (if any)")
print(setdiff(non_function_reason_names, newws_object_names))

cat("\n")
print("Reason non-function objects that are in new workspace but excluded from comparisons (if any)")
print(setdiff(reason_names_in_newws, names_to_compare))

cat("\n")
print("New-results objects that are not in Reason workspace and thus not compared (if any)")
print(setdiff(newws_object_names, reason_object_names))

cat("\n")
print("New results being compared to Reason counterparts")
print(names_to_compare)

cat("\n\n")


walk(names_to_compare, function(objname) {
  #cat("Testing object: ", objname, "\n")
  old_object <- get(objname, envir = oldws)
  new_object <- get(objname, envir = newws)  
  test_that(" new result-object matches Reason counterpart",{
      expect_equal(new_object, old_object, info = paste("Mismatch in", objname))
    })
  #cat("\n")
})


# test_that(" new result-object matches Reason counterpart",{
#   walk(names_to_compare, function(objname) {
#     cat("Testing object: ", objname, "\n")
#     old_object <- get(objname, envir = oldws)
#     new_object <- get(objname, envir = newws)
#     expect_equal(new_object, old_object, info = paste("Mismatch in", objname))
#     cat("\n")
#   })
# })

