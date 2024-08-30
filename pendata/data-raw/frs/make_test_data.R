# test against Reason data after frs revisions ------------------------------------------------

dir_reason <- r"(E:\R_projects\projects\Florida-FRS-main_generalizable\refactor\reason_results)"
fpath <- fs::path(dir_reason, "reason_workspace.RData")
test_data_path <- here::here("tests", "testthat", "testdata")

load(fpath, oldws <- new.env())
ns(oldws)
oldws$base_general_mort_table_ # very raw, don't bother to compare
oldws$base_general_mort_table # cleaned by Reason, wide

# cnames <- oldws$class_names_no_drop_frs |> str_replace(" ", "_")
# keep_object_names <- paste0("base_", cnames, "_mort_table")

keep_object_names <- paste0("base_", c("general", "regular", "teacher"), "_mort_table")

reason <- new.env()
for (name in keep_object_names){
  print(name)
  reason[[name]] <- oldws[[name]]
}
names(reason)
saveRDS(reason, file = fs::path(test_data_path, "reason.rds"))

reason <- readRDS(file = fs::path(test_data_path, "reason.rds"))
ns(reason)
reason$base_general_mort_table

