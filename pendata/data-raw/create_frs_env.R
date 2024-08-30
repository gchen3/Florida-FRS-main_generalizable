
frs <- new.env()
frs$base_mort_table <- readRDS(here::here("data-raw", "frs", "rds", "base_mort_table.rds"))
ns(frs)
usethis::use_data(frs, overwrite = TRUE)
