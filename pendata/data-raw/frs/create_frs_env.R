# Create and populate the frs environment

# Create the frs environment
frs <- new.env(parent = emptyenv())

# Load your data into the frs environment
frs$base_mort_table <- readRDS("path/to/your/raw/data.rds")

# Save the entire frs environment
usethis::use_data(frs, internal = TRUE, overwrite = TRUE)





# frs <- new.env()
# frs$base_mort_table <- readRDS(here::here("data-raw", "frs", "rds", "base_mort_table.rds"))
# # Add other data as needed
#
# # Save the frs environment as internal data
# usethis::use_data(frs, internal = TRUE, overwrite = TRUE)




# frs <- new.env()
# frs$base_mort_table <- readRDS(here::here("data-raw", "frs", "rds", "base_mort_table.rds"))
# ns(frs)
# usethis::use_data(frs, overwrite = TRUE)
