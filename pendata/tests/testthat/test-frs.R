library(tidyverse)

reformat_base_mortality <- function(etype) {
  dfnew <- frs$base_mort_table |>
    filter(employee_type == etype) |>
    pivot_wider(names_from = c(beneficiary_type, gender), values_from = rate) |>
    select(all_of(names(dfold)))

  # ensure compatibility with Reason object classes
  if(etype == "regular") dfnew <- as.data.frame(dfnew)

  return(dfnew)
}

reason <- readRDS(test_path("testdata", "reason.rds"))

for (etype in c("general", "regular", "teacher")) {
  print(etype)
  dfold <- reason[[paste0("base_", etype, "_mort_table")]]
  dfnew <- reformat_base_mortality(etype)
  expect_equal(dfnew, dfold)
}

