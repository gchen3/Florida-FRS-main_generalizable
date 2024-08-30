library(tidyverse)
library(pendata)


test_that("reformat_base_mortality works correctly", {
  reason <- readRDS(test_path("testdata", "reason.rds"))

  for (etype in c("general", "regular", "teacher")) {
    print(etype)
    dfold <- reason[[paste0("base_", etype, "_mort_table")]]
    dfnew <- frs$reformat_base_mortality(etype) |>
      select(all_of(names(dfold)))
    expect_equal(dfnew, dfold)
  }
})
