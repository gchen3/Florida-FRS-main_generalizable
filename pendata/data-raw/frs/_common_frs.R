
draw <- here::here("data-raw")
dfrs <- fs::path(draw, "frs")
drds <- fs::path(dfrs, "rds")
dxi <- fs::path(dfrs, "Reports", "extracted inputs")
dfreason <- fs::path(dfrs, "from_reason")

# source(here::here("data-raw", "libraries.r"))
source(fs::path(draw, "libraries.r"))
source(fs::path(dfrs, "functions.r"))
source(fs::path(dfrs, "functions_tier.r"))

frs_constants <- readRDS(fs::path(drds, "frs_constants.rds"))

FileName <- "Florida FRS inputs.xlsx"
fullpath <- fs::path(dfrs, FileName)



