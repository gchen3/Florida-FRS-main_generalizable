

# folders -----------------------------------------------------------------

dir_pd <- r"(E:\R_projects\projects\Florida-FRS-main_generalizable\pendata)"
dir_draw <- fs::path(dir_pd, "data-raw")
dir_r <- fs::path(dir_pd, "R")
dir_reason <- fs::path(dir_draw, "reason_results")
dir_soa <- fs::path(dir_draw, "external", "soa")


# libraries ---------------------------------------------------------------

source(fs::path(dir_draw, "libraries.r"))



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



