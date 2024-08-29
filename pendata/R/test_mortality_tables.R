
# folders -----------------------------------------------------------------

dir_pd <- r"(E:\R_projects\projects\Florida-FRS-main_generalizable\pendata)"
dir_draw <- fs::path(dir_pd, "data-raw")
dir_r <- fs::path(dir_pd, "R")
dir_reason <- fs::path(dir_draw, "reason_results")
dir_soa <- fs::path(dir_draw, "external", "soa")


# libraries ---------------------------------------------------------------

source(fs::path(dir_draw, "libraries.r"))
