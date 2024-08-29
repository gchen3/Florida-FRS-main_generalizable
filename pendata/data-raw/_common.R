
source(here::here("data-raw", "libraries.r"))

ddata <- here::here("data")
draw <- here::here("data-raw")

dsoa <- fs::path(draw, "standard", "soa")

dfrs <- fs::path(draw, "frs")
drds <- fs::path(dfrs, "rds")
dxi <- fs::path(dfrs, "Reports", "extracted inputs")
dfreason <- fs::path(dfrs, "from_reason")
