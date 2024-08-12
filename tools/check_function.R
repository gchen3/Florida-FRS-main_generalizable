source(here::here("tools", "libraries.R")) # load all tools
source(here::here("tools", "functions.R"))

fun <- get_salary_headcount_table
fun <- get_tier

fun <- get_base_mort_table
fun <- clean_mp_table
fun <- get_mp_final_table
fun <- get_mort_table
fun <- get_mort_retire_table

codetools::checkUsage(fun)

codetools::findGlobals(fun, merge = FALSE)
