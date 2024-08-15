
# run the following in a CLEAN environment with the branch of interest checked out --------------------------------

# RESET ENVIRONMENT MANUALLY !!!!!  

# Ctrl-Shift-F10 to restart RStudio and clear the environment

system.time(source(here::here("refactor", "Florida FRS master.R"))) # load and run the full FRS Florida model

# generate baseline results
system.time(baseline_funding <- get_funding_data())
system.time(baseline_liability <- get_liability_data())

system.time(save.image(here::here("refactor", "new_results", "new_workspace.RData"))) # save the entire workspace

system.time(source(here::here("refactor", "run_tests.R"))) # run tests

