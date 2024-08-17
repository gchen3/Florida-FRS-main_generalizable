
# run the following in a CLEAN environment with the branch of interest checked out --------------------------------

# MANUALLY RESET AND CLEAR ENVIRONMENT WITH Ctrl-Shift-F10 !!!!!  
# Then source this file (or run line by line)

system.time(source(here::here("refactor", "Florida FRS master.R"))) # load and run the full FRS Florida model

# generate baseline results
system.time(baseline_funding <- get_funding_data(funding_list, current_amort_layers_table))
system.time(baseline_liability <- get_liability_data())

system.time(save.image(here::here("refactor", "new_results", "new_workspace.RData"))) # save the entire workspace

system.time(source(here::here("refactor", "run_tests.R"))) # run tests


