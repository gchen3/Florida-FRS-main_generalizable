
# run the following in a CLEAN environment with the branch of interest checked out --------------------------------

# MANUALLY RESET AND CLEAR ENVIRONMENT WITH Ctrl-Shift-F10 !!!!!  
# Then source this file (or run line by line)

system.time(source(here::here("refactor", "R", "FRS master.R"))) # load and build the full FRS Florida model

# generate baseline results
system.time(baseline_funding <- get_funding_data(funding_list, current_amort_layers_table, params=params)) # about 60 secs tot, ~ 10 seconds for a single class

system.time(baseline_liability <- get_liability_data(class="regular", params = params)) # ~ 10 seconds for a single class

system.time(save.image(here::here("refactor", "new_results", "new_workspace.RData"))) # save the entire workspace, ~ 10-15 secs

# run one or the other of the following tests

# system.time(source(fs::path(tooldir, "run_tests.R"))) # run selected tests

system.time(source(fs::path(tooldir, "run_allobjects_tests.R"))) # run tests on all objects appropriate to compare ~ 15 secs


# NOTE:
#   regex for finding global variables:   \w+_(?=\s|$|\)|,|\;)  -- use in RStudio search box

