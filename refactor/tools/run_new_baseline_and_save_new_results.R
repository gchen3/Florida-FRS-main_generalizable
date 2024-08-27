cat("\014") # clear console in Windows
rm(list = ls())


# run the following in a CLEAN environment with the branch of interest checked out --------------------------------

# MANUALLY RESET AND CLEAR ENVIRONMENT WITH Ctrl-Shift-F10 !!!!!  
# Then source this file (or run line by line)

system.time(source(here::here("refactor", "R", "FRS_master.R"))) # load and build the full FRS Florida model

# generate baseline results
system.time(baseline_funding <- get_funding_data(funding_list = funding_list, 
                                                 current_amort_layers_table = current_amort_layers_table, 
                                                 wf_data_list = wf_data_list,
                                                 entrant_profile_table_list = entrant_profile_table_list, 
                                                 salary_headcount_table_list = salary_headcount_table_list, 
                                                 mort_table_list = mort_table_list, 
                                                 mort_retire_table_list = mort_retire_table_list, 
                                                 separation_rate_table_list = separation_rate_table_list,
                                                 params=params)) # about 60 secs tot, ~ 10 seconds for a single class

system.time(baseline_liability <- get_liability_data(class = "regular", 
                                                     wf_data = regular_wf_data, 
                                                     ben_payment_current = params$regular_ben_payment_current_,
                                                     retiree_pop_current = params$regular_retiree_pop_current_,
                                                     pvfb_term_current = params$regular_pvfb_term_current_,
                                                     entrant_profile_table = regular_entrant_profile_table,
                                                     salary_headcount_table = regular_salary_headcount_table,
                                                     mort_table = regular_mort_table,
                                                     mort_retire_table = regular_mort_retire_table,
                                                     separation_rate_table = regular_separation_rate_table,
                                                     params = params)
            ) # ~ 10 seconds for a single class

system.time(save.image(here::here("refactor", "new_results", "new_workspace.RData"))) # save the entire workspace, ~ 15-20 secs

# run one or the other of the following tests

# system.time(source(fs::path(tooldir, "run_tests.R"))) # run selected tests
# tooldir <- here::here("refactor", "tools") # run if needed
system.time(source(fs::path(tooldir, "run_allobjects_tests.R"))) # run tests on all objects appropriate to compare ~ 15 secs


# NOTE:
#   regex for finding global variables:   \w+_(?=\s|$|\)|,|\;)  -- use in RStudio search box

