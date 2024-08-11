
# run the following in a CLEAN environment --------------------------------

# run the full FRS Florida model

source(here::here("Florida FRS master.R")) # load the model

# generate baseline results

baseline_funding <- get_funding_data()
baseline_liability <- get_liability_data()

# save the entire workspace
save.image(here::here("new_results", "reason_workspace.RData"))


# additional new results --------------------------------------------------



