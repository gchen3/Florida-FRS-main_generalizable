

# get funding and amortization data

params$funding_list <- get_all_classes_funding_list(params$init_funding_data, params)

params$current_amort_layers_table <- get_current_amort_layers_summary_table(params$current_amort_layers_table_)
