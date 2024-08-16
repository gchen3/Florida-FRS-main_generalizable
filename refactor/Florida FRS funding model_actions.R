

# groups for class names:
class_names <- init_funding_data$class
class_names_no_drop_frs <- class_names[!class_names %in% c("drop", "frs")]
class_names_no_frs <- class_names[!class_names %in% c("frs")]


# get funding and amortization data
funding_list <- get_all_classes_funding_list(class_names, init_funding_data)
current_amort_layers_table <- get_current_amort_layers_summary_table(current_amort_layers_table_)










