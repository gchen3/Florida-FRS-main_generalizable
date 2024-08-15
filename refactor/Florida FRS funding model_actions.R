
get_all_classes_funding_list <- function(class_names){
  funding_list <- lapply(class_names, get_funding_table)
  names(funding_list) <- class_names
  
  return(funding_list)
}

get_current_amort_layers_summary_table <- function(current_amort_layers_table_){
  #Summarize current amortization layers
  current_amort_layers_table <- current_amort_layers_table_ %>% 
    mutate(amo_period = if_else(amo_period == "n/a", "20", amo_period),
           amo_period = as.numeric(amo_period)) %>% 
    group_by(class, amo_period) %>%
    summarise(amo_balance = sum(amo_balance)) %>% 
    #make sure that the amo periods are arranged in descending order
    arrange(class, desc(amo_period)) %>% 
    ungroup()
  
  return(current_amort_layers_table)
}


# groups for class names:
class_names <- init_funding_data$class
class_names_no_drop_frs <- class_names[!class_names %in% c("drop", "frs")]
class_names_no_frs <- class_names[!class_names %in% c("frs")]

funding_list <- get_all_classes_funding_list(class_names)
current_amort_layers_table <- get_current_amort_layers_summary_table(current_amort_layers_table_)










