

class_names <- init_funding_data$class
funding_list <- lapply(class_names, get_funding_table)
names(funding_list) <- class_names


#Summarize current amortization layers
current_amort_layers_table <- current_amort_layers_table_ %>% 
  mutate(amo_period = if_else(amo_period == "n/a", "20", amo_period),
         amo_period = as.numeric(amo_period)) %>% 
  group_by(class, amo_period) %>%
  summarise(amo_balance = sum(amo_balance)) %>% 
  #make sure that the amo periods are arranged in descending order
  arrange(class, desc(amo_period)) %>% 
  ungroup()

#More groups for class names:
class_names_no_drop_frs <- class_names[!class_names %in% c("drop", "frs")]
class_names_no_frs <- class_names[!class_names %in% c("frs")]



