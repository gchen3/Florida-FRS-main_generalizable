library(tidyverse)


# get_current_hire_amo_period_table ---------------------------------------

class_name <- "regular"
current_amort_layers_table <- params$current_amort_layers_table
class_amo_layers_table
amo_col_num <- max(params$current_amort_layers_table$amo_period, params$amo_period_new_ + params$funding_lag_)  
params <- params


get_current_hire_amo_period_table <- function(class_name,
                                              current_amort_layers_table,
                                              class_amo_layers_table,
                                              amo_col_num,
                                              params) {
  
  class_amo_layers_table <- current_amort_layers_table %>% 
    filter(class == class_name)
  
  current_periods <- class_amo_layers_table$amo_period
  future_periods <- params$amo_period_new_ + params$funding_lag_
  length(current_periods) <- amo_col_num
  length(future_periods) <- amo_col_num
  
  current_hire_amo_period_table <- rbind(current_periods, 
                                         matrix(future_periods,
                                                nrow = params$model_period_,
                                                ncol = amo_col_num,
                                                byrow = TRUE))
  
  rownames(current_hire_amo_period_table) <- NULL         #Remove row names
  
  #Put the amo periods on diagonal rows
  for (i in 2:nrow(current_hire_amo_period_table)) {
    for (j in 2:ncol(current_hire_amo_period_table)) {
      current_hire_amo_period_table[i, j] <- max(current_hire_amo_period_table[i-1, j-1] - 1, 0)
    }
  }
  
  #Turn all NAs in the table to 0s
  current_hire_amo_period_table[is.na(current_hire_amo_period_table)] <- 0
  
  return(current_hire_amo_period_table)
}


current_hire_amo_period <- get_current_hire_amo_period_table (class_name,
                                   current_amort_layers_table,
                                   class_amo_layers_table,
                                   amo_col_num,
                                   params)
current_hire_amo_period

# get_future_hire_amo_period_table ----------------------------------------
class_name <- "regular"
amo_col_num <- max(params$current_amort_layers_table$amo_period, params$amo_period_new_ + params$funding_lag_)  

get_future_hire_amo_period_table (class_name,
                                             amo_col_num,
                                             params) 

get_future_hire_amo_period_table <- function(class_name,
                                             amo_col_num,
                                             params) {
  
  future_periods <- params$amo_period_new_ + params$funding_lag_
  length(future_periods) <- amo_col_num
  
  future_hire_amo_period_table <- matrix(future_periods, 
                                         nrow = params$model_period_ + 1,
                                         ncol = amo_col_num,
                                         byrow = TRUE) 
  
  #Put the amo periods on diagonal rows
  for (i in 2:nrow(future_hire_amo_period_table)) {
    for (j in 2:ncol(future_hire_amo_period_table)) {
      future_hire_amo_period_table[i,j] <- max(future_hire_amo_period_table[i-1,j-1] - 1, 0)
    }
  }
  
  #Turn all NAs in the table to 0s
  future_hire_amo_period_table[is.na(future_hire_amo_period_table)] <- 0
  
  return(future_hire_amo_period_table)
}




# get_current_hire_debt_layer_table ---------------------------------------

class_name <- "regular"
current_amort_layers_table <- params$current_amort_layers_table
amo_col_num <- max(params$current_amort_layers_table$amo_period, params$amo_period_new_ + params$funding_lag_)

get_current_hire_debt_layer_table <- function(class_name,
                                              current_amort_layers_table,
                                              amo_col_num,
                                              params
) {
  
  current_hire_debt_layer_table <- matrix(0, nrow = params$model_period_ + 1, ncol = amo_col_num + 1)
  
  current_hire_debt_layers <- current_amort_layers_table %>% 
    filter(class == class_name) %>% 
    arrange(desc(amo_period)) %>% 
    pull(amo_balance)
  
  current_hire_debt_layer_table[1,1:length(current_hire_debt_layers)] <- current_hire_debt_layers
  
  return(current_hire_debt_layer_table)
}


current_hire_debt_layer <- get_current_hire_debt_layer_table(class_name,
                                              current_amort_layers_table,
                                              amo_col_num,
                                              params)

current_hire_debt_layer

# get_current_hire_amo_payment_table --------------------------------------
class_name <- "regular"
current_hire_amo_payment_table <- params$current_hire_amo_payment_table
current_hire_debt_layer_list
current_hire_amo_period_list
amo_col_num <- max(params$current_amort_layers_table$amo_period, params$amo_period_new_ + params$funding_lag_)
amo_pay_growth <- ifelse(params$amo_method_ == "level $", 0, params$amo_pay_growth_)
params

get_current_hire_amo_payment_table (class_name,
                                               current_hire_amo_payment_table,
                                               current_hire_debt_layer_list,
                                               current_hire_amo_period_list,
                                               amo_col_num,
                                               amo_pay_growth, 
                                               params)

get_current_hire_amo_payment_table <- function(class_name,
                                               current_hire_amo_payment_table,
                                               current_hire_debt_layer_list,
                                               current_hire_amo_period_list,
                                               amo_col_num,
                                               amo_pay_growth, # this can be different than params$amo_pay_growth_
                                               params) {
  
  current_hire_amo_payment_table <- matrix(0, nrow = params$model_period_ + 1, ncol = amo_col_num)
  
  init_debt_layers <- current_hire_debt_layer_list[1,1:amo_col_num]
  
  amo_periods <- current_hire_amo_period_list[1,1:amo_col_num]
  
  current_hire_amo_payment_table[1,1:amo_col_num] <- pentools::get_pmt(pv = init_debt_layers,
                                                             r = params$dr_old_,
                                                             g = amo_pay_growth,
                                                             nper = amo_periods,
                                                             t = 0.5)
  if (params$funding_lag_ > 0) {
    current_hire_amo_payment_table[1,1:params$funding_lag_] <- 0
  }
  
  return(current_hire_amo_payment_table)
}


