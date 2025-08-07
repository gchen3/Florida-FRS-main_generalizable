library(tidyverse)
library(here)
source(here::here("refactor", "R", "FRS_master_GC.R"))

# Functions from "Florida-FRS-main_generalizable/refactor/R/FRS_funding_model_functions_s.R"

# get_current_hire_amo_period_table ---------------------------------------
class_name <- "regular"
current_amort_layers_table <- params$current_amort_layers_table
class_amo_layers_table <- current_amort_layers_table %>% 
  filter(class == class_name)
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
current_hire_debt_layer
current_hire_amo_period
amo_col_num <- max(params$current_amort_layers_table$amo_period, params$amo_period_new_ + params$funding_lag_)
amo_pay_growth <- ifelse(params$amo_method_ == "level $", 0, params$amo_pay_growth_)

get_current_hire_amo_payment_table <- function(class_name,
                                               current_hire_amo_payment_table,
                                               current_hire_debt_layer_list,
                                               current_hire_amo_period_list,
                                               amo_col_num,
                                               amo_pay_growth, # this can be different than params$amo_pay_growth_
                                               params) {
  
  current_hire_amo_payment_table <- matrix(0, nrow = params$model_period_ + 1, ncol = amo_col_num)
  
  init_debt_layers <- current_hire_debt_layer[1,1:amo_col_num]
  
  amo_periods <- current_hire_amo_period[1,1:amo_col_num]
  
  current_hire_amo_payment_table[1,1:amo_col_num] <- get_pmt(pv = init_debt_layers,
                                                             r = params$dr_old_,
                                                             g = amo_pay_growth,
                                                             nper = amo_periods,
                                                             t = 0.5)
  if (params$funding_lag_ > 0) {
    current_hire_amo_payment_table[1,1:params$funding_lag_] <- 0
  }
  
  return(current_hire_amo_payment_table)
}

current_hire_amo_payment_table <- get_current_hire_amo_payment_table (class_name,
                                               current_hire_amo_payment_table,
                                               current_hire_debt_layer,
                                               current_hire_amo_period,
                                               amo_col_num,
                                               amo_pay_growth, 
                                               params)


current_hire_amo_payment_table


# Combined current amortization tables ------------------------------------

get_current_hire_amortization_tables <- function(class_name,
                                                 current_amort_layers_table,
                                                 amo_col_num,
                                                 amo_pay_growth,
                                                 params) {
  # 1. Filter and sort class-specific amortization data
  class_amo_layers <- current_amort_layers_table %>%
    filter(class == class_name) %>%
    arrange(desc(amo_period))
  
  # 2. Initialize matrices
  nrow_total <- params$model_period_ + 1
  current_hire_amo_period_table <- matrix(0, nrow = nrow_total, ncol = amo_col_num)
  current_hire_debt_layer_table <- matrix(0, nrow = nrow_total, ncol = amo_col_num + 1)
  current_hire_amo_payment_table <- matrix(0, nrow = nrow_total, ncol = amo_col_num)
  
  # 3. Fill the first row of period table (this part can be further simplified)
  current_periods <- class_amo_layers$amo_period
  future_periods <- params$amo_period_new_ + params$funding_lag_
  length(current_periods) <- amo_col_num
  length(future_periods) <- amo_col_num
  current_hire_amo_period_table[1, ] <- current_periods
  current_hire_amo_period_table[2:nrow_total, ] <- matrix(future_periods, 
                                                           nrow = nrow_total - 1, 
                                                           ncol = amo_col_num, 
                                                           byrow = TRUE)
  
  # 4. Fill out diagonal amortization periods
  for (i in 2:nrow_total) {
    for (j in 2:amo_col_num) {
      current_hire_amo_period_table[i, j] <- max(current_hire_amo_period_table[i - 1, j - 1] - 1, 0)
    }
  }
  
  # 5. Fill the first row of debt layer table
  current_hire_debt_layer_table[1, 1:length(class_amo_layers$amo_balance)] <- class_amo_layers$amo_balance
  
  # 6. Fill the first row of payment table
  init_debt_layers <- current_hire_debt_layer_table[1, 1:amo_col_num]
  amo_periods <- current_hire_amo_period_table[1, 1:amo_col_num]
  
  current_hire_amo_payment_table[1, 1:amo_col_num] <- get_pmt(
    pv = init_debt_layers,
    r = params$dr_old_,
    g = amo_pay_growth,
    nper = amo_periods,
    t = 0.5
  )
  
  if (params$funding_lag_ > 0) {
    current_hire_amo_payment_table[1, 1:params$funding_lag_] <- 0
  }
  
  # 7. Replace NAs with 0 in all tables
  current_hire_amo_period_table[is.na(current_hire_amo_period_table)] <- 0
  current_hire_debt_layer_table[is.na(current_hire_debt_layer_table)] <- 0
  current_hire_amo_payment_table[is.na(current_hire_amo_payment_table)] <- 0
  
  return(list(
    amo_period_table = current_hire_amo_period_table,
    debt_layer_table = current_hire_debt_layer_table,
    amo_payment_table = current_hire_amo_payment_table
  ))
}


current_hire_amortization_tables <- get_current_hire_amortization_tables(class_name,
                                                 current_amort_layers_table,
                                                 amo_col_num,
                                                 amo_pay_growth,
                                                 params)

identical(current_hire_amortization_tables$amo_payment_table, current_hire_amo_payment_table)
identical(unname(current_hire_amortization_tables$amo_period_table), unname(current_hire_amo_period))
identical(current_hire_amortization_tables$debt_layer_table, current_hire_debt_layer)

# Future ------------------------------------------------------------------

class_name <- "regular"
amo_col_num <- max(params$current_amort_layers_table$amo_period, params$amo_period_new_ + params$funding_lag_)  

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

future_hire_amo_period_table <- get_future_hire_amo_period_table (class_name,
                                                                  amo_col_num,
                                                                  params) 

future_hire_amo_period_table

get_future_hire_debt_layer_table <- function(class_name,
                                             amo_col_num,
                                             params) {
  # UAAL layers tables for new members
  future_hire_debt_layer_table <- matrix(0, nrow = params$model_period_ + 1, ncol = amo_col_num + 1)
  return(future_hire_debt_layer_table)
}

future_hire_debt_layer_table <- get_future_hire_debt_layer_table(class_name,
                                 amo_col_num,
                                 params)

get_future_hire_amo_payment_table <- function(class_name,
                                              amo_col_num,
                                              params) {
  # Amo payment tables for new members
  future_hire_amo_payment_table <- matrix(0, nrow = params$model_period_ + 1, ncol = amo_col_num)
  return(future_hire_amo_payment_table)
}

future_hire_amo_payment_table <- get_future_hire_amo_payment_table(class_name,
                                                                      amo_col_num,
                                                                      params)

## Combine future functions -------------------------------------------------------
class_name <- "regular"
amo_col_num <- max(params$current_amort_layers_table$amo_period, params$amo_period_new_ + params$funding_lag_)  

get_future_hire_amortization_tables <- function(class_name, amo_col_num, params) {
  
  nrow_total <- params$model_period_ + 1
  
  # 1. Create future_hire_amo_period_table (diagonal decrementing matrix)
  future_periods <- params$amo_period_new_ + params$funding_lag_
  length(future_periods) <- amo_col_num
  
  future_hire_amo_period_table <- matrix(future_periods, 
                                         nrow = nrow_total, 
                                         ncol = amo_col_num, 
                                         byrow = TRUE)
  
  for (i in 2:nrow_total) {
    for (j in 2:amo_col_num) {
      future_hire_amo_period_table[i, j] <- max(future_hire_amo_period_table[i - 1, j - 1] - 1, 0)
    }
  }
  
  future_hire_amo_period_table[is.na(future_hire_amo_period_table)] <- 0
  
  # 2. Create future_hire_debt_layer_table (zero matrix with 1 extra column)
  future_hire_debt_layer_table <- matrix(0, nrow = nrow_total, ncol = amo_col_num + 1)
  
  # 3. Create future_hire_amo_payment_table (zero matrix)
  future_hire_amo_payment_table <- matrix(0, nrow = nrow_total, ncol = amo_col_num)
  
  # Return all tables in a list
  return(list(
    amo_period_table = future_hire_amo_period_table,
    debt_layer_table = future_hire_debt_layer_table,
    amo_payment_table = future_hire_amo_payment_table
  ))
}

future_hire_amortization_tables <- get_future_hire_amortization_tables(class_name, 
                                                                        amo_col_num, 
                                                                        params)

identical(future_hire_amortization_tables$amo_payment_table, future_hire_amo_payment_table)
identical(future_hire_amortization_tables$amo_period_table, future_hire_amo_period_table)
identical(future_hire_amortization_tables$debt_layer_table, future_hire_debt_layer_table)

