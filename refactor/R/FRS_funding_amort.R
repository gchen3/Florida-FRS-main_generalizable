# Amortization Tables from "Florida-FRS-main_generalizable/refactor/R/FRS_funding_model_functions_s.R"

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
    current_hire_amo_period_table = current_hire_amo_period_table,
    current_hire_debt_layer_table = current_hire_debt_layer_table,
    current_hire_amo_payment_table = current_hire_amo_payment_table
  ))
}

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
    future_hire_amo_period_table = future_hire_amo_period_table,
    future_hire_debt_layer_table = future_hire_debt_layer_table,
    future_hire_amo_payment_table = future_hire_amo_payment_table
  ))
}