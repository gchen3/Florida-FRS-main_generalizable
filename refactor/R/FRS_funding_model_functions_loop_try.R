#
##                        Funding Model                        ####
#


# functions for funding list and amortization layers ----------------------

# these functions are NOT needed in the get_funding_data function, but are called before it is called


get_all_classes_funding_list <- function(init_funding_data,
                                         params){
  
  funding_list <- lapply(params$class_names_, get_funding_table, init_funding_data, params)
  names(funding_list) <- params$class_names_
  
  return(funding_list)
}


##GC: You don't need to use this function if the current_amort_layers_table is already cleaned (no n/a; summarize by class and amortization period)
get_current_amort_layers_summary_table <- function(current_amort_layers_table){
  
  #Summarize current amortization layers
  current_amort_layers_table <- current_amort_layers_table %>% 
    mutate(amo_period = if_else(amo_period == "n/a", "20", amo_period),
           amo_period = as.numeric(amo_period)) %>% 
    summarise(amo_balance = sum(amo_balance),
              .by = c(class, amo_period)) %>% 
    #make sure that the amo periods are arranged in descending order
    arrange(class, desc(amo_period))
  
  return(current_amort_layers_table)
}


# get_current_hire_amo_payment_table <- function(class_name,
#                                                current_hire_amo_payment_table,
#                                                current_hire_debt_layer_list,
#                                                current_hire_amo_period_list,
#                                                amo_col_num,
#                                                amo_pay_growth, # this can be different than params$amo_pay_growth_
#                                                params) {
#   
#   current_hire_amo_payment_table <- matrix(0, nrow = params$model_period_ + 1, ncol = amo_col_num)
#   
#   init_debt_layers <- current_hire_debt_layer_list[[class_name]][1,1:amo_col_num]
#   
#   amo_periods <- current_hire_amo_period_list[[class_name]][1,1:amo_col_num]
#   
#   current_hire_amo_payment_table[1,1:amo_col_num] <- get_pmt(pv = init_debt_layers,
#                                                              r = params$dr_old_,
#                                                              g = amo_pay_growth,
#                                                              nper = amo_periods,
#                                                              t = 0.5)
#   if (params$funding_lag_ > 0) {
#     current_hire_amo_payment_table[1,1:params$funding_lag_] <- 0
#   }
#   
#   return(current_hire_amo_payment_table)
# }
# 
# 
# get_current_hire_amo_period_table <- function(class_name,
#                                               current_amort_layers_table,
#                                               class_amo_layers_table,
#                                               amo_col_num,
#                                               params) {
#   
#   class_amo_layers_table <- current_amort_layers_table %>% 
#     filter(class == class_name)
#   
#   current_periods <- class_amo_layers_table$amo_period
#   future_periods <- params$amo_period_new_ + params$funding_lag_
#   length(current_periods) <- amo_col_num
#   length(future_periods) <- amo_col_num
#   
#   current_hire_amo_period_table <- rbind(current_periods, 
#                                          matrix(future_periods,
#                                                 nrow = params$model_period_,
#                                                 ncol = amo_col_num,
#                                                 byrow = TRUE))
#   
#   rownames(current_hire_amo_period_table) <- NULL         #Remove row names
#   
#   #Put the amo periods on diagonal rows
#   for (i in 2:nrow(current_hire_amo_period_table)) {
#     for (j in 2:ncol(current_hire_amo_period_table)) {
#       current_hire_amo_period_table[i, j] <- max(current_hire_amo_period_table[i-1, j-1] - 1, 0)
#     }
#   }
#   
#   #Turn all NAs in the table to 0s
#   current_hire_amo_period_table[is.na(current_hire_amo_period_table)] <- 0
#   
#   return(current_hire_amo_period_table)
# }
# 
# 
# get_current_hire_debt_layer_table <- function(class_name,
#                                               current_amort_layers_table,
#                                               amo_col_num,
#                                               params
# ) {
#   
#   current_hire_debt_layer_table <- matrix(0, nrow = params$model_period_ + 1, ncol = amo_col_num + 1)
#   
#   current_hire_debt_layers <- current_amort_layers_table %>% 
#     filter(class == class_name) %>% 
#     arrange(desc(amo_period)) %>% 
#     pull(amo_balance)
#   
#   current_hire_debt_layer_table[1,1:length(current_hire_debt_layers)] <- current_hire_debt_layers
#   
#   return(current_hire_debt_layer_table)
# }


#### Data preparation
#Create 9 empty data frames from the init_funding_data (representing 7 classes, DROP, and FRS system), then put them in a list to store funding outputs for these entities
get_funding_table <- function(class_name, 
                              init_funding_data,
                              params) {
  funding_table <- init_funding_data %>% 
    filter(class == class_name) %>% 
    select(-class) %>%
    add_row(year = (params$start_year_ + 1):(params$start_year_ + params$model_period_))
  
  funding_table[is.na(funding_table)] <- 0
  
  return(funding_table)
}


# get_future_hire_amo_payment_table <- function(class_name,
#                                               amo_col_num,
#                                               params) {
#   # Amo payment tables for new members
#   future_hire_amo_payment_table <- matrix(0, nrow = params$model_period_ + 1, ncol = amo_col_num)
#   return(future_hire_amo_payment_table)
# }
# 
# 
# get_future_hire_amo_period_table <- function(class_name,
#                                              amo_col_num,
#                                              params) {
#   
#   future_periods <- params$amo_period_new_ + params$funding_lag_
#   length(future_periods) <- amo_col_num
#   
#   future_hire_amo_period_table <- matrix(future_periods, 
#                                          nrow = params$model_period_ + 1,
#                                          ncol = amo_col_num,
#                                          byrow = TRUE) 
#   
#   #Put the amo periods on diagonal rows
#   for (i in 2:nrow(future_hire_amo_period_table)) {
#     for (j in 2:ncol(future_hire_amo_period_table)) {
#       future_hire_amo_period_table[i,j] <- max(future_hire_amo_period_table[i-1,j-1] - 1, 0)
#     }
#   }
#   
#   #Turn all NAs in the table to 0s
#   future_hire_amo_period_table[is.na(future_hire_amo_period_table)] <- 0
#   
#   return(future_hire_amo_period_table)
# }
# 
# 
# get_future_hire_debt_layer_table <- function(class_name,
#                                              amo_col_num,
#                                              params) {
#   # UAAL layers tables for new members
#   future_hire_debt_layer_table <- matrix(0, nrow = params$model_period_ + 1, ncol = amo_col_num + 1)
#   return(future_hire_debt_layer_table)
# }


# inner_loop1_payroll_benefits <- function(i,
#                                          funding_list,
#                                          liability_list,
#                                          frs_fund,
#                                          params){
#   
#   for (class in params$class_names_no_drop_frs_) {
#     # djb: it looks like no class values rely on frs values in this loop, so we could move frs entirely out of the loop
#     
#     #Do the assignment below to declutter the code
#     class_fund <- funding_list[[class]]
#     class_liab <- liability_list[[class]]
#     
#     #Payroll projection
#     class_fund$total_payroll[i] <- class_fund$total_payroll[i-1] * (1 + params$payroll_growth_) # lagged value
#     
#     class_fund$payroll_db_legacy[i] <- class_fund$total_payroll[i] * class_fund$payroll_db_legacy_ratio[i]
#     class_fund$payroll_db_new[i] <- class_fund$total_payroll[i] * class_fund$payroll_db_new_ratio[i]
#     class_fund$payroll_dc_legacy[i] <- class_fund$total_payroll[i] * class_fund$payroll_dc_legacy_ratio[i]
#     class_fund$payroll_dc_new[i] <- class_fund$total_payroll[i] * class_fund$payroll_dc_new_ratio[i]
#     
#     # GC: Below is the code to use estimated values instead of estimated ratios multipled by payroll, not ready to incoporate yet.
#     # class_fund$payroll_db_legacy[i] <- liab_data$payroll_db_legacy_est
#     # class_fund$payroll_db_new[i] <- liab_data$payroll_db_new_est
#     # class_fund$payroll_dc_legacy[i] <- liab_data$payroll_dc_legacy_est
#     # class_fund$payroll_dc_new[i] <- liab_data$payroll_dc_new_est
#     
#     # djb update frs values with class totals no lags -- could be consolidated
#     # GC commented out, but not yet consolidate
#     
#     # frs_fund$total_payroll[i] <- frs_fund$total_payroll[i] + class_fund$total_payroll[i]
#     # frs_fund$payroll_db_legacy[i] <- frs_fund$payroll_db_legacy[i] + class_fund$payroll_db_legacy[i]
#     # frs_fund$payroll_db_new[i] <- frs_fund$payroll_db_new[i] + class_fund$payroll_db_new[i]
#     # frs_fund$payroll_dc_legacy[i] <- frs_fund$payroll_dc_legacy[i] + class_fund$payroll_dc_legacy[i]
#     # frs_fund$payroll_dc_new[i] <- frs_fund$payroll_dc_new[i] + class_fund$payroll_dc_new[i]
#     
#     #Benefit payments and refunds projection
#     class_fund$ben_payment_legacy[i] <- class_liab$retire_ben_db_legacy_est[i] + 
#       class_liab$retire_ben_current_est[i] + 
#       class_liab$retire_ben_term_est[i]
#     class_fund$refund_legacy[i] <- class_liab$refund_db_legacy_est[i]
#     class_fund$ben_payment_new[i] <- class_liab$retire_ben_db_new_est[i]
#     class_fund$refund_new[i] <- class_liab$refund_db_new_est[i]
#     
#     class_fund$total_ben_payment[i] <- class_fund$ben_payment_legacy[i] + class_fund$ben_payment_new[i]
#     class_fund$total_refund[i] <- class_fund$refund_legacy[i] + class_fund$refund_new[i]
#     
#     # djb update frs values with class totals, no lags -- could be consolidated
#     # GC commented out, but not yet consolidate
#     
#     # frs_fund$ben_payment_legacy[i] <- frs_fund$ben_payment_legacy[i] + class_fund$ben_payment_legacy[i]
#     # frs_fund$refund_legacy[i] <- frs_fund$refund_legacy[i] + class_fund$refund_legacy[i]
#     # frs_fund$ben_payment_new[i] <- frs_fund$ben_payment_new[i] + class_fund$ben_payment_new[i]
#     # frs_fund$refund_new[i] <- frs_fund$refund_new[i] + class_fund$refund_new[i]
#     # 
#     # frs_fund$total_ben_payment[i] <- frs_fund$total_ben_payment[i] + class_fund$total_ben_payment[i]
#     # frs_fund$total_refund[i] <- frs_fund$total_refund[i] + class_fund$total_refund[i]
#     
#     #Normal cost projection
#     class_fund$nc_legacy[i] <- class_fund$nc_rate_db_legacy[i] * class_fund$payroll_db_legacy[i]
#     class_fund$nc_new[i] <- class_fund$nc_rate_db_new[i] * class_fund$payroll_db_new[i]
#     class_fund$total_nc_rate[i] <- (class_fund$nc_legacy[i] + class_fund$nc_new[i]) / 
#       (class_fund$payroll_db_legacy[i] + class_fund$payroll_db_new[i])
#     
#     # djb update frs values with class totals, no lags -- could be consolidated
#     # GC commented out, but not yet consolidate
#     
#     # frs_fund$nc_legacy[i] <- frs_fund$nc_legacy[i] + class_fund$nc_legacy[i]
#     # frs_fund$nc_new[i] <- frs_fund$nc_new[i] + class_fund$nc_new[i]
#     # frs_fund$total_nc_rate[i] <- (frs_fund$nc_legacy[i] + frs_fund$nc_new[i]) / 
#     #   (frs_fund$payroll_db_legacy[i] + frs_fund$payroll_db_new[i])
#     
#     #Accrued liability projection
#     class_fund$liability_gain_loss_legacy[i] <- class_liab$liability_gain_loss_legacy_est[i]
#     class_fund$liability_gain_loss_new[i] <- class_liab$liability_gain_loss_new_est[i]
#     class_fund$total_liability_gain_loss[i] <- class_liab$total_liability_gain_loss_est[i]
#     
#     # djb: lagged value here
#     class_fund$aal_legacy[i] <- class_fund$aal_legacy[i-1] * (1 + params$dr_current_) +
#       (class_fund$nc_legacy[i] - class_fund$ben_payment_legacy[i] - class_fund$refund_legacy[i]) *
#       (1 + params$dr_current_)^0.5 + 
#       class_fund$liability_gain_loss_legacy[i]
#     
#     # djb: lagged value here
#     class_fund$aal_new[i] <- class_fund$aal_new[i-1] * (1 + params$dr_new_) + 
#       (class_fund$nc_new[i] - class_fund$ben_payment_new[i] - class_fund$refund_new[i]) *
#       (1 + params$dr_new_)^0.5 + 
#       class_fund$liability_gain_loss_new[i]
#     
#     class_fund$total_aal[i] <- class_fund$aal_legacy[i] + class_fund$aal_new[i]
#     
#     # djb: FRS totals in the loop: update with class info no lags, could be consolidated
#     
#     # GC commented out, but not yet consolidate
#     
#     # frs_fund$lia_gain_loss_legacy[i] <- frs_fund$lia_gain_loss_legacy[i] + class_fund$liability_gain_loss_legacy[i]
#     # frs_fund$lia_gain_loss_new[i] <- frs_fund$lia_gain_loss_new[i] + class_fund$liability_gain_loss_new[i]
#     # frs_fund$total_lia_gain_loss[i] <- frs_fund$total_lia_gain_loss[i] + class_fund$total_liability_gain_loss[i]
#     
#     # GC commented out, but not yet consolidate
#     
#     # frs_fund$aal_legacy[i] <- frs_fund$aal_legacy[i] + class_fund$aal_legacy[i]
#     # frs_fund$aal_new[i] <- frs_fund$aal_new[i] + class_fund$aal_new[i]
#     # frs_fund$total_aal[i] <- frs_fund$total_aal[i] + class_fund$total_aal[i]
#     
#     #Assign the class outputs back to the funding_list
#     funding_list[[class]] <- class_fund
#     
#   } #.. end class in class_names_no_drop_frs loop
#   return(list(funding_list = funding_list,
#               frs_fund = frs_fund))
# }

inner_loop1_payroll_benefits <- function(i,
                                         funding_list,
                                         liability_list,
                                         params) {
  
  for (class in params$class_names_no_drop_frs_) {
    cf <- funding_list[[class]]
    cl <- liability_list[[class]]
    
    cf <- within(cf, {
      # --- payroll ---
      total_payroll[i]     <- total_payroll[i-1] * (1 + params$payroll_growth_)
      payroll_db_legacy[i] <- total_payroll[i] * payroll_db_legacy_ratio[i]
      payroll_db_new[i]    <- total_payroll[i] * payroll_db_new_ratio[i]
      payroll_dc_legacy[i] <- total_payroll[i] * payroll_dc_legacy_ratio[i]
      payroll_dc_new[i]    <- total_payroll[i] * payroll_dc_new_ratio[i]
      
      # --- benefits & refunds  ---
      ben_payment_legacy[i] <- cl$retire_ben_db_legacy_est[i] +
        cl$retire_ben_current_est[i] +
        cl$retire_ben_term_est[i]
      refund_legacy[i]      <- cl$refund_db_legacy_est[i]
      ben_payment_new[i]    <- cl$retire_ben_db_new_est[i]
      refund_new[i]         <- cl$refund_db_new_est[i]
      
      total_ben_payment[i]  <- ben_payment_legacy[i] + ben_payment_new[i]
      total_refund[i]       <- refund_legacy[i] + refund_new[i]
      
      # --- normal cost RATES  ---
      total_nc_rate[i] <- (nc_rate_db_legacy[i] * payroll_db_legacy[i] +
                             nc_rate_db_new[i]    * payroll_db_new[i]) /
        (payroll_db_legacy[i] + payroll_db_new[i])
      
      # --- normal cost DOLLARS ---
      nc_legacy[i] <- nc_rate_db_legacy[i] * payroll_db_legacy[i]
      nc_new[i]    <- nc_rate_db_new[i]    * payroll_db_new[i]
      
      # --- liability gains/losses and AAL ---
      liability_gain_loss_legacy[i] <- cl$liability_gain_loss_legacy_est[i]
      liability_gain_loss_new[i]    <- cl$liability_gain_loss_new_est[i]
      total_liability_gain_loss[i]  <- cl$total_liability_gain_loss_est[i]
      
      aal_legacy[i] <- aal_legacy[i-1] * (1 + params$dr_current_) +
        (nc_legacy[i] - ben_payment_legacy[i] - refund_legacy[i]) *
        (1 + params$dr_current_)^0.5 +
        liability_gain_loss_legacy[i]
      
      aal_new[i] <- aal_new[i-1] * (1 + params$dr_new_) +
        (nc_new[i] - ben_payment_new[i] - refund_new[i]) *
        (1 + params$dr_new_)^0.5 +
        liability_gain_loss_new[i]
      
      total_aal[i] <- aal_legacy[i] + aal_new[i]
    })
    
    funding_list[[class]] <- cf
  }
  
  list(funding_list = funding_list)
}

summarize_frs_payroll_benefits <- function(i, funding_list, classes, frs_fund) {
  sum_fields <- function(field) {
    sum(vapply(classes, function(cl) funding_list[[cl]][[field]][i], numeric(1)), na.rm = TRUE)
  }
  
  # Payroll
  frs_fund$total_payroll[i]     <- sum_fields("total_payroll")
  frs_fund$payroll_db_legacy[i] <- sum_fields("payroll_db_legacy")
  frs_fund$payroll_db_new[i]    <- sum_fields("payroll_db_new")
  frs_fund$payroll_dc_legacy[i] <- sum_fields("payroll_dc_legacy")
  frs_fund$payroll_dc_new[i]    <- sum_fields("payroll_dc_new")
  
  # Benefits & refunds
  frs_fund$ben_payment_legacy[i] <- sum_fields("ben_payment_legacy")
  frs_fund$ben_payment_new[i]    <- sum_fields("ben_payment_new")
  frs_fund$refund_legacy[i]      <- sum_fields("refund_legacy")
  frs_fund$refund_new[i]         <- sum_fields("refund_new")
  frs_fund$total_ben_payment[i]  <- sum_fields("total_ben_payment")
  frs_fund$total_refund[i]       <- sum_fields("total_refund")
  
  # NC dollars and total NC rate (BUT NOT component rates)
  frs_fund$nc_legacy[i]     <- sum_fields("nc_legacy")
  frs_fund$nc_new[i]        <- sum_fields("nc_new")
  frs_fund$total_nc_rate[i] <- (frs_fund$nc_legacy[i] + frs_fund$nc_new[i]) /
    (frs_fund$payroll_db_legacy[i] + frs_fund$payroll_db_new[i])
  
  # Liability gain/loss and AAL
  frs_fund$liability_gain_loss_legacy[i] <- sum_fields("liability_gain_loss_legacy")
  frs_fund$liability_gain_loss_new[i]    <- sum_fields("liability_gain_loss_new")
  frs_fund$total_liability_gain_loss[i]  <- sum_fields("total_liability_gain_loss")
  
  frs_fund$aal_legacy[i] <- sum_fields("aal_legacy")
  frs_fund$aal_new[i]    <- sum_fields("aal_new")
  frs_fund$total_aal[i]  <- sum_fields("total_aal")
  
  frs_fund
}


inner_drop1_funding <- function(i,
                                funding_list,
                                frs_fund,
                                params){
  # DANGER, TEMPORARY: not passing variables. will modify them and return
  
  #### Process DROP's payroll, benefit payments, normal cost, and accrued
  #      liability (note that this is a makeshift method for now). Proper
  #      modeling of DROP will be done in the future.
  
  drop_fund <- funding_list$drop
  regular_fund <- funding_list$regular
  
  #DROP payroll projection (no DC payroll for DROP)
  # djb: lagged value here
  # djb: NOTE RELIANCE ON REGULAR
  drop_fund$total_payroll[i] <- drop_fund$total_payroll[i-1] * (1 + params$payroll_growth_)
  drop_fund$payroll_db_legacy[i] <- drop_fund$total_payroll[i] * (regular_fund$payroll_db_legacy_ratio[i] + regular_fund$payroll_dc_legacy_ratio[i])
  drop_fund$payroll_db_new[i] <- drop_fund$total_payroll[i] * (regular_fund$payroll_db_new_ratio[i] + regular_fund$payroll_dc_new_ratio[i])
  
  #DROP benefit payments and refunds projection (based on Regular class' benefit payments and refunds)
  # djb: ask Reason to explain this - why are DROP benefits and refunds based on Regular??
  # djb: RELIANCE ON REGULAR
  # djb: lagged value here
  # GC: Why not using total FRS ratios?
  drop_fund$total_ben_payment[i] <- drop_fund$total_ben_payment[i-1] * 
    regular_fund$total_ben_payment[i] / regular_fund$total_ben_payment[i-1]
  
  drop_fund$total_refund[i] <- drop_fund$total_refund[i-1] * 
    regular_fund$total_refund[i] / regular_fund$total_refund[i-1]
  
  drop_fund$ben_payment_legacy[i] <- drop_fund$total_ben_payment[i] * 
    regular_fund$ben_payment_legacy[i] / regular_fund$total_ben_payment[i]
  
  drop_fund$refund_legacy[i] <- drop_fund$total_refund[i] * 
    regular_fund$refund_legacy[i] / regular_fund$total_refund[i]
  
  drop_fund$ben_payment_new[i] <- drop_fund$total_ben_payment[i] * 
    regular_fund$ben_payment_new[i] / regular_fund$total_ben_payment[i]
  
  drop_fund$refund_new[i] <- drop_fund$total_refund[i] * 
    regular_fund$refund_new[i] / regular_fund$total_refund[i]
  
  #DROP normal cost projection (DROP's normal cost rate = FRS's normal cost rate)
  # djb: RELIANCE ON FRS
  drop_fund$nc_rate_db_legacy[i] <- frs_fund$nc_legacy[i] / frs_fund$payroll_db_legacy[i]
  drop_fund$nc_rate_db_new[i] <- if_else(frs_fund$payroll_db_new[i] == 0, 0, frs_fund$nc_new[i] / frs_fund$payroll_db_new[i])
  
  drop_fund$nc_legacy[i] <- drop_fund$nc_rate_db_legacy[i] * drop_fund$payroll_db_legacy[i]
  drop_fund$nc_new[i] <- drop_fund$nc_rate_db_new[i] * drop_fund$payroll_db_new[i]
  drop_fund$total_nc_rate[i] <- (drop_fund$nc_legacy[i] + drop_fund$nc_new[i]) / 
    (drop_fund$payroll_db_legacy[i] + drop_fund$payroll_db_new[i])
  
  #DROP accrued liability projection
  # djb: lagged value here
  # djb: why the square root of 1 + dr_current
  drop_fund$aal_legacy[i] <- drop_fund$aal_legacy[i-1] * (1 + params$dr_current_) +
    (drop_fund$nc_legacy[i] - drop_fund$ben_payment_legacy[i] - drop_fund$refund_legacy[i]) * 
    (1 + params$dr_current_)^0.5 + 
    drop_fund$liability_gain_loss_legacy[i]
  
  drop_fund$aal_new[i] <- drop_fund$aal_new[i-1] * (1 + params$dr_new_) + 
    (drop_fund$nc_new[i] - drop_fund$ben_payment_new[i] - drop_fund$refund_new[i]) * 
    (1 + params$dr_new_)^0.5 +
    drop_fund$liability_gain_loss_new[i]
  
  drop_fund$total_aal[i] <- drop_fund$aal_legacy[i] + drop_fund$aal_new[i]
  
  #Assign the DROP outputs back to the funding_list
  funding_list$drop <- drop_fund
  return(funding_list)
}


inner_frs_fund1 <- function(i,
                            frs_fund,
                            drop_fund){
  # DANGER, TEMPORARY: not passing variables. will modify them and return
  
  ####Update FRS's numbers after DROP
  #FRS's payroll projection
  frs_fund$total_payroll[i] <- frs_fund$total_payroll[i] + drop_fund$total_payroll[i]
  frs_fund$payroll_db_legacy[i] <- frs_fund$payroll_db_legacy[i] + drop_fund$payroll_db_legacy[i]
  frs_fund$payroll_db_new[i] <- frs_fund$payroll_db_new[i] + drop_fund$payroll_db_new[i]
  
  #FRS's benefit payments and refunds projection
  frs_fund$ben_payment_legacy[i] <- frs_fund$ben_payment_legacy[i] + drop_fund$ben_payment_legacy[i]
  frs_fund$refund_legacy[i] <- frs_fund$refund_legacy[i] + drop_fund$refund_legacy[i]
  frs_fund$ben_payment_new[i] <- frs_fund$ben_payment_new[i] + drop_fund$ben_payment_new[i]
  frs_fund$refund_new[i] <- frs_fund$refund_new[i] + drop_fund$refund_new[i]
  
  frs_fund$total_ben_payment[i] <- frs_fund$total_ben_payment[i] + drop_fund$total_ben_payment[i]
  frs_fund$total_refund[i] <- frs_fund$total_refund[i] + drop_fund$total_refund[i]
  
  #FRS's normal cost projection
  frs_fund$nc_legacy[i] <- frs_fund$nc_legacy[i] + drop_fund$nc_legacy[i]
  frs_fund$nc_new[i] <- frs_fund$nc_new[i] + drop_fund$nc_new[i]
  frs_fund$total_nc_rate[i] <- (frs_fund$nc_legacy[i] + frs_fund$nc_new[i]) / (frs_fund$payroll_db_legacy[i] + frs_fund$payroll_db_new[i])
  
  frs_fund$nc_rate_db_legacy[i] <- frs_fund$nc_legacy[i] / frs_fund$payroll_db_legacy[i]
  frs_fund$nc_rate_db_new[i] <- if_else(frs_fund$payroll_db_new[i] == 0, 0, frs_fund$nc_new[i] / frs_fund$payroll_db_new[i])
  
  #FRS's accrued liability projection
  frs_fund$aal_legacy[i] <- frs_fund$aal_legacy[i] + drop_fund$aal_legacy[i]
  frs_fund$aal_new[i] <- frs_fund$aal_new[i] + drop_fund$aal_new[i]
  frs_fund$total_aal[i] <- frs_fund$total_aal[i] + drop_fund$total_aal[i]
  return(frs_fund)
}


inner_loop2_funding <- function(i,
                                funding_list,
                                frs_fund,
                                current_hire_amo_payment_list,
                                future_hire_amo_payment_list,
                                return_scenarios,
                                return_scen_index,
                                params){
  
  for (class in params$class_names_no_frs_) {
    
    #Do the assignments below to declutter the code
    class_fund <- funding_list[[class]]
    current_hire_amo_pay_table <- current_hire_amo_payment_list[[class]] # djb check the dimensions of these tables
    future_hire_amo_pay_table <- future_hire_amo_payment_list[[class]]
    
    #Normal cost and employee contribution rates
    class_fund$nc_rate_legacy[i] <- class_fund$nc_legacy[i] / class_fund$payroll_db_legacy[i]
    
    if(class_fund$payroll_db_new[i] == 0) {
      class_fund$nc_rate_new[i] <- 0
    } else {
      class_fund$nc_rate_new[i] <- class_fund$nc_new[i] / (class_fund$payroll_db_new[i])  
    } # end if else
    
    class_fund$ee_nc_rate_legacy[i] <- params$db_ee_cont_rate_
    class_fund$ee_nc_rate_new[i] <- params$db_ee_cont_rate_
    
    #Employer contribution rates (DB)
    class_fund$er_nc_rate_legacy[i] <- class_fund$nc_rate_legacy[i] - class_fund$ee_nc_rate_legacy[i]
    class_fund$er_nc_rate_new[i] <- class_fund$nc_rate_new[i] - class_fund$ee_nc_rate_new[i]
    
    class_fund$amo_rate_legacy[i] <- sum(current_hire_amo_pay_table[i-1,]) / class_fund$payroll_db_legacy[i]
    
    if (class_fund$payroll_db_new[i] == 0) {
      class_fund$amo_rate_new[i] <- 0
    } else {
      class_fund$amo_rate_new[i] <- sum(future_hire_amo_pay_table[i-1,]) / class_fund$payroll_db_new[i]
    } # end if else
    
    #Employer contribution rates (DC)
    if (class == "drop") {
      class_fund$er_dc_rate_legacy[i] <- 0
      class_fund$er_dc_rate_new[i] <- 0
    } else {
      
      # djb: temporary fix to prior code that used get to grab a global variable - now get from params
      temp_er_dc_cont_rate <- params[[str_replace(paste0(class, "_er_dc_cont_rate_"), " ", "_")]]
      class_fund$er_dc_rate_legacy[i] <- temp_er_dc_cont_rate
      class_fund$er_dc_rate_new[i] <- temp_er_dc_cont_rate
      rm(temp_er_dc_cont_rate) # clean up. too bad this is in a loop
    } # end if else
    
    #Admin rate
    class_fund$admin_exp_rate[i] <- class_fund$admin_exp_rate[i-1]
    
    #Employee contribution amounts
    class_fund$ee_nc_cont_legacy[i] <- class_fund$ee_nc_rate_legacy[i] * class_fund$payroll_db_legacy[i]
    class_fund$ee_nc_cont_new[i] <- class_fund$ee_nc_rate_new[i] * (class_fund$payroll_db_new[i])
    
    frs_fund$ee_nc_cont_legacy[i] <- frs_fund$ee_nc_cont_legacy[i] + class_fund$ee_nc_cont_legacy[i]
    frs_fund$ee_nc_cont_new[i] <- frs_fund$ee_nc_cont_new[i] + class_fund$ee_nc_cont_new[i]
    
    #Admin expense amounts
    class_fund$admin_exp_legacy[i] <- class_fund$admin_exp_rate[i] * class_fund$payroll_db_legacy[i]
    class_fund$admin_exp_new[i] <- class_fund$admin_exp_rate[i] * (class_fund$payroll_db_new[i])
    
    frs_fund$admin_exp_legacy[i] <- frs_fund$admin_exp_legacy[i] + class_fund$admin_exp_legacy[i]
    frs_fund$admin_exp_new[i] <- frs_fund$admin_exp_new[i] + class_fund$admin_exp_new[i]
    
    #Employer contribution amounts (DB)
    class_fund$er_nc_cont_legacy[i] <- class_fund$er_nc_rate_legacy[i] * class_fund$payroll_db_legacy[i] + class_fund$admin_exp_legacy[i]
    class_fund$er_nc_cont_new[i] <- class_fund$er_nc_rate_new[i] * (class_fund$payroll_db_new[i]) + class_fund$admin_exp_new[i]
    
    class_fund$er_amo_cont_legacy[i] <- class_fund$amo_rate_legacy[i] * class_fund$payroll_db_legacy[i]
    class_fund$er_amo_cont_new[i] <- class_fund$amo_rate_new[i] * (class_fund$payroll_db_new[i])
    class_fund$total_er_db_cont[i] <- class_fund$er_nc_cont_legacy[i] + class_fund$er_nc_cont_new[i] + class_fund$er_amo_cont_legacy[i] + class_fund$er_amo_cont_new[i]
    
    frs_fund$er_nc_cont_legacy[i] <- frs_fund$er_nc_cont_legacy[i] + class_fund$er_nc_cont_legacy[i]
    frs_fund$er_nc_cont_new[i] <- frs_fund$er_nc_cont_new[i] + class_fund$er_nc_cont_new[i]
    
    frs_fund$er_amo_cont_legacy[i] <- frs_fund$er_amo_cont_legacy[i] + class_fund$er_amo_cont_legacy[i]
    frs_fund$er_amo_cont_new[i] <- frs_fund$er_amo_cont_new[i] + class_fund$er_amo_cont_new[i]
    frs_fund$total_er_db_cont[i] <- frs_fund$total_er_db_cont[i] + class_fund$total_er_db_cont[i]
    
    #Employer contribution amounts (DC)
    class_fund$er_dc_cont_legacy[i] <- class_fund$er_dc_rate_legacy[i] * class_fund$payroll_dc_legacy[i]
    class_fund$er_dc_cont_new[i] <- class_fund$er_dc_rate_new[i] * class_fund$payroll_dc_new[i]
    class_fund$total_er_dc_cont[i] <- class_fund$er_dc_cont_legacy[i] + class_fund$er_dc_cont_new[i]
    
    frs_fund$er_dc_cont_legacy[i] <- frs_fund$er_dc_cont_legacy[i] + class_fund$er_dc_cont_legacy[i]
    frs_fund$er_dc_cont_new[i] <- frs_fund$er_dc_cont_new[i] + class_fund$er_dc_cont_new[i]
    frs_fund$total_er_dc_cont[i] <- frs_fund$total_er_dc_cont[i] + class_fund$total_er_dc_cont[i]
    
    #Simulated returns
    class_fund$roa[i] <- return_scenarios[which(return_scenarios$year == class_fund$year[i]), return_scen_index][[1]]
    frs_fund$roa[i] <- return_scenarios[which(return_scenarios$year == class_fund$year[i]), return_scen_index][[1]]
    
    #Solvency contribution and cash flows
    cf_legacy <- class_fund$ee_nc_cont_legacy[i] + 
      class_fund$er_nc_cont_legacy[i] + 
      class_fund$er_amo_cont_legacy[i] - 
      class_fund$ben_payment_legacy[i] - 
      class_fund$refund_legacy[i] - 
      class_fund$admin_exp_legacy[i]
    
    cf_new <- class_fund$ee_nc_cont_new[i] + 
      class_fund$er_nc_cont_new[i] + 
      class_fund$er_amo_cont_new[i] - 
      class_fund$ben_payment_new[i] - 
      class_fund$refund_new[i] - 
      class_fund$admin_exp_new[i]
    
    cf_total <- cf_legacy + cf_new
    
    class_fund$total_solv_cont[i] <- max(-(class_fund$mva[i-1] * (1 + class_fund$roa[i]) + 
                                             cf_total * (1 + class_fund$roa[i])^0.5) / (1 + class_fund$roa[i])^0.5, 0)
    
    class_fund$solv_cont_legacy[i] <- class_fund$total_solv_cont[i] * class_fund$aal_legacy[i] / class_fund$total_aal[i]
    
    class_fund$solv_cont_new[i] <- class_fund$total_solv_cont[i] * class_fund$aal_new[i] / class_fund$total_aal[i]
    
    class_fund$net_cf_legacy[i] <- cf_legacy + class_fund$solv_cont_legacy[i]
    class_fund$net_cf_new[i] <- cf_new + class_fund$solv_cont_new[i]
    
    frs_fund$net_cf_legacy[i] <- frs_fund$net_cf_legacy[i] + class_fund$net_cf_legacy[i]
    frs_fund$net_cf_new[i] <- frs_fund$net_cf_new[i] + class_fund$net_cf_new[i]
    
    #MVA projection
    class_fund$mva_legacy[i] <- class_fund$mva_legacy[i-1] * (1 + class_fund$roa[i]) + class_fund$net_cf_legacy[i] * (1 + class_fund$roa[i])^0.5
    class_fund$mva_new[i] <- class_fund$mva_new[i-1] * (1 + class_fund$roa[i]) + class_fund$net_cf_new[i] * (1 + class_fund$roa[i])^0.5
    class_fund$total_mva[i] <- class_fund$mva_legacy[i] + class_fund$mva_new[i]
    
    frs_fund$mva_legacy[i] <- frs_fund$mva_legacy[i] + class_fund$mva_legacy[i]
    frs_fund$mva_new[i] <- frs_fund$mva_new[i] + class_fund$mva_new[i]
    frs_fund$total_mva[i] <- frs_fund$total_mva[i] + class_fund$total_mva[i]
    
    #AVA development prep
    class_fund$ava_base_legacy[i] <- class_fund$ava_legacy[i-1] + class_fund$net_cf_legacy[i]/2
    class_fund$ava_base_new[i] <- class_fund$ava_new[i-1] + class_fund$net_cf_new[i]/2
    
    #Assign the class outputs back to the funding_list
    funding_list[[class]] <- class_fund
  } #.. end class in class_names_no_frs loop
  
  return(list(funding_list = funding_list,
              frs_fund = frs_fund))
}

inner_frs_fund2 <- function(i,
                            frs_fund,
                            params){
  # DANGER, TEMPORARY: not passing variables. will modify them and return  
  
  # <open code frs calculations>
  #AVA legacy development (step 1: calculate FRS's AVA)
  frs_fund$exp_inv_earnings_ava_legacy[i] <- frs_fund$ava_legacy[i-1] * params$dr_current_ + 
    frs_fund$net_cf_legacy[i] * params$dr_current_ / 2
  
  frs_fund$exp_ava_legacy[i] <- frs_fund$ava_legacy[i-1] + 
    frs_fund$net_cf_legacy[i] + 
    frs_fund$exp_inv_earnings_ava_legacy[i]
  
  # djb: Caution: hard-coded numbers here and further below ----
  frs_fund$ava_legacy[i] <- max(min(frs_fund$exp_ava_legacy[i] + (frs_fund$mva_legacy[i] - frs_fund$exp_ava_legacy[i]) * 0.2,
                                    frs_fund$mva_legacy[i] * 1.2), frs_fund$mva_legacy[i] * 0.8)
  
  frs_fund$alloc_inv_earnings_ava_legacy[i] <- frs_fund$ava_legacy[i] - 
    frs_fund$ava_legacy[i-1] - 
    frs_fund$net_cf_legacy[i]
  
  frs_fund$ava_base_legacy[i] <- frs_fund$ava_legacy[i-1] + frs_fund$net_cf_legacy[i]/2
  
  #AVA new development (step 1: calculate FRS's AVA)
  frs_fund$exp_inv_earnings_ava_new[i] <- frs_fund$ava_new[i-1] * params$dr_new_ +
    frs_fund$net_cf_new[i] * params$dr_new_ / 2
  frs_fund$exp_ava_new[i] <- frs_fund$ava_new[i-1] + frs_fund$net_cf_new[i] + frs_fund$exp_inv_earnings_ava_new[i]
  frs_fund$ava_new[i] <- max(min(frs_fund$exp_ava_new[i] + (frs_fund$mva_new[i] - frs_fund$exp_ava_new[i]) * 0.2,
                                 frs_fund$mva_new[i] * 1.2), 
                             frs_fund$mva_new[i] * 0.8)
  frs_fund$alloc_inv_earnings_ava_new[i] <- frs_fund$ava_new[i] - frs_fund$ava_new[i-1] - frs_fund$net_cf_new[i]
  frs_fund$ava_base_new[i] <- frs_fund$ava_new[i-1] + frs_fund$net_cf_new[i] / 2
  
  return(frs_fund)
}

inner_loop3_ava_development <- function(i,
                                        funding_list,
                                        frs_fund,
                                        params){
  # DANGER, TEMPORARY: not passing variables. will modify them and return
  
  for (class in params$class_names_no_frs_) { 
    #Do the assignment below to declutter the code
    class_fund <- funding_list[[class]]
    
    # AVA legacy development (step 2: calculate class's unadjusted AVA)
    class_fund$alloc_inv_earnings_ava_legacy[i] <- frs_fund$alloc_inv_earnings_ava_legacy[i] *
      class_fund$ava_base_legacy[i] / frs_fund$ava_base_legacy[i]
    
    class_fund$unadj_ava_legacy[i] <- class_fund$ava_legacy[i-1] + 
      class_fund$net_cf_legacy[i] + 
      class_fund$alloc_inv_earnings_ava_legacy[i]
    
    # AVA new development (step 2: calculate class's unadjusted AVA)
    class_fund$alloc_inv_earnings_ava_new[i] <- if_else(frs_fund$ava_base_new[i] == 0, 0, frs_fund$alloc_inv_earnings_ava_new[i] * class_fund$ava_base_new[i]/frs_fund$ava_base_new[i])
    class_fund$unadj_ava_new[i] <- class_fund$ava_new[i-1] + 
      class_fund$net_cf_new[i] + 
      class_fund$alloc_inv_earnings_ava_new[i]
    
    # Assign the class outputs back to the funding_list
    funding_list[[class]] <- class_fund
  }
  
  return(funding_list)
} 


inner_drop2_asset_reallocation <- function(i,
                                           funding_list,
                                           frs_fund) {
  # DANGER, TEMPORARY: not passing variables. will modify them and return  
  
  #DROP asset reallocation
  drop_fund <- funding_list$drop
  
  drop_fund$net_reallocation_legacy[i] <- drop_fund$unadj_ava_legacy[i] - drop_fund$aal_legacy[i] * frs_fund$ava_legacy[i] / frs_fund$aal_legacy[i]
  drop_fund$ava_legacy[i] <- drop_fund$unadj_ava_legacy[i] - drop_fund$net_reallocation_legacy[i]
  
  drop_fund$net_reallocation_new[i] <- if_else(frs_fund$aal_new[i] == 0, 0, drop_fund$unadj_ava_new[i] - drop_fund$aal_new[i] * frs_fund$ava_new[i] / frs_fund$aal_new[i])
  drop_fund$ava_new[i] <- drop_fund$unadj_ava_new[i] - drop_fund$net_reallocation_new[i]
  
  #Assign the DROP's updated numbers back to the funding_list
  funding_list$drop <- drop_fund
  
  return(funding_list)
}


inner_loop4_ava <- function(i,
                            funding_list,
                            frs_fund,
                            params){
  # DANGER, TEMPORARY: not passing variables. will modify them and return
  
  for (class in params$class_names_no_drop_frs_) {
    #Do the assignment below to declutter the code
    class_fund <- funding_list[[class]]
    
    #AVA legacy development (step 3: calculate class's adjusted AVA)
    class_drop_prop_legacy <- class_fund$aal_legacy[i] / (frs_fund$aal_legacy[i] - funding_list$drop$aal_legacy[i])
    class_fund$net_reallocation_legacy[i] <- class_drop_prop_legacy * funding_list$drop$net_reallocation_legacy[i]
    class_fund$ava_legacy[i] <- class_fund$unadj_ava_legacy[i] + class_fund$net_reallocation_legacy[i]
    
    #AVA new development (step 3: calculate class's adjusted AVA)
    class_drop_prop_new <- if_else((frs_fund$aal_new[i] - funding_list$drop$aal_new[i]) == 0, 0, class_fund$aal_new[i] / (frs_fund$aal_new[i] - funding_list$drop$aal_new[i]))
    class_fund$net_reallocation_new[i] <- class_drop_prop_new * funding_list$drop$net_reallocation_new[i]
    class_fund$ava_new[i] <- class_fund$unadj_ava_new[i] + class_fund$net_reallocation_new[i]
    
    #Assign the class outputs back to the funding_list
    funding_list[[class]] <- class_fund
  }
  
  return(funding_list)
}


inner_loop5_all_in_cost <- function(i,
                                    funding_list,
                                    frs_fund,
                                    params
){
  # DANGER, TEMPORARY: not passing variables. will modify them and return
  
  for (class in params$class_names_no_frs_) {
    #Do the assignment below to declutter the code
    class_fund <- funding_list[[class]]
    
    class_fund$total_ava[i] <- class_fund$ava_legacy[i] + class_fund$ava_new[i]
    
    frs_fund$total_ava[i] <- frs_fund$total_ava[i] + class_fund$total_ava[i]
    
    class_fund$ual_ava_legacy[i] <- class_fund$aal_legacy[i] - class_fund$ava_legacy[i]
    class_fund$ual_ava_new[i] <- class_fund$aal_new[i] - class_fund$ava_new[i]
    class_fund$total_ual_ava[i] <- class_fund$ual_ava_legacy[i] + class_fund$ual_ava_new[i]
    
    frs_fund$ual_ava_legacy[i] <- frs_fund$ual_ava_legacy[i] + class_fund$ual_ava_legacy[i]
    frs_fund$ual_ava_new[i] <- frs_fund$ual_ava_new[i] + class_fund$ual_ava_new[i]
    frs_fund$total_ual_ava[i] <- frs_fund$total_ual_ava[i] + class_fund$total_ual_ava[i]
    
    class_fund$ual_mva_legacy[i] <- class_fund$aal_legacy[i] - class_fund$mva_legacy[i]
    class_fund$ual_mva_new[i] <- class_fund$aal_new[i] - class_fund$mva_new[i]
    class_fund$total_ual_mva[i] <- class_fund$ual_mva_legacy[i] + class_fund$ual_mva_new[i]
    
    frs_fund$ual_mva_legacy[i] <- frs_fund$ual_mva_legacy[i] + class_fund$ual_mva_legacy[i]
    frs_fund$ual_mva_new[i] <- frs_fund$ual_mva_new[i] + class_fund$ual_mva_new[i]
    frs_fund$total_ual_mva[i] <- frs_fund$total_ual_mva[i] + class_fund$total_ual_mva[i]
    
    class_fund$fr_mva[i] <- class_fund$total_mva[i] / class_fund$total_aal[i]
    class_fund$fr_ava[i] <- class_fund$total_ava[i] / class_fund$total_aal[i]
    
    frs_fund$fr_mva[i] <- frs_fund$total_mva[i] / frs_fund$total_aal[i]
    frs_fund$fr_ava[i] <- frs_fund$total_ava[i] / frs_fund$total_aal[i]
    
    #Contribution analysis
    class_fund$total_er_cont[i] <- class_fund$total_er_db_cont[i] + class_fund$total_er_dc_cont[i] + class_fund$total_solv_cont[i]
    frs_fund$total_er_cont[i] <- frs_fund$total_er_cont[i] + class_fund$total_er_cont[i]
    
    class_fund$total_er_cont_rate[i] <- class_fund$total_er_cont[i] / class_fund$total_payroll[i]
    frs_fund$total_er_cont_rate[i] <- frs_fund$total_er_cont[i] / frs_fund$total_payroll[i]
    
    #All-in-cost analysis
    class_fund$total_er_cont_real[i] <- class_fund$total_er_cont[i] / (1 + params$inflation_)^(class_fund$year[i] - params$start_year_)
    frs_fund$total_er_cont_real[i] <- frs_fund$total_er_cont_real[i] + class_fund$total_er_cont_real[i]
    
    if (i == 2) {
      class_fund$cum_er_cont_real[i] <- class_fund$total_er_cont_real[i]
    } else {
      class_fund$cum_er_cont_real[i] <- class_fund$cum_er_cont_real[i - 1] + class_fund$total_er_cont_real[i]  
    } # end if else
    
    frs_fund$cum_er_cont_real[i] <- frs_fund$cum_er_cont_real[i] + class_fund$cum_er_cont_real[i]
    
    class_fund$total_ual_mva_real[i] <- class_fund$total_ual_mva[i] / (1 + params$inflation_)^(class_fund$year[i] - params$start_year_)
    frs_fund$total_ual_mva_real[i] <- frs_fund$total_ual_mva_real[i] + class_fund$total_ual_mva_real[i]
    
    class_fund$all_in_cost_real[i] <- class_fund$cum_er_cont_real[i] + class_fund$total_ual_mva_real[i]
    frs_fund$all_in_cost_real[i] <- frs_fund$all_in_cost_real[i] + class_fund$all_in_cost_real[i]
    
    
    #Assign the class outputs back to the funding_list
    funding_list[[class]] <- class_fund
  } #.. end class in class_names_no_frs loop
  
  return(list(funding_list = funding_list,
              frs_fund = frs_fund))
}


inner_loop6_amortization <- function(i,
                                     funding_list,
                                     current_hire_debt_layer_list,
                                     future_hire_debt_layer_list,
                                     current_hire_amo_period_list,
                                     future_hire_amo_period_list,
                                     current_hire_amo_payment_list,
                                     future_hire_amo_payment_list,
                                     amo_pay_growth,
                                     params){
  # DANGER, TEMPORARY: not passing variables. will modify them and return
  
  ####Amortization calculations
  for (class in params$class_names_no_frs_) {
    #Do the assignment below to declutter the code
    class_fund <- funding_list[[class]] # djb: here, class_fund is not modified
    
    current_hire_debt_layer_table <- current_hire_debt_layer_list[[class]]
    future_hire_debt_layer_table <- future_hire_debt_layer_list[[class]]
    
    current_hire_amo_period_table <- current_hire_amo_period_list[[class]]
    future_hire_amo_period_table <- future_hire_amo_period_list[[class]]
    
    current_hire_amo_pay_table <- current_hire_amo_payment_list[[class]]
    future_hire_amo_pay_table <- future_hire_amo_payment_list[[class]]
    
    #Amortization for legacy hires
    current_hire_debt_layer_table[i, 2:ncol(current_hire_debt_layer_table)] <- 
      current_hire_debt_layer_table[i-1, 1:(ncol(current_hire_debt_layer_table)-1)] * (1 + params$dr_current_) - 
      current_hire_amo_pay_table[i-1, 1:ncol(current_hire_amo_pay_table)] * (1 + params$dr_current_)^0.5
    
    current_hire_debt_layer_table[i, 1] <- class_fund$ual_ava_legacy[i] - sum(current_hire_debt_layer_table[i, 2:ncol(current_hire_debt_layer_table)])
    
    current_hire_amo_pay_table[i, 1:ncol(current_hire_amo_pay_table)] <- get_pmt(r = params$dr_current_,
                                                                                 g = amo_pay_growth,
                                                                                 nper = current_hire_amo_period_table[i, 1:ncol(current_hire_amo_period_table)],
                                                                                 pv = current_hire_debt_layer_table[i, 1:(ncol(current_hire_debt_layer_table)-1)],
                                                                                 t = 0.5)
    
    #Amortization for future hires
    future_hire_debt_layer_table[i, 2:ncol(future_hire_debt_layer_table)] <- 
      future_hire_debt_layer_table[i-1, 1:(ncol(future_hire_debt_layer_table)-1)] * (1 + params$dr_new_) -
      future_hire_amo_pay_table[i-1, 1:ncol(future_hire_amo_pay_table)] * (1 + params$dr_new_)^0.5
    
    future_hire_debt_layer_table[i, 1] <- class_fund$ual_ava_new[i] - sum(future_hire_debt_layer_table[i, 2:ncol(future_hire_debt_layer_table)])
    
    future_hire_amo_pay_table[i, 1:ncol(future_hire_amo_pay_table)] <- get_pmt(r = params$dr_new_,
                                                                               g = amo_pay_growth,
                                                                               nper = future_hire_amo_period_table[i, 1:ncol(future_hire_amo_period_table)],
                                                                               pv = future_hire_debt_layer_table[i, 1:(ncol(future_hire_debt_layer_table)-1)],
                                                                               t = 0.5)
    
    #Assign the amortization outputs back to respective tables
    current_hire_debt_layer_list[[class]] <- current_hire_debt_layer_table
    future_hire_debt_layer_list[[class]] <- future_hire_debt_layer_table
    
    current_hire_amo_payment_list[[class]] <- current_hire_amo_pay_table
    future_hire_amo_payment_list[[class]] <- future_hire_amo_pay_table
  }
  
  return(list(
    current_hire_debt_layer_list = current_hire_debt_layer_list,
    future_hire_debt_layer_list = future_hire_debt_layer_list,
    current_hire_amo_payment_list = current_hire_amo_payment_list,
    future_hire_amo_payment_list = future_hire_amo_payment_list
  ))
}



main_loop <- function(funding_list,
                      liability_list,
                      current_hire_amo_payment_list,
                      future_hire_amo_payment_list,
                      current_hire_amo_period_list,
                      future_hire_amo_period_list,
                      current_hire_debt_layer_list,
                      future_hire_debt_layer_list,
                      amo_pay_growth,
                      params){
  
  # return funding_list
  
  # djb - why can't we do one loop without frs?? (or is it updated within) and adjust for drop?
  # could we, for example?:
  #   - stack the classes
  #   - loop through the years to calc class values (because we need lags)
  #   - collapse to get frs non-drop totals
  #   - loop through the years to calc drop values, relying as needed on regular and on frs-nondrop values
  #   - combine to get frs+drop totals
  
  # djb: to help figure out how to reorganize this:
  # djb: when if ever do the class values rely on the frs values?
  # djb: when does drop rely on frs or on a class value?
  
  # djb: steps seem to be:
  #   - calc non-drop class values
  #   - calc FRS working totals because we need some FRS values for drop
  #   - calc drop values - here, e.g., we need FRS NC rate for drop NC rate
  
  # where does payroll_db_legacy_ratio come from?  
  
  # Key strategy: Loop through each year, then each class. The class loop should
  # exclude FRS, and may exclude DROP depending on the calculations
  
  for (i in 2:nrow(funding_list[[1]])) { # loop 2nd year to last year
    frs_fund <- funding_list$frs
    
    # CAUTION: I modify calling-environment variables in the functions below
    
    result <- inner_loop1_payroll_benefits(i,
                                           funding_list,
                                           liability_list,
                                           params) #.. no_drop_frs loop: payroll, benefits, refunds, normal cost, AAL
    
    
    # list2env(result, envir = parent.frame())  # works but not as easy to understand
    funding_list <- result$funding_list
    #frs_fund <- result$frs_fund
    
    classes_to_sum <- params$class_names_no_drop_frs_
    frs_fund <- summarize_frs_payroll_benefits(i, funding_list, classes_to_sum, frs_fund)
    
    
    funding_list <- inner_drop1_funding(i,
                                        funding_list,
                                        frs_fund,
                                        params) #.. open code: DROP payroll, benefits, NC, AL -- "makeshift"
    
    frs_fund <- inner_frs_fund1(i,
                                frs_fund,
                                funding_list$drop) # FRS totals: update with DROP -- payroll, benefits, refunds, NC, AL
    
    result <- inner_loop2_funding(i,
                                  funding_list,
                                  frs_fund,
                                  current_hire_amo_payment_list,
                                  future_hire_amo_payment_list,
                                  params$return_scenarios,
                                  params$return_scen_index,
                                  params) # NC, EEC, ERC-DB, admin expense
    funding_list <- result$funding_list
    frs_fund <- result$frs_fund    
    
    frs_fund <- inner_frs_fund2(i,
                                frs_fund,
                                params) 
    
    funding_list <- inner_loop3_ava_development(i,
                                                funding_list,
                                                frs_fund,
                                                params)
    
    funding_list <- inner_drop2_asset_reallocation(i,
                                                   funding_list,
                                                   frs_fund) #.. open code: DROP assets reallocation
    
    funding_list <- inner_loop4_ava(i,
                                    funding_list,
                                    frs_fund,
                                    params)
    
    result <- inner_loop5_all_in_cost(i,
                                      funding_list,
                                      frs_fund,
                                      params) # AVA UAL FR projections all-in cost
    funding_list <- result$funding_list
    frs_fund <- result$frs_fund    
    
    result <- inner_loop6_amortization(i,
                                       funding_list,
                                       current_hire_debt_layer_list,
                                       future_hire_debt_layer_list,
                                       current_hire_amo_period_list,
                                       future_hire_amo_period_list,
                                       current_hire_amo_payment_list,
                                       future_hire_amo_payment_list,
                                       amo_pay_growth,
                                       params)
    
    # djb maybe these next 4 lists need to be returned as a second list??
    current_hire_debt_layer_list <- result$current_hire_debt_layer_list
    future_hire_debt_layer_list <- result$future_hire_debt_layer_list
    current_hire_amo_payment_list <- result$current_hire_amo_payment_list
    future_hire_amo_payment_list <- result$future_hire_amo_payment_list    
    
    #Assign the FRS's updated numbers back to the funding_list
    funding_list$frs <- frs_fund
    
  } #.. end year loop
  return(funding_list)
} # end function main_loop ----