for (class in params$class_names_no_drop_frs_) {
  
  fund_data <- funding_list[[class]]
  liab_data <- liability_list[[class]]
  
  #payroll calibration
  fund_data$payroll_db_legacy_ratio <- lag(liab_data$payroll_db_legacy_est / liab_data$total_payroll_est) #use lag to align with the funding mechanism
  fund_data$payroll_db_new_ratio <- lag(liab_data$payroll_db_new_est / liab_data$total_payroll_est)
  fund_data$payroll_dc_legacy_ratio <- lag(liab_data$payroll_dc_legacy_est / liab_data$total_payroll_est)
  fund_data$payroll_dc_new_ratio <- lag(liab_data$payroll_dc_new_est / liab_data$total_payroll_est)
  
  #normal cost calibration/projection
  nc_cal <- params[[paste0(class, "_nc_cal_")]]
  fund_data$nc_rate_db_legacy <- lag(liab_data$nc_rate_db_legacy_est * nc_cal)
  fund_data$nc_rate_db_new <- lag(liab_data$nc_rate_db_new_est * nc_cal)
  
  #accrued liability calibration
  fund_data$aal_legacy[1] <- liab_data$aal_legacy_est[1]
  fund_data$total_aal[1] <- liab_data$total_aal_est[1]
  fund_data$ual_ava_legacy[1] <- fund_data$aal_legacy[1] - fund_data$ava_legacy[1]
  fund_data$total_ual_ava[1] <- fund_data$total_aal[1] - fund_data$total_ava[1]
  
  funding_list[[class]] <- fund_data
} # end model calibration loop

inner_loop1_payroll_benefits <- function(i,
                                         funding_list,
                                         liability_list,
                                         frs_fund,
                                         params){
  
  for (class in params$class_names_no_drop_frs_) {
    # djb: it looks like no class values rely on frs values in this loop, so we could move frs entirely out of the loop
    
    #Do the assignment below to declutter the code
    class_fund <- funding_list[[class]]
    class_liab <- liability_list[[class]]
    
    #Payroll projection
    class_fund$total_payroll[i] <- class_fund$total_payroll[i-1] * (1 + params$payroll_growth_) # lagged value
    
    class_fund$payroll_db_legacy[i] <- class_fund$total_payroll[i] * class_fund$payroll_db_legacy_ratio[i]
    class_fund$payroll_db_new[i] <- class_fund$total_payroll[i] * class_fund$payroll_db_new_ratio[i]
    class_fund$payroll_dc_legacy[i] <- class_fund$total_payroll[i] * class_fund$payroll_dc_legacy_ratio[i]
    class_fund$payroll_dc_new[i] <- class_fund$total_payroll[i] * class_fund$payroll_dc_new_ratio[i]
    
    # djb update frs values with class totals no lags -- could be consolidated
    frs_fund$total_payroll[i] <- frs_fund$total_payroll[i] + class_fund$total_payroll[i]
    frs_fund$payroll_db_legacy[i] <- frs_fund$payroll_db_legacy[i] + class_fund$payroll_db_legacy[i]
    frs_fund$payroll_db_new[i] <- frs_fund$payroll_db_new[i] + class_fund$payroll_db_new[i]
    frs_fund$payroll_dc_legacy[i] <- frs_fund$payroll_dc_legacy[i] + class_fund$payroll_dc_legacy[i]
    frs_fund$payroll_dc_new[i] <- frs_fund$payroll_dc_new[i] + class_fund$payroll_dc_new[i]
    
    #Benefit payments and refunds projection
    class_fund$ben_payment_legacy[i] <- class_liab$retire_ben_db_legacy_est[i] + 
      class_liab$retire_ben_current_est[i] + 
      class_liab$retire_ben_term_est[i]
    class_fund$refund_legacy[i] <- class_liab$refund_db_legacy_est[i]
    class_fund$ben_payment_new[i] <- class_liab$retire_ben_db_new_est[i]
    class_fund$refund_new[i] <- class_liab$refund_db_new_est[i]
    
    class_fund$total_ben_payment[i] <- class_fund$ben_payment_legacy[i] + class_fund$ben_payment_new[i]
    class_fund$total_refund[i] <- class_fund$refund_legacy[i] + class_fund$refund_new[i]
    
    # djb update frs values with class totals, no lags -- could be consolidated
    frs_fund$ben_payment_legacy[i] <- frs_fund$ben_payment_legacy[i] + class_fund$ben_payment_legacy[i]
    frs_fund$refund_legacy[i] <- frs_fund$refund_legacy[i] + class_fund$refund_legacy[i]
    frs_fund$ben_payment_new[i] <- frs_fund$ben_payment_new[i] + class_fund$ben_payment_new[i]
    frs_fund$refund_new[i] <- frs_fund$refund_new[i] + class_fund$refund_new[i]
    
    frs_fund$total_ben_payment[i] <- frs_fund$total_ben_payment[i] + class_fund$total_ben_payment[i]
    frs_fund$total_refund[i] <- frs_fund$total_refund[i] + class_fund$total_refund[i]
    
    #Normal cost projection
    class_fund$nc_legacy[i] <- class_fund$nc_rate_db_legacy[i] * class_fund$payroll_db_legacy[i]
    class_fund$nc_new[i] <- class_fund$nc_rate_db_new[i] * class_fund$payroll_db_new[i]
    class_fund$total_nc_rate[i] <- (class_fund$nc_legacy[i] + class_fund$nc_new[i]) / 
      (class_fund$payroll_db_legacy[i] + class_fund$payroll_db_new[i])
    
    # djb update frs values with class totals, no lags -- could be consolidated
    frs_fund$nc_legacy[i] <- frs_fund$nc_legacy[i] + class_fund$nc_legacy[i]
    frs_fund$nc_new[i] <- frs_fund$nc_new[i] + class_fund$nc_new[i]
    frs_fund$total_nc_rate[i] <- (frs_fund$nc_legacy[i] + frs_fund$nc_new[i]) / 
      (frs_fund$payroll_db_legacy[i] + frs_fund$payroll_db_new[i])
    
    #Accrued liability projection
    class_fund$liability_gain_loss_legacy[i] <- class_liab$liability_gain_loss_legacy_est[i]
    class_fund$liability_gain_loss_new[i] <- class_liab$liability_gain_loss_new_est[i]
    class_fund$total_liability_gain_loss[i] <- class_liab$total_liability_gain_loss_est[i]
    
    # djb: lagged value here
    class_fund$aal_legacy[i] <- class_fund$aal_legacy[i-1] * (1 + params$dr_current_) +
      (class_fund$nc_legacy[i] - class_fund$ben_payment_legacy[i] - class_fund$refund_legacy[i]) *
      (1 + params$dr_current_)^0.5 + 
      class_fund$liability_gain_loss_legacy[i]
    
    # djb: lagged value here
    class_fund$aal_new[i] <- class_fund$aal_new[i-1] * (1 + params$dr_new_) + 
      (class_fund$nc_new[i] - class_fund$ben_payment_new[i] - class_fund$refund_new[i]) *
      (1 + params$dr_new_)^0.5 + 
      class_fund$liability_gain_loss_new[i]
    
    class_fund$total_aal[i] <- class_fund$aal_legacy[i] + class_fund$aal_new[i]
    
    # djb: FRS totals in the loop: update with class info no lags, could be consolidated
    #frs_fund$lia_gain_loss_legacy[i] <- frs_fund$lia_gain_loss_legacy[i] + class_fund$liability_gain_loss_legacy[i]
    frs_fund$liability_gain_loss_legacy[i] <- frs_fund$liability_gain_loss_legacy[i] + class_fund$liability_gain_loss_legacy[i]
    #above was frs_fund$lia_gain_loss_legacy, updated to frs_fund$liability_gain_loss_legacy (Gang)
    
    frs_fund$lia_gain_loss_new[i] <- frs_fund$lia_gain_loss_new[i] + class_fund$liability_gain_loss_new[i]
    frs_fund$liability_gain_loss_new[i] <- frs_fund$liability_gain_loss_new[i] + class_fund$liability_gain_loss_new[i]
    #above was frs_fund$lia_gain_loss_legacy, updated to frs_fund$liability_gain_loss_legacy (Gang)
    
    #frs_fund$total_lia_gain_loss[i] <- frs_fund$total_lia_gain_loss[i] + class_fund$total_liability_gain_loss[i]
    frs_fund$total_liability_gain_loss[i] <- frs_fund$total_liability_gain_loss[i] + class_fund$total_liability_gain_loss[i]
    #above was total_lia_gain_loss, updated to total_liability_gain_loss (Gang)
    
    frs_fund$aal_legacy[i] <- frs_fund$aal_legacy[i] + class_fund$aal_legacy[i]
    frs_fund$aal_new[i] <- frs_fund$aal_new[i] + class_fund$aal_new[i]
    frs_fund$total_aal[i] <- frs_fund$total_aal[i] + class_fund$total_aal[i]
    
    #Assign the class outputs back to the funding_list
    funding_list[[class]] <- class_fund
    
  } #.. end class in class_names_no_drop_frs loop
  return(list(funding_list = funding_list,
              frs_fund = frs_fund))
}


inner_loop1_payroll_benefits_original <- function(i,
                                         funding_list,
                                         liability_list,
                                         frs_fund,
                                         params){
  
  for (class in params$class_names_no_drop_frs_) {
    # djb: it looks like no class values rely on frs values in this loop, so we could move frs entirely out of the loop
    
    #Do the assignment below to declutter the code
    class_fund <- funding_list[[class]]
    class_liab <- liability_list[[class]]
    
    #Payroll projection
    class_fund$total_payroll[i] <- class_fund$total_payroll[i-1] * (1 + params$payroll_growth_) # lagged value
    
    class_fund$payroll_db_legacy[i] <- class_fund$total_payroll[i] * class_fund$payroll_db_legacy_ratio[i]
    class_fund$payroll_db_new[i] <- class_fund$total_payroll[i] * class_fund$payroll_db_new_ratio[i]
    class_fund$payroll_dc_legacy[i] <- class_fund$total_payroll[i] * class_fund$payroll_dc_legacy_ratio[i]
    class_fund$payroll_dc_new[i] <- class_fund$total_payroll[i] * class_fund$payroll_dc_new_ratio[i]
    
    # djb update frs values with class totals no lags -- could be consolidated
    frs_fund$total_payroll[i] <- frs_fund$total_payroll[i] + class_fund$total_payroll[i]
    frs_fund$payroll_db_legacy[i] <- frs_fund$payroll_db_legacy[i] + class_fund$payroll_db_legacy[i]
    frs_fund$payroll_db_new[i] <- frs_fund$payroll_db_new[i] + class_fund$payroll_db_new[i]
    frs_fund$payroll_dc_legacy[i] <- frs_fund$payroll_dc_legacy[i] + class_fund$payroll_dc_legacy[i]
    frs_fund$payroll_dc_new[i] <- frs_fund$payroll_dc_new[i] + class_fund$payroll_dc_new[i]
    
    #Benefit payments and refunds projection
    class_fund$ben_payment_legacy[i] <- class_liab$retire_ben_db_legacy_est[i] + 
      class_liab$retire_ben_current_est[i] + 
      class_liab$retire_ben_term_est[i]
    class_fund$refund_legacy[i] <- class_liab$refund_db_legacy_est[i]
    class_fund$ben_payment_new[i] <- class_liab$retire_ben_db_new_est[i]
    class_fund$refund_new[i] <- class_liab$refund_db_new_est[i]
    
    class_fund$total_ben_payment[i] <- class_fund$ben_payment_legacy[i] + class_fund$ben_payment_new[i]
    class_fund$total_refund[i] <- class_fund$refund_legacy[i] + class_fund$refund_new[i]
    
    # djb update frs values with class totals, no lags -- could be consolidated
    frs_fund$ben_payment_legacy[i] <- frs_fund$ben_payment_legacy[i] + class_fund$ben_payment_legacy[i]
    frs_fund$refund_legacy[i] <- frs_fund$refund_legacy[i] + class_fund$refund_legacy[i]
    frs_fund$ben_payment_new[i] <- frs_fund$ben_payment_new[i] + class_fund$ben_payment_new[i]
    frs_fund$refund_new[i] <- frs_fund$refund_new[i] + class_fund$refund_new[i]
    
    frs_fund$total_ben_payment[i] <- frs_fund$total_ben_payment[i] + class_fund$total_ben_payment[i]
    frs_fund$total_refund[i] <- frs_fund$total_refund[i] + class_fund$total_refund[i]
    
    #Normal cost projection
    class_fund$nc_legacy[i] <- class_fund$nc_rate_db_legacy[i] * class_fund$payroll_db_legacy[i]
    class_fund$nc_new[i] <- class_fund$nc_rate_db_new[i] * class_fund$payroll_db_new[i]
    class_fund$total_nc_rate[i] <- (class_fund$nc_legacy[i] + class_fund$nc_new[i]) / 
      (class_fund$payroll_db_legacy[i] + class_fund$payroll_db_new[i])
    
    # djb update frs values with class totals, no lags -- could be consolidated
    frs_fund$nc_legacy[i] <- frs_fund$nc_legacy[i] + class_fund$nc_legacy[i]
    frs_fund$nc_new[i] <- frs_fund$nc_new[i] + class_fund$nc_new[i]
    frs_fund$total_nc_rate[i] <- (frs_fund$nc_legacy[i] + frs_fund$nc_new[i]) / 
      (frs_fund$payroll_db_legacy[i] + frs_fund$payroll_db_new[i])
    
    #Accrued liability projection
    class_fund$liability_gain_loss_legacy[i] <- class_liab$liability_gain_loss_legacy_est[i]
    class_fund$liability_gain_loss_new[i] <- class_liab$liability_gain_loss_new_est[i]
    class_fund$total_liability_gain_loss[i] <- class_liab$total_liability_gain_loss_est[i]
    
    # djb: lagged value here
    class_fund$aal_legacy[i] <- class_fund$aal_legacy[i-1] * (1 + params$dr_current_) +
      (class_fund$nc_legacy[i] - class_fund$ben_payment_legacy[i] - class_fund$refund_legacy[i]) *
      (1 + params$dr_current_)^0.5 + 
      class_fund$liability_gain_loss_legacy[i]
    
    # djb: lagged value here
    class_fund$aal_new[i] <- class_fund$aal_new[i-1] * (1 + params$dr_new_) + 
      (class_fund$nc_new[i] - class_fund$ben_payment_new[i] - class_fund$refund_new[i]) *
      (1 + params$dr_new_)^0.5 + 
      class_fund$liability_gain_loss_new[i]
    
    class_fund$total_aal[i] <- class_fund$aal_legacy[i] + class_fund$aal_new[i]
    
    # djb: FRS totals in the loop: update with class info no lags, could be consolidated
    frs_fund$lia_gain_loss_legacy[i] <- frs_fund$lia_gain_loss_legacy[i] + class_fund$liability_gain_loss_legacy[i]
    frs_fund$lia_gain_loss_new[i] <- frs_fund$lia_gain_loss_new[i] + class_fund$liability_gain_loss_new[i]
    frs_fund$total_lia_gain_loss[i] <- frs_fund$total_lia_gain_loss[i] + class_fund$total_liability_gain_loss[i]
    
    frs_fund$aal_legacy[i] <- frs_fund$aal_legacy[i] + class_fund$aal_legacy[i]
    frs_fund$aal_new[i] <- frs_fund$aal_new[i] + class_fund$aal_new[i]
    frs_fund$total_aal[i] <- frs_fund$total_aal[i] + class_fund$total_aal[i]
    
    #Assign the class outputs back to the funding_list
    funding_list[[class]] <- class_fund
    
  } #.. end class in class_names_no_drop_frs loop
  return(list(funding_list = funding_list,
              frs_fund = frs_fund))
}


inner_loop1_payroll_benefits_s <- function(i,
                                         funding_list,
                                         liability_list,
                                         frs_fund,
                                         params){
  
  for (class in params$class_names_no_drop_frs_) {

    class <- "regular"
    class_fund <- funding_list[[class]]
    class_liab <- liability_list[[class]]
    
    time_index <- 2:30
    
    for (i in 2:nrow(funding_list[[1]])) {
      
    #Payroll projection
    #class_fund$total_payroll[i] <- class_fund$total_payroll[i-1] * (1 + params$payroll_growth_) # lagged value
    growth_factors <- cumprod(c(1, rep(1 + params$payroll_growth_, model_period - 2)))
    initial_payroll <- class_fund$total_payroll[1]
    class_fund$total_payroll[time_index] <- initial_payroll * growth_factors
    
    
    class_fund$payroll_db_legacy[i] <- class_fund$total_payroll[i] * class_fund$payroll_db_legacy_ratio[i]
    class_fund$payroll_db_new[i] <- class_fund$total_payroll[i] * class_fund$payroll_db_new_ratio[i]
    class_fund$payroll_dc_legacy[i] <- class_fund$total_payroll[i] * class_fund$payroll_dc_legacy_ratio[i]
    class_fund$payroll_dc_new[i] <- class_fund$total_payroll[i] * class_fund$payroll_dc_new_ratio[i]
    }
    # djb update frs values with class totals no lags -- could be consolidated
    frs_fund$total_payroll[i] <- frs_fund$total_payroll[i] + class_fund$total_payroll[i]
    frs_fund$payroll_db_legacy[i] <- frs_fund$payroll_db_legacy[i] + class_fund$payroll_db_legacy[i]
    frs_fund$payroll_db_new[i] <- frs_fund$payroll_db_new[i] + class_fund$payroll_db_new[i]
    frs_fund$payroll_dc_legacy[i] <- frs_fund$payroll_dc_legacy[i] + class_fund$payroll_dc_legacy[i]
    frs_fund$payroll_dc_new[i] <- frs_fund$payroll_dc_new[i] + class_fund$payroll_dc_new[i]
    
    #Benefit payments and refunds projection
    class_fund$ben_payment_legacy[i] <- class_liab$retire_ben_db_legacy_est[i] + 
      class_liab$retire_ben_current_est[i] + 
      class_liab$retire_ben_term_est[i]
    class_fund$refund_legacy[i] <- class_liab$refund_db_legacy_est[i]
    class_fund$ben_payment_new[i] <- class_liab$retire_ben_db_new_est[i]
    class_fund$refund_new[i] <- class_liab$refund_db_new_est[i]
    
    class_fund$total_ben_payment[i] <- class_fund$ben_payment_legacy[i] + class_fund$ben_payment_new[i]
    class_fund$total_refund[i] <- class_fund$refund_legacy[i] + class_fund$refund_new[i]
    
    # djb update frs values with class totals, no lags -- could be consolidated
    frs_fund$ben_payment_legacy[i] <- frs_fund$ben_payment_legacy[i] + class_fund$ben_payment_legacy[i]
    frs_fund$refund_legacy[i] <- frs_fund$refund_legacy[i] + class_fund$refund_legacy[i]
    frs_fund$ben_payment_new[i] <- frs_fund$ben_payment_new[i] + class_fund$ben_payment_new[i]
    frs_fund$refund_new[i] <- frs_fund$refund_new[i] + class_fund$refund_new[i]
    
    frs_fund$total_ben_payment[i] <- frs_fund$total_ben_payment[i] + class_fund$total_ben_payment[i]
    frs_fund$total_refund[i] <- frs_fund$total_refund[i] + class_fund$total_refund[i]
    
    #Normal cost projection
    class_fund$nc_legacy[i] <- class_fund$nc_rate_db_legacy[i] * class_fund$payroll_db_legacy[i]
    class_fund$nc_new[i] <- class_fund$nc_rate_db_new[i] * class_fund$payroll_db_new[i]
    class_fund$total_nc_rate[i] <- (class_fund$nc_legacy[i] + class_fund$nc_new[i]) / 
      (class_fund$payroll_db_legacy[i] + class_fund$payroll_db_new[i])
    
    # djb update frs values with class totals, no lags -- could be consolidated
    frs_fund$nc_legacy[i] <- frs_fund$nc_legacy[i] + class_fund$nc_legacy[i]
    frs_fund$nc_new[i] <- frs_fund$nc_new[i] + class_fund$nc_new[i]
    frs_fund$total_nc_rate[i] <- (frs_fund$nc_legacy[i] + frs_fund$nc_new[i]) / 
      (frs_fund$payroll_db_legacy[i] + frs_fund$payroll_db_new[i])
    
    #Accrued liability projection
    class_fund$liability_gain_loss_legacy[i] <- class_liab$liability_gain_loss_legacy_est[i]
    class_fund$liability_gain_loss_new[i] <- class_liab$liability_gain_loss_new_est[i]
    class_fund$total_liability_gain_loss[i] <- class_liab$total_liability_gain_loss_est[i]
    
    # djb: lagged value here
    class_fund$aal_legacy[i] <- class_fund$aal_legacy[i-1] * (1 + params$dr_current_) +
      (class_fund$nc_legacy[i] - class_fund$ben_payment_legacy[i] - class_fund$refund_legacy[i]) *
      (1 + params$dr_current_)^0.5 + 
      class_fund$liability_gain_loss_legacy[i]
    
    # djb: lagged value here
    class_fund$aal_new[i] <- class_fund$aal_new[i-1] * (1 + params$dr_new_) + 
      (class_fund$nc_new[i] - class_fund$ben_payment_new[i] - class_fund$refund_new[i]) *
      (1 + params$dr_new_)^0.5 + 
      class_fund$liability_gain_loss_new[i]
    
    class_fund$total_aal[i] <- class_fund$aal_legacy[i] + class_fund$aal_new[i]
    
    # djb: FRS totals in the loop: update with class info no lags, could be consolidated
    #frs_fund$lia_gain_loss_legacy[i] <- frs_fund$lia_gain_loss_legacy[i] + class_fund$liability_gain_loss_legacy[i]
    frs_fund$liability_gain_loss_legacy[i] <- frs_fund$liability_gain_loss_legacy[i] + class_fund$liability_gain_loss_legacy[i]
    #above was frs_fund$lia_gain_loss_legacy, updated to frs_fund$liability_gain_loss_legacy (Gang)
    
    frs_fund$lia_gain_loss_new[i] <- frs_fund$lia_gain_loss_new[i] + class_fund$liability_gain_loss_new[i]
    frs_fund$liability_gain_loss_new[i] <- frs_fund$liability_gain_loss_new[i] + class_fund$liability_gain_loss_new[i]
    #above was frs_fund$lia_gain_loss_legacy, updated to frs_fund$liability_gain_loss_legacy (Gang)
    
    #frs_fund$total_lia_gain_loss[i] <- frs_fund$total_lia_gain_loss[i] + class_fund$total_liability_gain_loss[i]
    frs_fund$total_liability_gain_loss[i] <- frs_fund$total_liability_gain_loss[i] + class_fund$total_liability_gain_loss[i]
    #above was total_lia_gain_loss, updated to total_liability_gain_loss (Gang)
    
    frs_fund$aal_legacy[i] <- frs_fund$aal_legacy[i] + class_fund$aal_legacy[i]
    frs_fund$aal_new[i] <- frs_fund$aal_new[i] + class_fund$aal_new[i]
    frs_fund$total_aal[i] <- frs_fund$total_aal[i] + class_fund$total_aal[i]
    
    #Assign the class outputs back to the funding_list
    funding_list[[class]] <- class_fund
    
  } #.. end class in class_names_no_drop_frs loop
  return(list(funding_list = funding_list,
              frs_fund = frs_fund))
}


for (i in 2:nrow(funding_list[[1]])) {
result <- inner_loop1_payroll_benefits(i,
                                       funding_list,
                                       liability_list,
                                       frs_fund,
                                       params)
  }

for (i in 2:nrow(funding_list[[1]])) {
  result_use_original <- inner_loop1_payroll_benefits_original(i,
                                       funding_list,
                                       liability_list,
                                       frs_fund,
                                       params)
}

identical(result, result_use_original)
