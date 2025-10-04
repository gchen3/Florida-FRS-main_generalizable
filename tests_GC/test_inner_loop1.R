i <- 2
funding_list <- params$funding_list

classes <- params$class_names_no_drop_frs_
liab_all <- lm_env$get_liability_data_s(bm_env, wf_data_env, params)
liability_list <- map(
        classes,
        ~ liab_all %>% filter(class == .x) %>% select(-class)) %>% set_names(classes)
frs_fund <- funding_list$frs

class <- "regular"

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
} 
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
    frs_fund$liability_gain_loss_legacy[i] <- frs_fund$liability_gain_loss_legacy[i] + class_fund$liability_gain_loss_legacy[i]
    frs_fund$liability_gain_loss_new[i] <- frs_fund$liability_gain_loss_new[i] + class_fund$liability_gain_loss_new[i]
    frs_fund$total_liability_gain_loss[i] <- frs_fund$total_liability_gain_loss[i] + class_fund$total_liability_gain_loss[i]
    
    frs_fund$aal_legacy[i] <- frs_fund$aal_legacy[i] + class_fund$aal_legacy[i]
    frs_fund$aal_new[i] <- frs_fund$aal_new[i] + class_fund$aal_new[i]
    frs_fund$total_aal[i] <- frs_fund$total_aal[i] + class_fund$total_aal[i]
    
    #Assign the class outputs back to the funding_list
    funding_list[[class]] <- class_fund
    
  } #.. end class in class_names_no_drop_frs loop
  return(list(funding_list = funding_list,
              frs_fund = frs_fund))
}

inner_loop1_out <- inner_loop1_payroll_benefits(i,
                                                funding_list,
                                                liability_list,
                                                frs_fund,
                                                params)





# alternative functions with frs pulled out -------------------------------

i <- 2
funding_list <- params$funding_list

classes <- params$class_names_no_drop_frs_
liab_all <- lm_env$get_liability_data_s(bm_env, wf_data_env, params)
liability_list <- map(
  classes,
  ~ liab_all %>% filter(class == .x) %>% select(-class)) %>% set_names(classes)
frs_fund <- funding_list$frs

class <- "regular"

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
} 

inner_loop1_payroll_benefits_new <- function(i,
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


result_new <- inner_loop1_payroll_benefits_new (i,
                                       funding_list,
                                       liability_list,
                                       params) 

funding_list_new <- result_new$funding_list

classes_to_sum <- params$class_names_no_drop_frs_

frs_fund_new <- summarize_frs_payroll_benefits(i, funding_list_new, classes_to_sum, frs_fund)


# confirm results identical -----------------------------------------------

identical(inner_loop1_out$funding_list, funding_list_new)
identical(inner_loop1_out$frs_fund, frs_fund_new)
