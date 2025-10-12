source(here::here("tests_GC", "test_inner_loop1.R"))

# original function -------------------------------------------------------

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





# New function with pre-calcualted ratios ---------------------------------

build_drop_drivers <- function(funding_list, frs_fund) {
  reg <- funding_list$regular
  n   <- nrow(reg)
  
  out <- data.frame(
    db_legacy_share     = rep(NA_real_, n),
    db_new_share        = rep(NA_real_, n),
    g_ben               = rep(NA_real_, n),
    g_ref               = rep(NA_real_, n),
    ben_legacy_share    = rep(NA_real_, n),
    ref_legacy_share    = rep(NA_real_, n),
    nc_rate_db_legacy   = rep(NA_real_, n),
    nc_rate_db_new      = rep(NA_real_, n)
  )
  
  for (i in 2:n) {
    # payroll split (fold DC into DB for DROP)
    out$db_legacy_share[i]  <- reg$payroll_db_legacy_ratio[i] + reg$payroll_dc_legacy_ratio[i]
    out$db_new_share[i]     <- reg$payroll_db_new_ratio[i]    + reg$payroll_dc_new_ratio[i]
    
    # growth factors from i-1 -> i
    out$g_ben[i]            <- reg$total_ben_payment[i] / reg$total_ben_payment[i-1]
    out$g_ref[i]            <- reg$total_refund[i]      / reg$total_refund[i-1]
    
    # composition shares at i
    out$ben_legacy_share[i] <- reg$ben_payment_legacy[i] / reg$total_ben_payment[i]
    out$ref_legacy_share[i] <- reg$refund_legacy[i]      / reg$total_refund[i]
    
    # NC rates from FRS at i
    out$nc_rate_db_legacy[i] <- frs_fund$nc_legacy[i] / frs_fund$payroll_db_legacy[i]
    out$nc_rate_db_new[i] <- if (is.na(frs_fund$payroll_db_new[i])) NA_real_
                            else if (frs_fund$payroll_db_new[i] == 0) 0
                            else frs_fund$nc_new[i] / frs_fund$payroll_db_new[i]
  }
  
  out
}

inner_drop1_funding_cached <- function(i,
                                       funding_list,
                                       drop_drivers,
                                       params){
  
  drop_fund <- funding_list$drop
  d         <- drop_drivers[i, ]
  
  # Payroll (no DC in DROP)
  drop_fund$total_payroll[i]     <- drop_fund$total_payroll[i-1] * (1 + params$payroll_growth_)
  drop_fund$payroll_db_legacy[i] <- drop_fund$total_payroll[i] * d$db_legacy_share
  drop_fund$payroll_db_new[i]    <- drop_fund$total_payroll[i] * d$db_new_share
  
  # Benefits & refunds: grow totals, then split by shares
  drop_fund$total_ben_payment[i] <- drop_fund$total_ben_payment[i-1] * d$g_ben
  drop_fund$total_refund[i]      <- drop_fund$total_refund[i-1]      * d$g_ref
  
  drop_fund$ben_payment_legacy[i] <- drop_fund$total_ben_payment[i] * d$ben_legacy_share
  drop_fund$ben_payment_new[i]    <- drop_fund$total_ben_payment[i] * (1 - d$ben_legacy_share)
  
  drop_fund$refund_legacy[i]      <- drop_fund$total_refund[i] * d$ref_legacy_share
  drop_fund$refund_new[i]         <- drop_fund$total_refund[i] * (1 - d$ref_legacy_share)
  
  # NC (rates pulled from cached FRS rates)
  drop_fund$nc_rate_db_legacy[i] <- d$nc_rate_db_legacy
  drop_fund$nc_rate_db_new[i]    <- d$nc_rate_db_new
  drop_fund$nc_legacy[i]         <- drop_fund$nc_rate_db_legacy[i] * drop_fund$payroll_db_legacy[i]
  drop_fund$nc_new[i]            <- drop_fund$nc_rate_db_new[i]    * drop_fund$payroll_db_new[i]
  drop_fund$total_nc_rate[i]     <- (drop_fund$nc_legacy[i] + drop_fund$nc_new[i]) /
    (drop_fund$payroll_db_legacy[i] + drop_fund$payroll_db_new[i])
  
  # AAL roll-forward
  drop_fund$aal_legacy[i] <- drop_fund$aal_legacy[i-1] * (1 + params$dr_current_) +
    (drop_fund$nc_legacy[i] - drop_fund$ben_payment_legacy[i] - drop_fund$refund_legacy[i]) *
    (1 + params$dr_current_)^0.5 +
    drop_fund$liability_gain_loss_legacy[i]
  
  drop_fund$aal_new[i] <- drop_fund$aal_new[i-1] * (1 + params$dr_new_) +
    (drop_fund$nc_new[i] - drop_fund$ben_payment_new[i] - drop_fund$refund_new[i]) *
    (1 + params$dr_new_)^0.5 +
    drop_fund$liability_gain_loss_new[i]
  
  drop_fund$total_aal[i] <- drop_fund$aal_legacy[i] + drop_fund$aal_new[i]
  
  funding_list$drop <- drop_fund
  funding_list
}

# test the function -------------------------------------------------------
inner_loop1_out$funding_list
inner_loop1_out$frs_fund

results_original <- inner_drop1_funding(i = 2,
                                        funding_list = inner_loop1_out$funding_list,
                                        frs_fund = inner_loop1_out$frs_fund,
                                        params = params)

drop_drivers <- build_drop_drivers(inner_loop1_out$funding_list, inner_loop1_out$frs_fund)

results_new <- inner_drop1_funding_cached(i = 2,
                                              funding_list = inner_loop1_out$funding_list,
                                              drop_drivers = drop_drivers,
                                              params = params)

identical(results_original, results_new)
all.equal(results_original, results_new)
