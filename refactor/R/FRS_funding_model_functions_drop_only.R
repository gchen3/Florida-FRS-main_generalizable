# =====================================================================
# drop.R — DROP MODULE for V5
# Source this AFTER V5.R. It overrides the DROP hooks.
# To use: params$enable_drop_ <- TRUE; source("drop.R")
# =====================================================================

# 1) Build DROP drivers from Regular + FRS working totals
build_drop_drivers_hook <- function(funding_list, frs_fund) {
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
    
    # growth factors i-1 -> i
    out$g_ben[i] <- reg$total_ben_payment[i] / reg$total_ben_payment[i-1]
    out$g_ref[i] <- reg$total_refund[i]      / reg$total_refund[i-1]
    
    # composition shares at i
    out$ben_legacy_share[i] <- reg$ben_payment_legacy[i] / reg$total_ben_payment[i]
    out$ref_legacy_share[i] <- reg$refund_legacy[i]      / reg$total_refund[i]
    
    # NC rates from FRS at i (legacy plain; new: guard zero; NA stays NA)
    out$nc_rate_db_legacy[i] <- frs_fund$nc_legacy[i] / frs_fund$payroll_db_legacy[i]
    out$nc_rate_db_new[i]    <- if (is.na(frs_fund$payroll_db_new[i])) NA_real_
    else if (frs_fund$payroll_db_new[i] == 0) 0
    else frs_fund$nc_new[i] / frs_fund$payroll_db_new[i]
  }
  
  out
}

# 2) DROP step 1: payroll/benefits/NC/AAL (cached drivers)
drop_step1_funding_hook <- function(i, funding_list, drop_drivers, params){
  if (is.null(drop_drivers)) return(funding_list)
  
  drop_fund <- funding_list$drop
  d         <- drop_drivers[i, ]
  
  # Payroll (no DC in DROP)
  drop_fund$total_payroll[i]     <- drop_fund$total_payroll[i-1] * (1 + params$payroll_growth_)
  drop_fund$payroll_db_legacy[i] <- drop_fund$total_payroll[i] * d$db_legacy_share
  drop_fund$payroll_db_new[i]    <- drop_fund$total_payroll[i] * d$db_new_share
  
  # Benefits & refunds: grow totals then split
  drop_fund$total_ben_payment[i] <- drop_fund$total_ben_payment[i-1] * d$g_ben
  drop_fund$total_refund[i]      <- drop_fund$total_refund[i-1]      * d$g_ref
  
  drop_fund$ben_payment_legacy[i] <- drop_fund$total_ben_payment[i] * d$ben_legacy_share
  drop_fund$ben_payment_new[i]    <- drop_fund$total_ben_payment[i] * (1 - d$ben_legacy_share)
  drop_fund$refund_legacy[i]      <- drop_fund$total_refund[i] * d$ref_legacy_share
  drop_fund$refund_new[i]         <- drop_fund$total_refund[i] * (1 - d$ref_legacy_share)
  
  # NC (rates from FRS drivers)
  drop_fund$nc_rate_db_legacy[i] <- d$nc_rate_db_legacy
  drop_fund$nc_rate_db_new[i]    <- d$nc_rate_db_new
  drop_fund$nc_legacy[i]         <- drop_fund$nc_rate_db_legacy[i] * drop_fund$payroll_db_legacy[i]
  drop_fund$nc_new[i]            <- drop_fund$nc_rate_db_new[i]    * drop_fund$payroll_db_new[i]
  drop_fund$total_nc_rate[i]     <- (drop_fund$nc_legacy[i] + drop_fund$nc_new[i]) /
    (drop_fund$payroll_db_legacy[i] + drop_fund$payroll_db_new[i])
  
  # AAL roll-forward
  drop_fund$aal_legacy[i] <- drop_fund$aal_legacy[i-1] * (1 + params$dr_current_) +
    (drop_fund$nc_legacy[i] - drop_fund$ben_payment_legacy[i] - drop_fund$refund_legacy[i]) * (1 + params$dr_current_)^0.5 +
    drop_fund$liability_gain_loss_legacy[i]
  
  drop_fund$aal_new[i] <- drop_fund$aal_new[i-1] * (1 + params$dr_new_) +
    (drop_fund$nc_new[i] - drop_fund$ben_payment_new[i] - drop_fund$refund_new[i]) * (1 + params$dr_new_)^0.5 +
    drop_fund$liability_gain_loss_new[i]
  
  drop_fund$total_aal[i] <- drop_fund$aal_legacy[i] + drop_fund$aal_new[i]
  
  funding_list$drop <- drop_fund
  funding_list
}

# 3) Add DROP to FRS working totals (post-step1)
frs_add_drop_hook <- function(i, frs_fund, funding_list){
  drop_fund <- funding_list$drop
  
  frs_fund$total_payroll[i]     <- frs_fund$total_payroll[i]     + drop_fund$total_payroll[i]
  frs_fund$payroll_db_legacy[i] <- frs_fund$payroll_db_legacy[i] + drop_fund$payroll_db_legacy[i]
  frs_fund$payroll_db_new[i]    <- frs_fund$payroll_db_new[i]    + drop_fund$payroll_db_new[i]
  
  frs_fund$ben_payment_legacy[i] <- frs_fund$ben_payment_legacy[i] + drop_fund$ben_payment_legacy[i]
  frs_fund$refund_legacy[i]      <- frs_fund$refund_legacy[i]      + drop_fund$refund_legacy[i]
  frs_fund$ben_payment_new[i]    <- frs_fund$ben_payment_new[i]    + drop_fund$ben_payment_new[i]
  frs_fund$refund_new[i]         <- frs_fund$refund_new[i]         + drop_fund$refund_new[i]
  frs_fund$total_ben_payment[i]  <- frs_fund$total_ben_payment[i]  + drop_fund$total_ben_payment[i]
  frs_fund$total_refund[i]       <- frs_fund$total_refund[i]       + drop_fund$total_refund[i]
  
  frs_fund$nc_legacy[i]     <- frs_fund$nc_legacy[i]     + drop_fund$nc_legacy[i]
  frs_fund$nc_new[i]        <- frs_fund$nc_new[i]        + drop_fund$nc_new[i]
  frs_fund$total_nc_rate[i] <- (frs_fund$nc_legacy[i] + frs_fund$nc_new[i]) /
    (frs_fund$payroll_db_legacy[i] + frs_fund$payroll_db_new[i])
  
  frs_fund$nc_rate_db_legacy[i] <- frs_fund$nc_legacy[i] / frs_fund$payroll_db_legacy[i]
  frs_fund$nc_rate_db_new[i]    <- if_else(frs_fund$payroll_db_new[i] == 0, 0, frs_fund$nc_new[i] / frs_fund$payroll_db_new[i])
  
  frs_fund$aal_legacy[i] <- frs_fund$aal_legacy[i] + drop_fund$aal_legacy[i]
  frs_fund$aal_new[i]    <- frs_fund$aal_new[i]    + drop_fund$aal_new[i]
  frs_fund$total_aal[i]  <- frs_fund$total_aal[i]  + drop_fund$total_aal[i]
  
  frs_fund
}

# 4) DROP step 2: asset reallocation vs system AVA
drop_step2_asset_reallocation_hook <- function(i, funding_list, frs_fund) {
  drop_fund <- funding_list$drop
  
  drop_fund$net_reallocation_legacy[i] <-
    drop_fund$unadj_ava_legacy[i] - drop_fund$aal_legacy[i] * frs_fund$ava_legacy[i] / frs_fund$aal_legacy[i]
  drop_fund$ava_legacy[i] <- drop_fund$unadj_ava_legacy[i] - drop_fund$net_reallocation_legacy[i]
  
  drop_fund$net_reallocation_new[i] <- if_else(
    frs_fund$aal_new[i] == 0, 0,
    drop_fund$unadj_ava_new[i] - drop_fund$aal_new[i] * frs_fund$ava_new[i] / frs_fund$aal_new[i]
  )
  drop_fund$ava_new[i] <- drop_fund$unadj_ava_new[i] - drop_fund$net_reallocation_new[i]
  
  funding_list$drop <- drop_fund
  funding_list
}

# 5) Apply DROP reallocation back to classes and finalize AVA
finalize_ava_hook <- function(i, funding_list, frs_fund, params, classes){
  for (class in classes) {
    if (class == "drop") next
    cf <- funding_list[[class]]
    
    # Legacy
    class_drop_prop_legacy <- cf$aal_legacy[i] / (frs_fund$aal_legacy[i] - funding_list$drop$aal_legacy[i])
    cf$net_reallocation_legacy[i] <- class_drop_prop_legacy * funding_list$drop$net_reallocation_legacy[i]
    cf$ava_legacy[i] <- cf$unadj_ava_legacy[i] + cf$net_reallocation_legacy[i]
    
    # New
    class_drop_prop_new <- if_else(
      (frs_fund$aal_new[i] - funding_list$drop$aal_new[i]) == 0, 0,
      cf$aal_new[i] / (frs_fund$aal_new[i] - funding_list$drop$aal_new[i])
    )
    cf$net_reallocation_new[i] <- class_drop_prop_new * funding_list$drop$net_reallocation_new[i]
    cf$ava_new[i] <- cf$unadj_ava_new[i] + cf$net_reallocation_new[i]
    
    funding_list[[class]] <- cf
  }
  
  # ensure DROP’s AVA already set from reallocation step (no change here)
  funding_list
}

# Optional: convenience toggler
enable_drop <- function(params) {
  params$enable_drop_ <- TRUE
  params
}

# =====================================================================
# End DROP module
# =====================================================================
