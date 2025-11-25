# drop.R — DROP module for V5 (override main_loop when sourced) ---------

# Source AFTER V5.R. Then set:
#   params$enable_drop_ <- TRUE
# to run with DROP. If FALSE, this main_loop behaves like vanilla V5.



# 1) Build DROP drivers from Regular + FRS working totals
build_drop_drivers <- function(funding_list, frs_fund) {
  reg <- funding_list$regular
  n   <- nrow(reg)
  
  out <- data.frame(
    db_legacy_share   = rep(NA_real_, n),
    db_new_share      = rep(NA_real_, n),
    g_ben             = rep(NA_real_, n),
    g_ref             = rep(NA_real_, n),
    ben_legacy_share  = rep(NA_real_, n),
    ref_legacy_share  = rep(NA_real_, n),
    nc_rate_db_legacy = rep(NA_real_, n),
    nc_rate_db_new    = rep(NA_real_, n)
  )
  
  for (i in 2:n) {
    # Payroll split (fold DC into DB for DROP)
    out$db_legacy_share[i] <- reg$payroll_db_legacy_ratio[i] + reg$payroll_dc_legacy_ratio[i]
    out$db_new_share[i]    <- reg$payroll_db_new_ratio[i]    + reg$payroll_dc_new_ratio[i]
    
    # Growth factors t-1 -> t
    out$g_ben[i] <- reg$total_ben_payment[i] / reg$total_ben_payment[i-1]
    out$g_ref[i] <- reg$total_refund[i]      / reg$total_refund[i-1]
    
    # Legacy shares at t
    out$ben_legacy_share[i] <- reg$ben_payment_legacy[i] / reg$total_ben_payment[i]
    out$ref_legacy_share[i] <- reg$refund_legacy[i]      / reg$total_refund[i]
    
    # NC rates from FRS at t
    out$nc_rate_db_legacy[i] <- frs_fund$nc_legacy[i] / frs_fund$payroll_db_legacy[i]
    out$nc_rate_db_new[i] <- if (is.na(frs_fund$payroll_db_new[i])) NA_real_
    else if (frs_fund$payroll_db_new[i] == 0) 0
    else frs_fund$nc_new[i] / frs_fund$payroll_db_new[i]
  }
  
  out
}

# 2) DROP step 1: payroll/benefits/NC/AAL using cached drivers
drop_step1_funding <- function(i, funding_list, drop_drivers, params){
  d         <- drop_drivers[i, ]
  drop_fund <- funding_list$drop
  
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
  
  # NC (rates from drivers)
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

# 3) Add DROP to FRS working totals
frs_add_drop_step <- function(i, frs_fund, drop_fund){
  frs_fund$total_payroll[i]     <- frs_fund$total_payroll[i]     + drop_fund$total_payroll[i]
  frs_fund$payroll_db_legacy[i] <- frs_fund$payroll_db_legacy[i] + drop_fund$payroll_db_legacy[i]
  frs_fund$payroll_db_new[i]    <- frs_fund$payroll_db_new[i]    + drop_fund$payroll_db_new[i]
  
  frs_fund$ben_payment_legacy[i] <- frs_fund$ben_payment_legacy[i] + drop_fund$ben_payment_legacy[i]
  frs_fund$ben_payment_new[i]    <- frs_fund$ben_payment_new[i]    + drop_fund$ben_payment_new[i]
  frs_fund$refund_legacy[i]      <- frs_fund$refund_legacy[i]      + drop_fund$refund_legacy[i]
  frs_fund$refund_new[i]         <- frs_fund$refund_new[i]         + drop_fund$refund_new[i]
  frs_fund$total_ben_payment[i]  <- frs_fund$total_ben_payment[i]  + drop_fund$total_ben_payment[i]
  frs_fund$total_refund[i]       <- frs_fund$total_refund[i]       + drop_fund$total_refund[i]
  
  frs_fund$nc_legacy[i] <- frs_fund$nc_legacy[i] + drop_fund$nc_legacy[i]
  frs_fund$nc_new[i]    <- frs_fund$nc_new[i]    + drop_fund$nc_new[i]
  frs_fund$total_nc_rate[i] <- (frs_fund$nc_legacy[i] + frs_fund$nc_new[i]) /
    (frs_fund$payroll_db_legacy[i] + frs_fund$payroll_db_new[i])
  
  frs_fund$nc_rate_db_legacy[i] <- frs_fund$nc_legacy[i] / frs_fund$payroll_db_legacy[i]
  frs_fund$nc_rate_db_new[i]    <- dplyr::if_else(frs_fund$payroll_db_new[i] == 0, 0, frs_fund$nc_new[i] / frs_fund$payroll_db_new[i])
  
  frs_fund$aal_legacy[i] <- frs_fund$aal_legacy[i] + drop_fund$aal_legacy[i]
  frs_fund$aal_new[i]    <- frs_fund$aal_new[i]    + drop_fund$aal_new[i]
  frs_fund$total_aal[i]  <- frs_fund$total_aal[i]  + drop_fund$total_aal[i]
  
  frs_fund
}

# 4) DROP step 2: asset reallocation vs system AVA
drop_step2_asset_reallocation <- function(i, funding_list, frs_fund) {
  drop_fund <- funding_list$drop
  
  drop_fund$net_reallocation_legacy[i] <-
    drop_fund$unadj_ava_legacy[i] - drop_fund$aal_legacy[i] * frs_fund$ava_legacy[i] / frs_fund$aal_legacy[i]
  drop_fund$ava_legacy[i] <- drop_fund$unadj_ava_legacy[i] - drop_fund$net_reallocation_legacy[i]
  
  drop_fund$net_reallocation_new[i] <- dplyr::if_else(
    frs_fund$aal_new[i] == 0, 0,
    drop_fund$unadj_ava_new[i] - drop_fund$aal_new[i] * frs_fund$ava_new[i] / frs_fund$aal_new[i]
  )
  drop_fund$ava_new[i] <- drop_fund$unadj_ava_new[i] - drop_fund$net_reallocation_new[i]
  
  funding_list$drop <- drop_fund
  funding_list
}

# 5) Apply DROP reallocation back to classes
class_apply_drop_reallocation <- function(i, funding_list, frs_fund, params){
  for (class in params$class_names_no_drop_frs_) {
    cf <- funding_list[[class]]
    
    # Legacy
    class_drop_prop_legacy <- cf$aal_legacy[i] / (frs_fund$aal_legacy[i] - funding_list$drop$aal_legacy[i])
    cf$net_reallocation_legacy[i] <- class_drop_prop_legacy * funding_list$drop$net_reallocation_legacy[i]
    cf$ava_legacy[i] <- cf$unadj_ava_legacy[i] + cf$net_reallocation_legacy[i]
    
    # New
    class_drop_prop_new <- dplyr::if_else(
      (frs_fund$aal_new[i] - funding_list$drop$aal_new[i]) == 0, 0,
      cf$aal_new[i] / (frs_fund$aal_new[i] - funding_list$drop$aal_new[i])
    )
    cf$net_reallocation_new[i] <- class_drop_prop_new * funding_list$drop$net_reallocation_new[i]
    cf$ava_new[i] <- cf$unadj_ava_new[i] + cf$net_reallocation_new[i]
    
    funding_list[[class]] <- cf
  }
  funding_list
}



# Override main_loop: DROP-aware flow (falls back to vanilla if di --------


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
  
  for (i in 2:nrow(funding_list[[1]])) {
    frs_fund <- funding_list$frs
    
    # (1) per-class payroll/benefits/NC$/AAL (non-DROP classes only)
    res <- inner_loop1_payroll_benefits(i, funding_list, liability_list, params)
    funding_list <- res$funding_list
    
    # (2) FRS core snapshot (pre-DROP)
    classes_core <- params$class_names_no_drop_frs_
    frs_fund <- summarize_frs_payroll_benefits(i, funding_list, classes_core, frs_fund)
    
    drop_on <- isTRUE(params$enable_drop_)
    classes_for_flows <- if (drop_on) params$class_names_no_frs_ else params$class_names_no_drop_frs_
    
    # (3) DROP step 1 (if enabled)
    if (drop_on) {
      drop_drivers <- build_drop_drivers(funding_list, frs_fund)
      funding_list <- drop_step1_funding(i, funding_list, drop_drivers, params)
      frs_fund     <- frs_add_drop_step(i, frs_fund, funding_list$drop)
    }
    
    # (4) per-class funding flows/MVA/AVA bases
    res <- inner_loop2_funding(
      i,
      funding_list, frs_fund,
      current_hire_amo_payment_list,
      future_hire_amo_payment_list,
      params$return_scenarios,
      params$return_scen_index,
      params,
      classes_for_flows
    )
    funding_list <- res$funding_list
    frs_fund     <- res$frs_fund
    
    # (5) FRS AVA smoothing
    frs_fund <- inner_frs_fund2(i, frs_fund, params)
    
    # (6) per-class unadjusted AVA from FRS allocated earnings
    funding_list <- inner_loop3_ava_development(i, funding_list, frs_fund, params, classes_for_flows)
    
    # (7) finalize AVA
    if (drop_on) {
      # DROP: compute asset reallocation and push back to classes
      funding_list <- drop_step2_asset_reallocation(i, funding_list, frs_fund)
      funding_list <- class_apply_drop_reallocation(i, funding_list, frs_fund, params)
    } else {
      # Vanilla: ava = unadj
      for (class in classes_for_flows) {
        cf <- funding_list[[class]]
        cf$ava_legacy[i] <- cf$unadj_ava_legacy[i]
        cf$ava_new[i]    <- cf$unadj_ava_new[i]
        funding_list[[class]] <- cf
      }
    }
    
    # (8) per-class UAL/FR/all-in + push to FRS
    res <- inner_loop5_all_in_cost(i, funding_list, frs_fund, params, classes_for_flows)
    funding_list <- res$funding_list
    frs_fund     <- res$frs_fund
    
    # (9) amortization layers
    res <- inner_loop6_amortization(
      i,
      funding_list,
      current_hire_debt_layer_list,
      future_hire_debt_layer_list,
      current_hire_amo_period_list,
      future_hire_amo_period_list,
      current_hire_amo_payment_list,
      future_hire_amo_payment_list,
      amo_pay_growth,
      params,
      classes_for_flows
    )
    current_hire_debt_layer_list  <- res$current_hire_debt_layer_list
    future_hire_debt_layer_list   <- res$future_hire_debt_layer_list
    current_hire_amo_payment_list <- res$current_hire_amo_payment_list
    future_hire_amo_payment_list  <- res$future_hire_amo_payment_list
    
    # Store FRS back
    funding_list$frs <- frs_fund
  }
  
  funding_list
}

# End DROP module ---------------------------------------------------------
# Usage:
#   source("V5.R",  local = fm_env)
#   source("drop.R", local = fm_env)        # overrides main_loop
#   params$enable_drop_ <- TRUE
#   baseline <- fm_env$get_funding_data(params, return = "stacked")


