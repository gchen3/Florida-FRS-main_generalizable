# V5.R — Funding Model (vanilla, no DROP)

# Note: This file assumes these exist in your session/env:
# - lm_env$get_liability_data_s(bm_env, wf_data_env, params)
# - get_current_hire_amortization_tables(...)
# - get_future_hire_amortization_tables(...)
# It also assumes 'params' contains all model inputs listed below.

# ----------------------- Pre-run data clearning ---------------------------------------------

get_funding_table <- function(class_name, init_funding_data, params) {
  funding_table <- init_funding_data %>%
    dplyr::filter(class == class_name) %>%
    dplyr::select(-class) %>%
    tibble::add_row(year = (params$start_year_ + 1):(params$start_year_ + params$model_period_))
  funding_table[is.na(funding_table)] <- 0
  funding_table
}

get_all_classes_funding_list <- function(init_funding_data, params) {
  funding_list <- lapply(params$class_names_, get_funding_table, init_funding_data, params)
  names(funding_list) <- params$class_names_
  funding_list
}

get_current_amort_layers_summary_table <- function(current_amort_layers_table){
  current_amort_layers_table %>%
    dplyr::mutate(amo_period = dplyr::if_else(amo_period == "n/a", "20", amo_period),
                  amo_period = as.numeric(amo_period)) %>%
    dplyr::summarise(amo_balance = sum(amo_balance), .by = c(class, amo_period)) %>%
    dplyr::arrange(class, dplyr::desc(amo_period))
}

params$ava_smooth_years_ <- 5       # years for AVA smoothing
params$ava_cap_upper_    <- 1.20    # upper AVA caps
params$ava_cap_lower_    <- 0.80    # upper AVA caps

# ---------------- Phase 1: per-class payroll, benefits, refunds, NC$, AAL ----

inner_loop1_payroll_benefits <- function(i, funding_list, liability_list, params) {
  for (class in params$class_names_no_drop_frs_) {
    cf <- funding_list[[class]]
    cl <- liability_list[[class]]
    
    cf <- within(cf, {
      # Payroll
      total_payroll[i]     <- total_payroll[i-1] * (1 + params$payroll_growth_)
      payroll_db_legacy[i] <- total_payroll[i] * payroll_db_legacy_ratio[i]
      payroll_db_new[i]    <- total_payroll[i] * payroll_db_new_ratio[i]
      payroll_dc_legacy[i] <- total_payroll[i] * payroll_dc_legacy_ratio[i]
      payroll_dc_new[i]    <- total_payroll[i] * payroll_dc_new_ratio[i]
      
      # Benefits & refunds
      ben_payment_legacy[i] <- cl$retire_ben_db_legacy_est[i] +
        cl$retire_ben_current_est[i] +
        cl$retire_ben_term_est[i]
      refund_legacy[i]      <- cl$refund_db_legacy_est[i]
      ben_payment_new[i]    <- cl$retire_ben_db_new_est[i]
      refund_new[i]         <- cl$refund_db_new_est[i]
      
      total_ben_payment[i]  <- ben_payment_legacy[i] + ben_payment_new[i]
      total_refund[i]       <- refund_legacy[i] + refund_new[i]
      
      # Total NC rate (weighted)
      total_nc_rate[i] <- (nc_rate_db_legacy[i] * payroll_db_legacy[i] +
                             nc_rate_db_new[i]    * payroll_db_new[i]) /
        (payroll_db_legacy[i] + payroll_db_new[i])
      
      # NC dollars
      nc_legacy[i] <- nc_rate_db_legacy[i] * payroll_db_legacy[i]
      nc_new[i]    <- nc_rate_db_new[i]    * payroll_db_new[i]
      
      # Liability & AAL roll-forward
      liability_gain_loss_legacy[i] <- cl$liability_gain_loss_legacy_est[i]
      liability_gain_loss_new[i]    <- cl$liability_gain_loss_new_est[i]
      total_liability_gain_loss[i]  <- cl$total_liability_gain_loss_est[i]
      
      aal_legacy[i] <- aal_legacy[i-1] * (1 + params$dr_current_) +
        (nc_legacy[i] - ben_payment_legacy[i] - refund_legacy[i]) * (1 + params$dr_current_)^0.5 +
        liability_gain_loss_legacy[i]
      
      aal_new[i] <- aal_new[i-1] * (1 + params$dr_new_) +
        (nc_new[i] - ben_payment_new[i] - refund_new[i]) * (1 + params$dr_new_)^0.5 +
        liability_gain_loss_new[i]
      
      total_aal[i] <- aal_legacy[i] + aal_new[i]
    })
    
    funding_list[[class]] <- cf
  }
  list(funding_list = funding_list)
}

# ----------- FRS snapshot after Phase 1 (sum of non-DROP classes) -----------

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
  
  # NC and total NC rate
  frs_fund$nc_legacy[i]     <- sum_fields("nc_legacy")
  frs_fund$nc_new[i]        <- sum_fields("nc_new")
  frs_fund$total_nc_rate[i] <- (frs_fund$nc_legacy[i] + frs_fund$nc_new[i]) /
    (frs_fund$payroll_db_legacy[i] + frs_fund$payroll_db_new[i])
  
  # Liability & AAL
  frs_fund$liability_gain_loss_legacy[i] <- sum_fields("liability_gain_loss_legacy")
  frs_fund$liability_gain_loss_new[i]    <- sum_fields("liability_gain_loss_new")
  frs_fund$total_liability_gain_loss[i]  <- sum_fields("total_liability_gain_loss")
  
  frs_fund$aal_legacy[i] <- sum_fields("aal_legacy")
  frs_fund$aal_new[i]    <- sum_fields("aal_new")
  frs_fund$total_aal[i]  <- sum_fields("total_aal")
  
  frs_fund
}

# --------- Phase 2: per-class contributions, flows, MVA, AVA bases ----------

inner_loop2_funding <- function(i,
                                funding_list,
                                frs_fund,
                                current_hire_amo_payment_list,
                                future_hire_amo_payment_list,
                                return_scenarios,
                                return_scen_index,
                                params,
                                classes){
  for (class in classes) {
    cf  <- funding_list[[class]]
    cap <- current_hire_amo_payment_list[[class]]
    fap <- future_hire_amo_payment_list[[class]]
    
    # NC rates
    cf$nc_rate_legacy[i] <- cf$nc_legacy[i] / cf$payroll_db_legacy[i]
    cf$nc_rate_new[i]    <- if (cf$payroll_db_new[i] == 0) 0 else cf$nc_new[i] / cf$payroll_db_new[i]
    
    # EE rates (constant)
    cf$ee_nc_rate_legacy[i] <- params$db_ee_cont_rate_
    cf$ee_nc_rate_new[i]    <- params$db_ee_cont_rate_
    
    # ER DB rates
    cf$er_nc_rate_legacy[i] <- cf$nc_rate_legacy[i] - cf$ee_nc_rate_legacy[i]
    cf$er_nc_rate_new[i]    <- cf$nc_rate_new[i]    - cf$ee_nc_rate_new[i]
    
    # Amortization rates
    cf$amo_rate_legacy[i] <- sum(cap[i-1, ]) / cf$payroll_db_legacy[i]
    cf$amo_rate_new[i]    <- if (cf$payroll_db_new[i] == 0) 0 else sum(fap[i-1, ]) / cf$payroll_db_new[i]
    
    # DC ER rates
    if (class == "drop") {
      cf$er_dc_rate_legacy[i] <- 0
      cf$er_dc_rate_new[i]    <- 0
    } else {
      nm <- stringr::str_replace(paste0(class, "_er_dc_cont_rate_"), " ", "_")
      er_dc_rate <- params[[nm]]
      cf$er_dc_rate_legacy[i] <- er_dc_rate
      cf$er_dc_rate_new[i]    <- er_dc_rate
    }
    
    # Admin rate
    cf$admin_exp_rate[i] <- cf$admin_exp_rate[i-1]
    
    # EE contributions
    cf$ee_nc_cont_legacy[i] <- cf$ee_nc_rate_legacy[i] * cf$payroll_db_legacy[i]
    cf$ee_nc_cont_new[i]    <- cf$ee_nc_rate_new[i]    * cf$payroll_db_new[i]
    frs_fund$ee_nc_cont_legacy[i] <- frs_fund$ee_nc_cont_legacy[i] + cf$ee_nc_cont_legacy[i]
    frs_fund$ee_nc_cont_new[i]    <- frs_fund$ee_nc_cont_new[i]    + cf$ee_nc_cont_new[i]
    
    # Admin expenses
    cf$admin_exp_legacy[i] <- cf$admin_exp_rate[i] * cf$payroll_db_legacy[i]
    cf$admin_exp_new[i]    <- cf$admin_exp_rate[i] * cf$payroll_db_new[i]
    frs_fund$admin_exp_legacy[i] <- frs_fund$admin_exp_legacy[i] + cf$admin_exp_legacy[i]
    frs_fund$admin_exp_new[i]    <- frs_fund$admin_exp_new[i]    + cf$admin_exp_new[i]
    
    # ER DB contributions
    cf$er_nc_cont_legacy[i]  <- cf$er_nc_rate_legacy[i] * cf$payroll_db_legacy[i] + cf$admin_exp_legacy[i]
    cf$er_nc_cont_new[i]     <- cf$er_nc_rate_new[i]    * cf$payroll_db_new[i]    + cf$admin_exp_new[i]
    cf$er_amo_cont_legacy[i] <- cf$amo_rate_legacy[i]   * cf$payroll_db_legacy[i]
    cf$er_amo_cont_new[i]    <- cf$amo_rate_new[i]      * cf$payroll_db_new[i]
    cf$total_er_db_cont[i]   <- cf$er_nc_cont_legacy[i] + cf$er_nc_cont_new[i] + cf$er_amo_cont_legacy[i] + cf$er_amo_cont_new[i]
    
    frs_fund$er_nc_cont_legacy[i]  <- frs_fund$er_nc_cont_legacy[i] + cf$er_nc_cont_legacy[i]
    frs_fund$er_nc_cont_new[i]     <- frs_fund$er_nc_cont_new[i]    + cf$er_nc_cont_new[i]
    frs_fund$er_amo_cont_legacy[i] <- frs_fund$er_amo_cont_legacy[i] + cf$er_amo_cont_legacy[i]
    frs_fund$er_amo_cont_new[i]    <- frs_fund$er_amo_cont_new[i]    + cf$er_amo_cont_new[i]
    frs_fund$total_er_db_cont[i]   <- frs_fund$total_er_db_cont[i]   + cf$total_er_db_cont[i]
    
    # ER DC contributions
    cf$er_dc_cont_legacy[i] <- cf$er_dc_rate_legacy[i] * cf$payroll_dc_legacy[i]
    cf$er_dc_cont_new[i]    <- cf$er_dc_rate_new[i]    * cf$payroll_dc_new[i]
    cf$total_er_dc_cont[i]  <- cf$er_dc_cont_legacy[i] + cf$er_dc_cont_new[i]
    
    frs_fund$er_dc_cont_legacy[i] <- frs_fund$er_dc_cont_legacy[i] + cf$er_dc_cont_legacy[i]
    frs_fund$er_dc_cont_new[i]    <- frs_fund$er_dc_cont_new[i]    + cf$er_dc_cont_new[i]
    frs_fund$total_er_dc_cont[i]  <- frs_fund$total_er_dc_cont[i]  + cf$total_er_dc_cont[i]
    
    # Returns
    cf$roa[i] <- return_scenarios[which(return_scenarios$year == cf$year[i]), return_scen_index][[1]]
    frs_fund$roa[i] <- cf$roa[i]
    
    # Solvency contribution & cash flows
    cf_legacy <- cf$ee_nc_cont_legacy[i] + cf$er_nc_cont_legacy[i] + cf$er_amo_cont_legacy[i] -
      cf$ben_payment_legacy[i] - cf$refund_legacy[i] - cf$admin_exp_legacy[i]
    cf_new <- cf$ee_nc_cont_new[i] + cf$er_nc_cont_new[i] + cf$er_amo_cont_new[i] -
      cf$ben_payment_new[i] - cf$refund_new[i] - cf$admin_exp_new[i]
    cf_total <- cf_legacy + cf_new
    
    cf$total_solv_cont[i] <- max(-(cf$mva[i-1] * (1 + cf$roa[i]) + cf_total * (1 + cf$roa[i])^0.5) / (1 + cf$roa[i])^0.5, 0)
    cf$solv_cont_legacy[i] <- cf$total_solv_cont[i] * cf$aal_legacy[i] / cf$total_aal[i]
    cf$solv_cont_new[i]    <- cf$total_solv_cont[i] * cf$aal_new[i]    / cf$total_aal[i]
    
    cf$net_cf_legacy[i] <- cf_legacy + cf$solv_cont_legacy[i]
    cf$net_cf_new[i]    <- cf_new    + cf$solv_cont_new[i]
    frs_fund$net_cf_legacy[i] <- frs_fund$net_cf_legacy[i] + cf$net_cf_legacy[i]
    frs_fund$net_cf_new[i]    <- frs_fund$net_cf_new[i]    + cf$net_cf_new[i]
    
    # MVA
    cf$mva_legacy[i] <- cf$mva_legacy[i-1] * (1 + cf$roa[i]) + cf$net_cf_legacy[i] * (1 + cf$roa[i])^0.5
    cf$mva_new[i]    <- cf$mva_new[i-1]    * (1 + cf$roa[i]) + cf$net_cf_new[i]    * (1 + cf$roa[i])^0.5
    cf$total_mva[i]  <- cf$mva_legacy[i] + cf$mva_new[i]
    
    frs_fund$mva_legacy[i] <- frs_fund$mva_legacy[i] + cf$mva_legacy[i]
    frs_fund$mva_new[i]    <- frs_fund$mva_new[i]    + cf$mva_new[i]
    frs_fund$total_mva[i]  <- frs_fund$total_mva[i]  + cf$total_mva[i]
    
    # AVA bases
    cf$ava_base_legacy[i] <- cf$ava_legacy[i-1] + cf$net_cf_legacy[i]/2
    cf$ava_base_new[i]    <- cf$ava_new[i-1]    + cf$net_cf_new[i]/2
    
    funding_list[[class]] <- cf
  }
  
  list(funding_list = funding_list, frs_fund = frs_fund)
}

# ---------------------- FRS AVA smoothing -----------------------------------

inner_frs_fund2 <- function(i, frs_fund, params){
  years  <- params$ava_smooth_years_
  cap_up <- params$ava_cap_upper_
  cap_dn <- params$ava_cap_lower_
  w <- 1 / if (is.finite(years) && years > 0) years else 1
  
  ## Legacy
  frs_fund$exp_inv_earnings_ava_legacy[i] <- frs_fund$ava_legacy[i-1] * params$dr_current_ +
    frs_fund$net_cf_legacy[i] * params$dr_current_ / 2
  frs_fund$exp_ava_legacy[i] <- frs_fund$ava_legacy[i-1] +
    frs_fund$net_cf_legacy[i] + frs_fund$exp_inv_earnings_ava_legacy[i]
  
  legacy_mid <- frs_fund$exp_ava_legacy[i] +
    (frs_fund$mva_legacy[i] - frs_fund$exp_ava_legacy[i]) * w
  frs_fund$ava_legacy[i] <- pmin(pmax(legacy_mid,
                                      frs_fund$mva_legacy[i] * cap_dn),
                                 frs_fund$mva_legacy[i] * cap_up)
  
  frs_fund$alloc_inv_earnings_ava_legacy[i] <- frs_fund$ava_legacy[i] -
    frs_fund$ava_legacy[i-1] - frs_fund$net_cf_legacy[i]
  frs_fund$ava_base_legacy[i] <- frs_fund$ava_legacy[i-1] + frs_fund$net_cf_legacy[i] / 2
  
  ## New
  frs_fund$exp_inv_earnings_ava_new[i] <- frs_fund$ava_new[i-1] * params$dr_new_ +
    frs_fund$net_cf_new[i] * params$dr_new_ / 2
  frs_fund$exp_ava_new[i] <- frs_fund$ava_new[i-1] +
    frs_fund$net_cf_new[i] + frs_fund$exp_inv_earnings_ava_new[i]
  
  new_mid <- frs_fund$exp_ava_new[i] +
    (frs_fund$mva_new[i] - frs_fund$exp_ava_new[i]) * w
  frs_fund$ava_new[i] <- pmin(pmax(new_mid,
                                   frs_fund$mva_new[i] * cap_dn),
                              frs_fund$mva_new[i] * cap_up)
  
  frs_fund$alloc_inv_earnings_ava_new[i] <- frs_fund$ava_new[i] - frs_fund$ava_new[i-1] - frs_fund$net_cf_new[i]
  frs_fund$ava_base_new[i] <- frs_fund$ava_new[i-1] + frs_fund$net_cf_new[i] / 2
  
  frs_fund
}

# --- Phase 3: per-class AVA from FRS allocated earnings ---------------------

inner_loop3_ava_development <- function(i, funding_list, frs_fund, params, classes){
  for (class in classes) {
    cf <- funding_list[[class]]
    
    # Legacy
    cf$alloc_inv_earnings_ava_legacy[i] <-
      frs_fund$alloc_inv_earnings_ava_legacy[i] *
      (cf$ava_base_legacy[i] / frs_fund$ava_base_legacy[i])
    
    cf$unadj_ava_legacy[i] <- cf$ava_legacy[i-1] +
      cf$net_cf_legacy[i] +
      cf$alloc_inv_earnings_ava_legacy[i]
    
    # New (guard zero only)
    cf$alloc_inv_earnings_ava_new[i] <- dplyr::if_else(
      frs_fund$ava_base_new[i] == 0, 0,
      frs_fund$alloc_inv_earnings_ava_new[i] * (cf$ava_base_new[i] / frs_fund$ava_base_new[i])
    )
    
    cf$unadj_ava_new[i] <- cf$ava_new[i-1] +
      cf$net_cf_new[i] +
      cf$alloc_inv_earnings_ava_new[i]
    
    funding_list[[class]] <- cf
  }
  funding_list
}

# ------------- Phase 4: per-class UAL/FR and all-in cost --------------------

inner_loop5_all_in_cost <- function(i, funding_list, frs_fund, params, classes){
  for (class in classes) {
    cf <- funding_list[[class]]
    
    cf$total_ava[i] <- cf$ava_legacy[i] + cf$ava_new[i]
    frs_fund$total_ava[i] <- frs_fund$total_ava[i] + cf$total_ava[i]
    
    cf$ual_ava_legacy[i] <- cf$aal_legacy[i] - cf$ava_legacy[i]
    cf$ual_ava_new[i]    <- cf$aal_new[i]    - cf$ava_new[i]
    cf$total_ual_ava[i]  <- cf$ual_ava_legacy[i] + cf$ual_ava_new[i]
    
    frs_fund$ual_ava_legacy[i] <- frs_fund$ual_ava_legacy[i] + cf$ual_ava_legacy[i]
    frs_fund$ual_ava_new[i]    <- frs_fund$ual_ava_new[i]    + cf$ual_ava_new[i]
    frs_fund$total_ual_ava[i]  <- frs_fund$total_ual_ava[i]  + cf$total_ual_ava[i]
    
    cf$ual_mva_legacy[i] <- cf$aal_legacy[i] - cf$mva_legacy[i]
    cf$ual_mva_new[i]    <- cf$aal_new[i]    - cf$mva_new[i]
    cf$total_ual_mva[i]  <- cf$ual_mva_legacy[i] + cf$ual_mva_new[i]
    
    frs_fund$ual_mva_legacy[i] <- frs_fund$ual_mva_legacy[i] + cf$ual_mva_legacy[i]
    frs_fund$ual_mva_new[i]    <- frs_fund$ual_mva_new[i]    + cf$ual_mva_new[i]
    frs_fund$total_ual_mva[i]  <- frs_fund$total_ual_mva[i]  + cf$total_ual_mva[i]
    
    cf$fr_mva[i] <- cf$total_mva[i] / cf$total_aal[i]
    cf$fr_ava[i] <- cf$total_ava[i] / cf$total_aal[i]
    frs_fund$fr_mva[i] <- frs_fund$total_mva[i] / frs_fund$total_aal[i]
    frs_fund$fr_ava[i] <- frs_fund$total_ava[i] / frs_fund$total_aal[i]
    
    # Contributions & all-in cost
    cf$total_er_cont[i]       <- cf$total_er_db_cont[i] + cf$total_er_dc_cont[i] + cf$total_solv_cont[i]
    frs_fund$total_er_cont[i] <- frs_fund$total_er_cont[i] + cf$total_er_cont[i]
    
    cf$total_er_cont_rate[i]  <- cf$total_er_cont[i] / cf$total_payroll[i]
    frs_fund$total_er_cont_rate[i] <- frs_fund$total_er_cont[i] / frs_fund$total_payroll[i]
    
    cf$total_er_cont_real[i] <- cf$total_er_cont[i] / (1 + params$inflation_)^(cf$year[i] - params$start_year_)
    frs_fund$total_er_cont_real[i] <- frs_fund$total_er_cont_real[i] + cf$total_er_cont_real[i]
    
    cf$cum_er_cont_real[i] <- if (i == 2) cf$total_er_cont_real[i] else cf$cum_er_cont_real[i-1] + cf$total_er_cont_real[i]
    frs_fund$cum_er_cont_real[i] <- frs_fund$cum_er_cont_real[i] + cf$cum_er_cont_real[i]
    
    cf$total_ual_mva_real[i] <- cf$total_ual_mva[i] / (1 + params$inflation_)^(cf$year[i] - params$start_year_)
    frs_fund$total_ual_mva_real[i] <- frs_fund$total_ual_mva_real[i] + cf$total_ual_mva_real[i]
    
    cf$all_in_cost_real[i] <- cf$cum_er_cont_real[i] + cf$total_ual_mva_real[i]
    frs_fund$all_in_cost_real[i] <- frs_fund$all_in_cost_real[i] + cf$all_in_cost_real[i]
    
    funding_list[[class]] <- cf
  }
  list(funding_list = funding_list, frs_fund = frs_fund)
}

# --------------------- Phase 5: amortization layers -------------------------

inner_loop6_amortization <- function(i,
                                     funding_list,
                                     current_hire_debt_layer_list,
                                     future_hire_debt_layer_list,
                                     current_hire_amo_period_list,
                                     future_hire_amo_period_list,
                                     current_hire_amo_payment_list,
                                     future_hire_amo_payment_list,
                                     amo_pay_growth,
                                     params,
                                     classes){
  for (class in classes) {
    cf  <- funding_list[[class]]
    
    chd <- current_hire_debt_layer_list[[class]]
    fhd <- future_hire_debt_layer_list[[class]]
    
    chap <- current_hire_amo_period_list[[class]]
    fhap <- future_hire_amo_period_list[[class]]
    
    cap <- current_hire_amo_payment_list[[class]]
    fap <- future_hire_amo_payment_list[[class]]
    
    # Legacy layers
    chd[i, 2:ncol(chd)] <- chd[i-1, 1:(ncol(chd)-1)] * (1 + params$dr_current_) -
      cap[i-1, 1:ncol(cap)] * (1 + params$dr_current_)^0.5
    chd[i, 1] <- cf$ual_ava_legacy[i] - sum(chd[i, 2:ncol(chd)])
    
    cap[i, 1:ncol(cap)] <- get_pmt(
      r    = params$dr_current_,
      g    = amo_pay_growth,
      nper = chap[i, 1:ncol(chap)],
      pv   = chd[i, 1:(ncol(chd)-1)],
      t    = 0.5
    )
    
    # New layers
    fhd[i, 2:ncol(fhd)] <- fhd[i-1, 1:(ncol(fhd)-1)] * (1 + params$dr_new_) -
      fap[i-1, 1:ncol(fap)] * (1 + params$dr_new_)^0.5
    fhd[i, 1] <- cf$ual_ava_new[i] - sum(fhd[i, 2:ncol(fhd)])
    
    fap[i, 1:ncol(fap)] <- get_pmt(
      r    = params$dr_new_,
      g    = amo_pay_growth,
      nper = fhap[i, 1:ncol(fhap)],
      pv   = fhd[i, 1:(ncol(fhd)-1)],
      t    = 0.5
    )
    
    current_hire_debt_layer_list[[class]]  <- chd
    future_hire_debt_layer_list[[class]]   <- fhd
    current_hire_amo_payment_list[[class]] <- cap
    future_hire_amo_payment_list[[class]]  <- fap
  }
  
  list(
    current_hire_debt_layer_list  = current_hire_debt_layer_list,
    future_hire_debt_layer_list   = future_hire_debt_layer_list,
    current_hire_amo_payment_list = current_hire_amo_payment_list,
    future_hire_amo_payment_list  = future_hire_amo_payment_list
  )
}

# ----------------------------- MAIN LOOP --------------------------

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
    
    # (1) per-class core (non-DROP classes)
    res <- inner_loop1_payroll_benefits(i, funding_list, liability_list, params)
    funding_list <- res$funding_list
    
    # (2) FRS core snapshot (pre-DROP)
    classes_core <- params$class_names_no_drop_frs_
    frs_fund <- summarize_frs_payroll_benefits(i, funding_list, classes_core, frs_fund)
    
    # (3) per-class flows/MVA/AVA bases (no DROP in vanilla)
    classes_for_flows <- params$class_names_no_drop_frs_
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
    
    # (4) FRS AVA smoothing
    frs_fund <- inner_frs_fund2(i, frs_fund, params)
    
    # (5) per-class unadjusted AVA
    funding_list <- inner_loop3_ava_development(i, funding_list, frs_fund, params, classes_for_flows)
    
    # (6) finalize class AVA (vanilla = use unadjusted AVA)
    for (class in classes_for_flows) {
      cf <- funding_list[[class]]
      cf$ava_legacy[i] <- cf$unadj_ava_legacy[i]
      cf$ava_new[i]    <- cf$unadj_ava_new[i]
      funding_list[[class]] <- cf
    }
    
    # (7) per-class UAL/FR/all-in + push to FRS
    res <- inner_loop5_all_in_cost(i, funding_list, frs_fund, params, classes_for_flows)
    funding_list <- res$funding_list
    frs_fund     <- res$frs_fund
    
    # (8) amortization layers
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
    
    # store back
    funding_list$frs <- frs_fund
  }
  
  funding_list
}

# --------------------------- TOP-LEVEL DRIVER -------------------------------

get_funding_data <- function(liab_data_env, params, return = "unstacked") {
  funding_list               <- params$funding_list
  current_amort_layers_table <- params$current_amort_layers_table
  
  # Liability outputs (provided by lm_env)
  classes <- params$class_names_no_drop_frs_
  liability_list <- liab_data_env$liability_list
  
  # liab_all <- lm_env$get_liability_data_s(bf_data_env, wf_data_env, params)
  # liability_list <- purrr::map(
  #   classes,
  #   ~ liab_all %>% dplyr::filter(class == .x) %>% dplyr::select(-class)
  # ) %>% purrr::set_names(classes)
  
  # Model calibration (payroll ratios, NC rates, initial AAL)
  for (class in params$class_names_no_drop_frs_) {
    fund_data <- funding_list[[class]]
    liab_data <- liability_list[[class]]
    
    # Payroll ratios (lagged)
    fund_data$payroll_db_legacy_ratio <- dplyr::lag(liab_data$payroll_db_legacy_est / liab_data$total_payroll_est)
    fund_data$payroll_db_new_ratio    <- dplyr::lag(liab_data$payroll_db_new_est    / liab_data$total_payroll_est)
    fund_data$payroll_dc_legacy_ratio <- dplyr::lag(liab_data$payroll_dc_legacy_est / liab_data$total_payroll_est)
    fund_data$payroll_dc_new_ratio    <- dplyr::lag(liab_data$payroll_dc_new_est    / liab_data$total_payroll_est)
    
    # NC rates (lagged & calibrated)
    nc_cal <- params[[paste0(class, "_nc_cal_")]]
    fund_data$nc_rate_db_legacy <- dplyr::lag(liab_data$nc_rate_db_legacy_est * nc_cal)
    fund_data$nc_rate_db_new    <- dplyr::lag(liab_data$nc_rate_db_new_est    * nc_cal)
    
    # AAL initialization
    fund_data$aal_legacy[1]     <- liab_data$aal_legacy_est[1]
    fund_data$total_aal[1]      <- liab_data$total_aal_est[1]
    fund_data$ual_ava_legacy[1] <- fund_data$aal_legacy[1] - fund_data$ava_legacy[1]
    fund_data$total_ual_ava[1]  <- fund_data$total_aal[1]  - fund_data$total_ava[1]
    
    funding_list[[class]] <- fund_data
  }
  
  # Amortization setup
  amo_period_new <- params$amo_period_new_
  amo_col_num <- max(current_amort_layers_table$amo_period, amo_period_new + params$funding_lag_)
  amo_pay_growth <- ifelse(params$amo_method_ == "level $", 0, params$amo_pay_growth_)
  
  # Current-hire amortization tables
  current_by_class <- params$class_names_no_frs_ |>
    rlang::set_names() |>
    purrr::map(~ get_current_hire_amortization_tables(
      class_name = .x,
      current_amort_layers_table = current_amort_layers_table,
      amo_col_num = amo_col_num,
      amo_pay_growth = amo_pay_growth,
      params = params
    ))
  current_by_table <- purrr::transpose(current_by_class)
  names(current_by_table) <- names(current_by_class[[1]])
  current_name_map <- c(
    current_hire_amo_period_table  = "current_hire_amo_period_list",
    current_hire_debt_layer_table  = "current_hire_debt_layer_list",
    current_hire_amo_payment_table = "current_hire_amo_payment_list"
  )
  current_hire_amo_list <- rlang::set_names(current_by_table, current_name_map[names(current_by_table)])
  
  # Future-hire amortization tables
  future_by_class <- params$class_names_no_frs_ |>
    rlang::set_names() |>
    purrr::map(~ get_future_hire_amortization_tables(
      class_name = .x,
      amo_col_num = amo_col_num,
      params = params
    ))
  future_by_table <- purrr::transpose(future_by_class)
  names(future_by_table) <- names(future_by_class[[1]])
  future_name_map <- c(
    future_hire_amo_period_table  = "future_hire_amo_period_list",
    future_hire_debt_layer_table  = "future_hire_debt_layer_list",
    future_hire_amo_payment_table = "future_hire_amo_payment_list"
  )
  future_hire_amo_list <- rlang::set_names(future_by_table, future_name_map[names(future_by_table)])
  
  # Run vanilla main loop
  funding_list <- main_loop(
    funding_list = funding_list,
    liability_list = liability_list,
    current_hire_amo_payment_list = current_hire_amo_list$current_hire_amo_payment_list,
    future_hire_amo_payment_list  = future_hire_amo_list$future_hire_amo_payment_list,
    current_hire_amo_period_list  = current_hire_amo_list$current_hire_amo_period_list,
    future_hire_amo_period_list   = future_hire_amo_list$future_hire_amo_period_list,
    current_hire_debt_layer_list  = current_hire_amo_list$current_hire_debt_layer_list,
    future_hire_debt_layer_list   = future_hire_amo_list$future_hire_debt_layer_list,
    amo_pay_growth = amo_pay_growth,
    params = params
  )
  
  if (return == "stacked") dplyr::bind_rows(funding_list, .id = "class") else funding_list
}

# =========================== End V5.R ========================================
