#################################################################
##                       Liability Model                       ##
#################################################################

get_wf_active_df_final_s <- function(wf_active_df_s,
                                     benefit_val_table_s,
                                     params){
  #Join wf active table with FinalData table to calculate the overall payroll, normal costs, PVFB, and PVFS each year
  wf_active_df_final_s <- wf_active_df_s %>%
    filter(year <= params$start_year_ + params$model_period_) %>% 
    mutate(entry_year = year - (age - entry_age)) %>% 
    left_join(benefit_val_table_s, by = c("class", "entry_age", "age" = "term_age", "year" = "term_year", "entry_year")) %>% 
    select(class, entry_age, age, year, entry_year, n_active, indv_norm_cost, salary, 
           pvfb_db_wealth_at_current_age, pvfnc_db, pvfs_at_current_age) %>% 
    left_join(params$db_dc_legacy_table, 
              by = join_by(class,
                           entry_year >= year_ll, 
                           entry_year < year_ul)) %>%
    left_join(params$db_dc_new_table,
              by = join_by(class,
                           entry_year >= new_year_ll,
                           entry_year < new_year_ul)) %>%
    replace(is.na(.), 0) %>%
    # filter(n_active > 0) %>% 
    #allocate members to plan designs based on entry year
    mutate(n_active_db_legacy = n_active * db_legacy,
           n_active_db_new = n_active * db_new,
           n_active_dc_legacy = n_active * dc_legacy,
           n_active_dc_new = n_active * dc_new) %>%
    group_by(class, year) %>% 
    summarise(
      #Payroll
      payroll_db_legacy_est = sum(salary * n_active_db_legacy),
      payroll_db_new_est = sum(salary * n_active_db_new),
      payroll_dc_legacy_est = sum(salary * n_active_dc_legacy),
      payroll_dc_new_est = sum(salary * n_active_dc_new),
      total_payroll_est = sum(salary * n_active),
      #Normal cost rates
      nc_rate_db_legacy_est = if_else(payroll_db_legacy_est == 0, 
                                      0, 
                                      sum(indv_norm_cost * salary * n_active_db_legacy) / sum(salary * n_active_db_legacy)),
      nc_rate_db_new_est = if_else(payroll_db_new_est == 0, 
                                   0, 
                                   sum(indv_norm_cost * salary * n_active_db_new) / sum(salary * n_active_db_new)),
      #Present value of future benefits
      pvfb_active_db_legacy_est = sum(pvfb_db_wealth_at_current_age * n_active_db_legacy),
      pvfb_active_db_new_est = sum(pvfb_db_wealth_at_current_age * n_active_db_new),
      #Present value of future normal costs
      pvfnc_db_legacy_est = sum(pvfnc_db * n_active_db_legacy),
      pvfnc_db_new_est = sum(pvfnc_db * n_active_db_new),
      #Count of active members
      total_n_active = sum(n_active)
    ) %>% 
    ungroup() %>% 
    mutate(payroll_db_est = payroll_db_legacy_est + payroll_db_new_est,
           payroll_dc_est = payroll_dc_legacy_est + payroll_dc_new_est,
           total_nc_rate_est = if_else(payroll_db_est == 0, 0, (nc_rate_db_legacy_est * payroll_db_legacy_est + nc_rate_db_new_est * payroll_db_new_est) / payroll_db_est),
           aal_active_db_legacy_est = pvfb_active_db_legacy_est - pvfnc_db_legacy_est,
           aal_active_db_new_est = pvfb_active_db_new_est - pvfnc_db_new_est) %>% 
    replace(is.na(.), 0)
  
  return(wf_active_df_final_s)
}


get_wf_term_df_final_s <- function(
    wf_term_df_s,
    benefit_val_table_s,
    benefit_table_s,
    params
) {
  #Term table
  wf_term_df_final_s <- wf_term_df_s %>% 
    filter(year <= params$start_year_ + params$model_period_,
           n_term > 0) %>% 
    mutate(entry_year = year - (age - entry_age)) %>% 
    #join benefit_val_table to get PV_DB_Benefit (the present value of benefits at termination)
    left_join(benefit_val_table_s, by = c("class", "entry_age", "term_year", "entry_year")) %>% 
    select(class, entry_age, age, year, term_year, entry_year, dist_age, n_term, pvfb_db_at_term_age) %>% 
    #join benefit_table to get the surv_DR at current age
    left_join(benefit_table_s %>% 
                select(-pvfb_db_at_term_age), 
              by = c("class", "entry_age", "age" = "dist_age", "year" = "dist_year", "term_year", "entry_year")) %>% 
    select(class, entry_age, age, year, term_year, entry_year, dist_age, n_term, pvfb_db_at_term_age, cum_mort_dr) %>% 
    #rename to clarify variables' meanings
    rename(cum_mort_dr_current = cum_mort_dr) %>% 
    left_join(params$db_dc_legacy_table, 
              by = join_by(class,
                           entry_year >= year_ll, 
                           entry_year < year_ul)) %>%
    left_join(params$db_dc_new_table,
              by = join_by(class,
                           entry_year >= new_year_ll,
                           entry_year < new_year_ul)) %>%
    mutate(
      #pvfb_db_term = First DB benefit * annuity factor at retirement * surv_DR at retirement / surv_DR at current time
      #Note that pvfb_db_at_term_ag = First DB benefit * annuity factor at retirement * surv_DR at retirement
      pvfb_db_term = pvfb_db_at_term_age / cum_mort_dr_current,
      n_term_db_legacy = n_term * db_legacy,
      n_term_db_new = n_term * db_new
    ) %>% 
    group_by(class, year) %>% 
    summarise(aal_term_db_legacy_est = sum(pvfb_db_term * n_term_db_legacy),
              aal_term_db_new_est = sum(pvfb_db_term * n_term_db_new)
    ) %>% 
    ungroup()
  
  return(wf_term_df_final_s)
}


get_wf_refund_df_final_s <- function(wf_refund_df_s,
                                     benefit_table_s,
                                     params){
  # Join wf refund table with benefit table to calculate the overall refunds each year
  wf_refund_df_final_s <- wf_refund_df_s %>% 
    filter(year <= params$start_year_ + params$model_period_,
           n_refund > 0) %>% 
    mutate(entry_year = year - (age - entry_age)) %>% 
    left_join(benefit_table_s, 
              by = c("class", "entry_age", "age" = "dist_age", "year" = "dist_year", "term_year", "entry_year")) %>% 
    select(class, entry_age, age, year, term_year, entry_year, n_refund, db_ee_balance) %>% 
    left_join(params$db_dc_legacy_table, 
              by = join_by(class,
                           entry_year >= year_ll, 
                           entry_year < year_ul)) %>%
    left_join(params$db_dc_new_table,
              by = join_by(class,
                           entry_year >= new_year_ll,
                           entry_year < new_year_ul)) %>%
    #allocate members to plan designs based on entry year
    mutate(n_refund_db_legacy = n_refund * db_legacy,
           n_refund_db_new = n_refund * db_new
    ) %>%
    # mutate(n_refund_db_legacy = if_else(entry_year < 2018, 
    #                                     n_refund * ratios$db_legacy_before_2018_ratio,
    #                                     if_else(entry_year < params$new_year_, 
    #                                             n_refund * ratios$db_legacy_after_2018_ratio, 
    #                                             0)),
    #        n_refund_db_new = if_else(entry_year < params$new_year_, 0, n_refund * ratios$db_new_ratio)
    # ) %>%
    group_by(class, year) %>% 
    summarise(refund_db_legacy_est = sum(db_ee_balance * n_refund_db_legacy),
              refund_db_new_est = sum(db_ee_balance * n_refund_db_new)
    ) %>% 
    ungroup()
  
  return(wf_refund_df_final_s)
}


get_wf_retire_df_final_s <- function(wf_retire_df_s,
                                     benefit_table_s,
                                     ann_factor_table_s,
                                     params){
  # Join wf retire table with benefit table to calculate the overall retirement benefits each year
  wf_retire_df_final_s <- wf_retire_df_s %>% 
    filter(year <= params$start_year_ + params$model_period_) %>% 
    mutate(entry_year = year - (age - entry_age)) %>%    
    left_join(benefit_table_s, by = c("class", "entry_age", "entry_year", "term_year", "retire_year" = "dist_year")) %>% 
    select(class, entry_age, age, year, term_year, retire_year, entry_year, n_retire, db_benefit, cola) %>% 
    left_join(ann_factor_table_s %>% 
                select(-cola), 
              by = c("class", "entry_age", "entry_year", "term_year", "year" = "dist_year")) %>% 
    select(class, entry_age, age, year, term_year, retire_year, entry_year, n_retire, db_benefit, cola, ann_factor) %>% 
    rename(base_db_benefit = db_benefit) %>% 
    #Adjust the benefit based on COLA and allocate members to plan designs based on entry year
    left_join(params$db_dc_legacy_table, 
              by = join_by(class,
                           entry_year >= year_ll, 
                           entry_year < year_ul)) %>%
    left_join(params$db_dc_new_table,
              by = join_by(class,
                           entry_year >= new_year_ll,
                           entry_year < new_year_ul)) %>%
    mutate(
      db_benefit_final = base_db_benefit * (1 + cola)^(year - retire_year),
      n_retire_db_legacy = n_retire * db_legacy,
      n_retire_db_new = n_retire * db_new,
      #We use "AnnuityFactor_DR - 1" below because the PVFB for retirees excludes the first payment (i.e. the first payment has already been delivered when the PVFB is calculated)
      pvfb_db_retire = db_benefit_final * (ann_factor - 1)
    ) %>% 
    group_by(class, year) %>% 
    summarise(retire_ben_db_legacy_est = sum(db_benefit_final * n_retire_db_legacy),
              retire_ben_db_new_est = sum(db_benefit_final * n_retire_db_new),
              
              aal_retire_db_legacy_est = sum(pvfb_db_retire * n_retire_db_legacy),
              aal_retire_db_new_est = sum(pvfb_db_retire * n_retire_db_new)
    ) %>% 
    ungroup()
  
  return(wf_retire_df_final_s)    
}


get_wf_retire_current_final_s <- function(ann_factor_retire_table_s,
                                          params) {
  
  # Project benefit payments for current retirees
  retire_current_int_s <- params$retiree_distribution %>% 
    select(age, n_retire_ratio, total_ben_ratio) %>% 
    crossing(params$current_year_table %>% select(class, retiree_pop_current, ben_payment_current)) %>%
    mutate(
      n_retire_current = n_retire_ratio * retiree_pop_current,
      total_ben_current = total_ben_ratio * ben_payment_current,
      avg_ben_current = total_ben_current / n_retire_current,
      year = params$start_year_
    )
  
  wf_retire_current_s <- ann_factor_retire_table_s %>% 
    filter(year <= params$start_year_ + params$model_period_) %>% 
    left_join(retire_current_int_s, by = c("class","age", "year")) %>% 
    select(base_age:ann_factor_retire, n_retire_current, avg_ben_current, total_ben_current, class) %>% 
    group_by(class, base_age) %>% 
    mutate(n_retire_current = pentools::recur_grow(n_retire_current, -mort_final),
           avg_ben_current = pentools::recur_grow2(avg_ben_current, cola),
           total_ben_current = n_retire_current * avg_ben_current,
           #W e use "AnnuityFactor_DR - 1" below because the PVFB for retirees excludes the first payment (i.e. the first payment has already been delivered when the PVFB is calculated)
           pvfb_retire_current = avg_ben_current * (ann_factor_retire - 1)
    ) %>% 
    filter(!is.na(n_retire_current)) %>% 
    ungroup()
  
  wf_retire_current_final_s <- wf_retire_current_s %>% 
    group_by(class, year) %>% 
    summarise(retire_ben_current_est = sum(total_ben_current),
              aal_retire_current_est = sum(n_retire_current * pvfb_retire_current)
    ) %>% 
    ungroup()
  # rename(year = Years)
  
  return(wf_retire_current_final_s)
}


get_wf_term_current_s <- function(
    params){
  
  # Project benefit payments for current term vested members
  # Note that we use the original "dr_current_" in calculating the benefit payments so that any discount rate adjustment can work
  # Set model years
  year <- params$start_year_:(params$start_year_ + params$model_period_)
  amo_years_term <- (params$start_year_ + 1):(params$start_year_ + params$amo_period_term_)
  
  # Build all-class version correctly
  wf_term_current_s <- params$current_year_table %>%
    select(class, pvfb_term_current) %>%
    mutate(retire_ben_term = purrr::map_dbl(
      pvfb_term_current,
      ~ get_pmt(
        r = params$dr_current_,
        nper = params$amo_period_term_,
        pv = .x,
        g = params$payroll_growth_
      )
    )) %>%
    rowwise() %>%
    mutate(retire_ben_term_vec = list({
      vec <- double(length = length(year))
      vec[year %in% amo_years_term] <- pentools::recur_grow3(
        retire_ben_term,
        g = params$payroll_growth_,
        nper = params$amo_period_term_
      )
      vec
    })
    ) %>%
    ungroup() %>%
    mutate(year = list(year)) %>%
    unnest(c(year, retire_ben_term_vec)) %>%
    rename(retire_ben_term_est = retire_ben_term_vec) %>%
    group_by(class) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      aal_term_current_est = pentools::roll_pv(
        rate = params$dr_current_,
        g = params$payroll_growth_,
        nper = params$amo_period_term_,
        pmt_vec = retire_ben_term_est
      )
    ) %>%
    ungroup() %>%
    select(class, year, retire_ben_term_est, aal_term_current_est)
  
  return(wf_term_current_s)
  
}

get_funding_df_s <- function(wf_active_df_final_s,
                             wf_term_df_final_s,
                             wf_refund_df_final_s,
                             wf_retire_df_final_s,
                             wf_retire_current_final_s,
                             wf_term_current_s,
                             params)
  
  ##### Funding model - liability side
{
  funding_df_s <- wf_active_df_final_s %>% 
    left_join(wf_term_df_final_s,
              by = join_by(class, year)) %>% 
    left_join(wf_refund_df_final_s,
              by = join_by(class, year)) %>% 
    left_join(wf_retire_df_final_s,
              by = join_by(class, year)) %>%
    left_join(wf_retire_current_final_s,
              by = join_by(class, year)) %>% 
    left_join(wf_term_current_s,
              by = join_by(class, year)) %>%
    replace(is.na(.), 0) %>% 
    mutate(
      aal_legacy_est = aal_active_db_legacy_est + aal_term_db_legacy_est + aal_retire_db_legacy_est + aal_retire_current_est + aal_term_current_est,
      aal_new_est = aal_active_db_new_est + aal_term_db_new_est + aal_retire_db_new_est,
      total_aal_est = aal_legacy_est + aal_new_est,
      tot_ben_refund_legacy_est = refund_db_legacy_est + retire_ben_db_legacy_est + retire_ben_current_est + retire_ben_term_est,
      tot_ben_refund_new_est = refund_db_new_est + retire_ben_db_new_est,
      tot_ben_refund_est = tot_ben_refund_legacy_est + tot_ben_refund_new_est
    )
  
  # Initialize output list
  funding_list <- list()
  
  #Calculate liability gain/loss if any and project AAL using the roll forward method
  for (class_name in unique(funding_df_s$class)) {
    funding_df <- funding_df_s %>% filter(class == class_name)
    
    funding_df$liability_gain_loss_legacy_est <- 0
    funding_df$liability_gain_loss_new_est <- 0
    funding_df$total_liability_gain_loss_est <- 0
    
    funding_df$aal_legacy_roll <- 0
    funding_df$aal_new_roll <- 0
    funding_df$total_aal_roll <- 0
    
    for (i in 1:nrow(funding_df)) {
      if (i == 1) {
        funding_df$liability_gain_loss_legacy_est[i] <- 0
        funding_df$liability_gain_loss_new_est[i] <- 0
        
        funding_df$aal_legacy_roll[i] <- funding_df$aal_legacy_est[i]
        funding_df$aal_new_roll[i] <- funding_df$aal_new_est[i]
        
      } else {
        
        funding_df$liability_gain_loss_legacy_est[i] <- round(funding_df$aal_legacy_est[i] -
                                                                (funding_df$aal_legacy_est[i-1] * (1 + params$dr_current_) +
                                                                   funding_df$payroll_db_legacy_est[i-1] * funding_df$nc_rate_db_legacy_est[i-1] -
                                                                   funding_df$tot_ben_refund_legacy_est[i]),
                                                              digits = 1)
        
        funding_df$liability_gain_loss_new_est[i] <- round(funding_df$aal_new_est[i] -
                                                             (funding_df$aal_new_est[i-1] * (1 + params$dr_new_) + 
                                                                funding_df$payroll_db_new_est[i-1] * funding_df$nc_rate_db_new_est[i-1] -
                                                                funding_df$tot_ben_refund_new_est[i]), 
                                                           digits = 1)
        
        funding_df$aal_legacy_roll[i] <- funding_df$aal_legacy_roll[i-1] * (1 + params$dr_current_) +
          funding_df$payroll_db_legacy_est[i-1] * funding_df$nc_rate_db_legacy_est[i-1] -
          funding_df$tot_ben_refund_legacy_est[i] +
          funding_df$liability_gain_loss_legacy_est[i]
        
        funding_df$aal_new_roll[i] <- funding_df$aal_new_roll[i-1] * (1 + params$dr_new_) +
          funding_df$payroll_db_new_est[i-1] * funding_df$nc_rate_db_new_est[i-1] -
          funding_df$tot_ben_refund_new_est[i] + 
          funding_df$liability_gain_loss_new_est[i]
      }
    }
    
    funding_df$total_liability_gain_loss_est <- funding_df$liability_gain_loss_legacy_est + funding_df$liability_gain_loss_new_est
    funding_df$total_aal_roll <- funding_df$aal_legacy_roll + funding_df$aal_new_roll
    
    funding_list[[class_name]] <- funding_df
    
  }
  
  funding_df_s <- bind_rows(funding_list)
  
  
  return(funding_df_s)
}

# main function -----------------------------------------------------------
get_liability_data_s <- function(
    bm_env,
    wf_data_env,
    params
) {
  
  # unpack the wf_data and benefit_data objects
  wf_active_df_s <- wf_data_env$wf_active_df_s
  wf_term_df_s <- wf_data_env$wf_term_df_s
  wf_refund_df_s <- wf_data_env$wf_refund_df_s
  wf_retire_df_s <- wf_data_env$wf_retire_df_s
  
  benefit_val_table_s <- bm_env$benefit_data_s$benefit_val_table 
  benefit_table_s <- bm_env$benefit_data_s$benefit_table 
  ann_factor_table_s <- bm_env$benefit_data_s$ann_factor_table 
  ann_factor_retire_table_s <- bm_env$benefit_data_s$ann_factor_retire_table
  
  wf_active_df_final_s <- get_wf_active_df_final_s(
    wf_active_df_s,
    benefit_val_table_s,
    params
  )
  
  wf_term_df_final_s <- get_wf_term_df_final_s(
    wf_term_df_s,
    benefit_val_table_s,
    benefit_table_s,
    params
  )  
  
  wf_refund_df_final_s <- get_wf_refund_df_final_s(
    wf_refund_df_s,
    benefit_table_s,
    params
  )    
  
  wf_retire_df_final_s <- get_wf_retire_df_final_s(
    wf_retire_df_s,
    benefit_table_s,
    ann_factor_table_s,
    params
  )
  
  wf_retire_current_final_s <- get_wf_retire_current_final_s(
    ann_factor_retire_table_s,
    params
  )
  
  wf_term_current_s <- get_wf_term_current_s(
    params
  )
  
  funding_df_s <- get_funding_df_s(wf_active_df_final_s,
                                   wf_term_df_final_s,
                                   wf_refund_df_final_s,
                                   wf_retire_df_final_s,
                                   wf_retire_current_final_s,
                                   wf_term_current_s,
                                   params)
  
  # Check liability gain/loss
  # If the liability gain/loss isn't 0 under the perfect condition (experience = assumption), something must be wrong.
  
  return(funding_df_s)
}


