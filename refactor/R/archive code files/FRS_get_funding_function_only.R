################### Model function starts here ####################

get_funding_data <- function(
    params,
    return = "unstacked" # or "stacked"
) {
  
  # unpack parameters
  funding_list <- params$funding_list
  current_amort_layers_table <- params$current_amort_layers_table
  wf_data_list <- params$wf_data_list
  entrant_profile_table_list <- params$entrant_profile_table_list
  salary_headcount_table_list <- params$salary_headcount_table_list
  mort_table_list <- params$mort_table_list
  mort_retire_table_list <- params$mort_retire_table_list
  separation_rate_table_list <- params$separation_rate_table_list  
  
  funding_lag <- params$funding_lag_
  model_period <- params$model_period_
  
  cola_tier_1_active_constant <- params$cola_tier_1_active_constant_
  cola_tier_1_active <- params$cola_tier_1_active_
  cola_tier_2_active <- params$cola_tier_2_active_
  cola_tier_3_active <- params$cola_tier_3_active_
  cola_current_retire <- params$cola_current_retire_
  cola_current_retire_one <- params$cola_current_retire_one_
  one_time_cola <- params$one_time_cola_
  
  #inputs below are for the liability model
  non_special_db_new_ratio <- params$non_special_db_new_ratio_
  special_db_new_ratio <- params$special_db_new_ratio_
  
  #inputs below are for the funding model
  amo_period_new <- params$amo_period_new_
  
  
  # returns updated funding_list
  
  
  # unpack funding_list into a stacked tibble
  funding_list_stacked <- bind_rows(funding_list, .id = "class")
  # djb save it to see if I can reproduce using stacked input data
  saveRDS(funding_list_stacked, fs::path(stackdir, "funding_list_stacked.rds"))
  
  #### Produce liability outputs for each class (except DROP and FRS system) ----
  
  # Use mclapply to run the liability model in parallel. May not work properly
  # with Windows OS or API. Switch back to lapply if needed. When working,
  # mclapply will be about twice as fast as lapply.
  a <- proc.time()
  classes <- params$class_names_no_drop_frs_
  liab_all <- lm_env$get_liability_data_s(bm_env, wf_data_env, params)
  liability_list <- map(
    classes,
    ~ liab_all %>% filter(class == .x) %>% select(-class)
  ) %>% set_names(classes)
  
  
  # get values of arguments to get_liability_data for this class and then call it
  # call_get_liability_data <- function(class_name) {
  #   # create lists of data frames so that get_liablity_data does not have to (dangerously) pull data from the global environment with assign
  #   # 
  #   # element_name <- paste0(class_name, "_wf_data")
  #   # wf_data <- params$wf_data_list[[element_name]]
  #   # 
  #   # ben_payment_current <- params[[paste0(class_name, "_ben_payment_current_")]]
  #   # retiree_pop_current <- params[[paste0(class_name, "_retiree_pop_current_")]]
  #   # pvfb_term_current <- params[[paste0(class_name, "_pvfb_term_current_")]]
  #   # 
  #   # element_name <- paste0(class_name, "_entrant_profile_table")
  #   # entrant_profile_table <- params$entrant_profile_table_list[[element_name]]
  #   # 
  #   # element_name <- paste0(class_name, "_salary_headcount_table")
  #   # salary_headcount_table <- params$salary_headcount_table_list[[element_name]]    
  #   # 
  #   # element_name <- paste0(class_name, "_mort_table")
  #   # mort_table <- params$mort_table_list[[element_name]]     
  #   # 
  #   # element_name <- paste0(class_name, "_separation_rate_table")
  #   # separation_rate_table <- params$separation_rate_table_list[[element_name]]         
  #   # 
  #   # element_name <- paste0(class_name, "_mort_retire_table")
  #   # mort_retire_table <- params$mort_retire_table_list[[element_name]]         
  #   
  #   lm_env$get_liability_data_s(bm_env,
  #                             wf_data_env, 
  #                             params) %>% filter(class == class_name) %>% select(-class)
  # }
  # 
  # liability_list <- mclapply(
  #   X = params$class_names_no_drop_frs_, 
  #   FUN = call_get_liability_data,
  #   # Set mc.cores to 1 for compatibility with Windows
  #   mc.cores = 1
  # )
  # names(liability_list) <- params$class_names_no_drop_frs_
  b <- proc.time()
  print("liability_list time")
  print(b - a)
  
  # FOR LATER USE (djb): unpack liability_list into a stacked tibble
  # liability_list_stacked <- bind_rows(liability_list, .id = "class")
  # # save it to see if I can reproduce using stacked input data
  # saveRDS(liability_list_stacked, fs::path(stackdir, "liability_list_stacked.rds"))
  # 
  # # FOR LATER USE (djb) classes_stacked
  # classes_stacked <- funding_list_stacked |> 
  #   filter(class %in% params$class_names_no_drop_frs_) |>
  #   left_join(liability_list_stacked,
  #             by = join_by(class, year)) |> 
  #   left_join(params$nc_cal_ |> 
  #               mutate(class = str_replace(class, "_", "")) |> # make senior management uniform SOON!!
  #               rename(nc_cal = nc_cal_), # djb this is correct - refers to a column name not the global variable
  #             by = join_by(class)) |> 
  #   arrange(class, year) |> # make sure we get the lags right
  #   # new variables,  use lag to align with the funding mechanism
  #   mutate(# payroll calibration
  #     payroll_db_legacy_ratio = lag(payroll_db_legacy_est / total_payroll_est),
  #     payroll_db_new_ratio = lag(payroll_db_new_est / total_payroll_est),
  #     payroll_dc_legacy_ratio = lag(payroll_dc_legacy_est / total_payroll_est),
  #     payroll_dc_new_ratio = lag(payroll_dc_new_est / total_payroll_est),
  #     
  #     # normal cost calibration/projection
  #     nc_rate_db_legacy = lag(nc_rate_db_legacy_est * nc_cal),
  #     nc_rate_db_new = lag(nc_rate_db_new_est * nc_cal),
  #     
  #     # aal calibration - no great way to do this in a chain so use 4 ifelse statements
  #     aal_legacy = if_else(year == first(year), aal_legacy_est, aal_legacy),
  #     total_aal = if_else(year == first(year), total_aal_est, total_aal),
  #     
  #     ual_ava_legacy = ifelse(year == first(year),
  #                             aal_legacy - ava_legacy,
  #                             ual_ava_legacy),
  #     
  #     total_ual_ava = ifelse(year == first(year),
  #                            total_aal - total_ava,
  #                            total_ual_ava),
  #     .by=class)
  
  # names(flstacked) |> sort()
  
  # djb: examine the drop comments immediately below
  #Create a "liability" data for the DROP plan
  #This is a makeshift solution for now. Proper modeling of the DROP plan will be done in the future.
  # drop_liability_output <- funding_list[["drop"]]
  
  #### Model calibration ----
  # does the same thing as classes_stacked above does
  a <- proc.time()
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
  
  ####Set up amo period sequences
  #Create two lists, one for the current hire amo periods, and one for new hire amo periods
  # Each has 8 elements (1 per class excl. frs), each element is a matrix 31 years x 21 columns
  # current_hire_amo_period_list$regular
  
  # djb Determine amo payment parameters BEFORE calling routines related to amo_payment ----
  
  #Determine the number of columns for the amo period tables
  amo_col_num <- max(current_amort_layers_table$amo_period, amo_period_new + params$funding_lag_)  
  
  #Level % or level $ for debt amortization 
  # create LOCAL variable amo_pay_growth - I moved this up from below
  amo_pay_growth <- ifelse(params$amo_method_ == "level $", 0, params$amo_pay_growth_)
  
  # ----- CURRENT -----
  current_by_class <- params$class_names_no_frs_ |>
    set_names() |>
    map(~ get_current_hire_amortization_tables(
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
  
  current_hire_amo_list <- set_names(current_by_table,
                                     current_name_map[names(current_by_table)])
  
  library(purrr)
  
  # ----- FUTURE -----
  future_by_class <- params$class_names_no_frs_ |>
    set_names() |>
    map(~ get_future_hire_amortization_tables(
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
  
  future_hire_amo_list <- set_names(future_by_table,
                                    future_name_map[names(future_by_table)])
  
  # current_hire_amo_period_list <- purrr::set_names(params$class_names_no_frs_) |> 
  #   # returns a list of 8 matrices, 31 x 21 (nyears x amo_col_num)
  #   purrr::map(
  #     get_current_hire_amo_period_table,
  #     current_amort_layers_table,
  #     class_amo_layers_table,
  #     amo_col_num,
  #     params)  
  # 
  # future_hire_amo_period_list <- purrr::set_names(params$class_names_no_frs_) |> 
  #   # returns a list of 8 matrices, 31 x 21 (nyears x amo_col_num)
  #   purrr::map(
  #     get_future_hire_amo_period_table,
  #     amo_col_num,
  #     params)
  # 
  # 
  # 
  # ####Set up the UAAL layer and amo payment tables for current members and initialize the first UAAL layer and amo payments
  # #UAAL layers tables for current members
  # current_hire_debt_layer_list <- purrr::set_names(params$class_names_no_frs_) |> 
  #   # returns a list of 8 matrices, 31 x 22 (nyears x amo_col_num+1)
  #   purrr::map(
  #     get_current_hire_debt_layer_table,
  #     current_amort_layers_table,
  #     amo_col_num,
  #     params)
  # 
  # current_hire_amo_payment_list <- purrr::set_names(params$class_names_no_frs_) |> 
  #   # returns a list of 8 matrices, 31 x 21 (nyears x amo_col_num)
  #   purrr::map(
  #     get_current_hire_amo_payment_table,
  #     current_hire_amo_payment_table,
  #     current_hire_debt_layer_list,
  #     current_hire_amo_period_list,
  #     amo_col_num,
  #     amo_pay_growth, # DO NOT CHANGE TO params$amo_pay_growth, this can be different
  #     params)
  # 
  # ####Set up the UAL layer and amo payment tables for new members
  # future_hire_debt_layer_list <- purrr::set_names(params$class_names_no_frs_) |> 
  #   # returns a list of 8 matrices, 31 x 22 (nyears x amo_col_num+1)
  #   purrr::map( 
  #     get_future_hire_debt_layer_table,
  #     amo_col_num,
  #     params)
  # 
  # #Amo payment tables for new members
  # future_hire_amo_payment_list <- purrr::set_names(params$class_names_no_frs_) |> 
  #   # returns a list of 8 matrices, 31 x 21 (nyears x amo_col_num)
  #   purrr::map(
  #     get_future_hire_amo_payment_table,
  #     amo_col_num,
  #     params)
  
  # djb: create nested tibble with these matrices
  # verify that all names are the same and in the same order
  # amo_table <- tibble(
  #   class = names(current_hire_amo_period_list),
  #   
  #   current_hire_amo_period = purrr::map(current_hire_amo_period_list, \(x) x),
  #   current_hire_amo_payment = purrr::map(current_hire_amo_payment_list, \(x) x),
  #   current_hire_debt_layer = purrr::map(current_hire_debt_layer_list, \(x) x),
  #   
  #   future_hire_amo_period = purrr::map(future_hire_amo_period_list, \(x) x),
  #   future_hire_amo_payment = purrr::map(future_hire_amo_payment_list, \(x) x),
  #   future_hire_debt_layer = purrr::map(future_hire_debt_layer_list, \(x) x)
  # )
  
  # here's how to verify that the names are all properly aligned
  # amo_table |> 
  #   mutate(across(-class, \(x) names(x)))
  
  # djb: this next block seems DANGEROUS - they modify a GLOBAL variable,
  # return_scenarios, and further, use hard-coded values
  
  #Set return values for "model" and "assumption" scenarios
  #Set 2023 returns and update "model" and "assumption" scenarios
  # djb CAUTION does this need to be in funding model?? ----
  # return_scenarios <- params$return_scenarios |> 
  #   mutate(across(-year, \(x) ifelse(year==2023, params$return_2023_, x)),
  #          model=ifelse(year > 2023, params$model_return_, model),
  #          assumption=ifelse(year > 2023, params$dr_current_, assumption))    
  
  #Return scenario
  # return_scen <- "recur_recession"
  # return_scen_index <- which(colnames(params$return_scenarios) == params$return_scen_)
  
  
  funding_list <- main_loop(funding_list = funding_list,
                            liability_list = liability_list,
                            current_hire_amo_payment_list = current_hire_amo_list$current_hire_amo_payment_list,
                            future_hire_amo_payment_list = future_hire_amo_list$future_hire_amo_payment_list,
                            current_hire_amo_period_list = current_hire_amo_list$current_hire_amo_period_list,
                            future_hire_amo_period_list = future_hire_amo_list$future_hire_amo_period_list,
                            current_hire_debt_layer_list = current_hire_amo_list$current_hire_debt_layer_list,
                            future_hire_debt_layer_list = future_hire_amo_list$future_hire_debt_layer_list,
                            amo_pay_growth,
                            params)
  
  if (return == "stacked") {
    output <- bind_rows(funding_list, .id = "class")
  } else {
    output <- funding_list
  }
  
  return(output)
  
}