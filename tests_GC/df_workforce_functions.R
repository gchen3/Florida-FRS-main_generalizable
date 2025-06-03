entry_age_range <- frs_data_env$regular_entrant_profile_table$entry_age
year_range <- params$start_year_:(params$start_year_ + params$model_period_)   
age_range <- min(entry_age_range):max(params$age_range_)
retire_year_range <- year_range
term_year_range <- year_range
salary_headcount_table <- frs_data_env$regular_salary_headcount_table
mort_table <- frs_data_env$regular_mort_table
separation_rate_table <- frs_data_env$regular_separation_rate_table
retire_refund_ratio = params$retire_refund_ratio_
benefit_val_table <- bm_env$benefit_data_s$benefit_val_table_s %>% filter(class == "regular") %>% select(-class)
entrant_profile_table <- frs_data_env$regular_entrant_profile_table 
pop_growth=params$pop_growth_

initialize_arrays <- function(
    age_range,
    entry_age_range,
    year_range,
    term_year_range,
    retire_year_range,
    salary_headcount_table,
    mort_table,
    separation_rate_table,
    benefit_val_table,
    retire_refund_ratio
){
  # Define array dimensions and names ----
  active_dim <- c(length(entry_age_range), length(age_range), length(year_range))
  active_dim_names <- list(entry_age = entry_age_range, 
                           age = age_range, 
                           year = year_range)
  
  term_dim <- c(length(entry_age_range), 
                length(age_range), 
                length(year_range), 
                length(term_year_range))
  term_dim_names <- list(entry_age = entry_age_range, 
                         age = age_range, 
                         year = year_range, 
                         term_year = term_year_range)
  
  retire_dim <- c(length(entry_age_range), 
                  length(age_range), 
                  length(year_range), 
                  length(term_year_range), 
                  length(retire_year_range))
  retire_dim_names <- list(entry_age = entry_age_range,
                           age = age_range, 
                           year = year_range, 
                           term_year = term_year_range, 
                           retire_year = retire_year_range)
  
  wf_active <- array(0, dim = active_dim, dimnames = active_dim_names)
  wf_term <- array(0, dim = term_dim, dimnames = term_dim_names)
  wf_refund <- wf_term
  wf_retire <- array(0, dim = retire_dim, dimnames = retire_dim_names)
  
  # Initial active population ----
  active_int_df <- expand_grid(entry_age = entry_age_range, 
                               age = age_range) %>%
    left_join(salary_headcount_table, by = c("entry_age", "age")) %>%
    replace(is.na(.), 0) %>%
    select(entry_age, age, count)
  
  active_int_matrix <- xtabs(count ~ entry_age + age, active_int_df) # should we store this as a sparse array?
  
  wf_active[,,1] <- active_int_matrix # djb: wf_active dimensions are entry_age x age x year -- so fill in first year (2022) wf_active[,,"2022"]
  
  # Create probability arrays ----
  
  #.. Mortality probability array (4 dimensions: entry_age, age, year, term_year) ----
  mort_df_term <- expand_grid(entry_age = entry_age_range,
                              age = age_range, 
                              year = year_range, 
                              term_year = term_year_range) %>% 
    left_join(mort_table, by = c("entry_age", "age" = "dist_age", "year" = "dist_year", "term_year")) %>% 
    mutate(mort = if_else(is.na(mort_final), 0, mort_final))
  
  mort_array_term <- xtabs(mort ~ entry_age + age + year + term_year, mort_df_term)
  
  #.. Separation probability array (3 dimensions: entry_age, age, year) ----
  sep_df <- expand_grid(entry_age = entry_age_range,
                        age = age_range, 
                        year = year_range) %>% 
    mutate(entry_year = year - (age - entry_age)) %>% 
    left_join(separation_rate_table, by = c("entry_age", "age" = "term_age", "entry_year")) %>% 
    select(entry_age, age, year, separation_rate) %>% 
    mutate(separation_rate = if_else(is.na(separation_rate), 0, separation_rate))
  
  sep_array <- xtabs(separation_rate ~ entry_age + age + year, sep_df)
  
  #.. Refund and retirement probability arrays ----
  
  optimal_retire <- benefit_val_table %>% 
    # rename(term_age = Age) %>% 
    select(entry_year, entry_age, term_age, yos, dist_age, ben_decision) %>% 
    mutate(refund = case_when(ben_decision == "refund" ~ 1,     # use case_when instead of ifelse to handle NA values better
                              ben_decision == "mix" ~ 1 - retire_refund_ratio,
                              .default = 0),
           retire = case_when(ben_decision == "retire" ~ 1,
                              ben_decision == "mix" ~ 1,
                              .default = 0),
           refund_age = term_age)
  
  #.... Retire probability array (4 dimensions: entry_age, age, year, term_year) ----
  retire_df <- expand_grid(entry_age = entry_age_range,
                           age = age_range, 
                           year = year_range, 
                           term_year = term_year_range) %>% 
    mutate(
      entry_year = year - (age - entry_age),
      term_age = age - (year - term_year),
      yos = term_age - entry_age) %>% 
    filter(year - term_year >= 0, yos >= 0) %>% 
    left_join(optimal_retire, by = c("entry_age",
                                     "age" = "dist_age",
                                     "entry_year",
                                     "term_age",
                                     "yos")) %>% 
    mutate(retire = if_else(is.na(retire), 0, retire))
  
  retire_array <- xtabs(retire ~ entry_age + age + year + term_year, retire_df) 
  
  #.... Refund probability array (4 dimensions: entry_age, age, year, term_year) ----
  # Note that employees get refunds in the same year they get terminated.
  refund_df <- expand_grid(entry_age = entry_age_range,
                           age = age_range, 
                           year = year_range, 
                           term_year = term_year_range) %>% 
    mutate(
      entry_year = year - (age - entry_age),
      term_age = age - (year - term_year),
      yos = term_age - entry_age
    ) %>% 
    filter(year - term_year >= 0, yos >= 0) %>% 
    left_join(optimal_retire, by = c("entry_age",
                                     "age" = "refund_age",
                                     "entry_year",
                                     "term_age",
                                     "yos")) %>% 
    mutate(refund = if_else(is.na(refund), 0, refund))
  # djb: is there any reason we shouldn't combine retire_df and refund_df? same structure and methods; wide or stacked
  
  refund_array <- xtabs(refund ~ entry_age + age + year + term_year, refund_df)
  
  return(list(wf_active=wf_active,
              wf_term=wf_term, 
              wf_refund=wf_refund, 
              wf_retire=wf_retire,
              mort_array_term=mort_array_term,
              sep_array=sep_array,
              retire_array=retire_array,
              refund_array=refund_array))
}

init_list <- initialize_arrays(
  age_range,
  entry_age_range,
  year_range,
  term_year_range,
  retire_year_range,
  salary_headcount_table,
  mort_table,
  separation_rate_table,
  benefit_val_table,
  retire_refund_ratio = params$retire_refund_ratio_
)


initialize_df <- function(
    age_range,
    entry_age_range,
    year_range,
    term_year_range,
    retire_year_range,
    salary_headcount_table,
    mort_table,
    separation_rate_table,
    benefit_val_table,
    retire_refund_ratio
) {
  # Initial active population
  wf_active_df <- expand_grid(entry_age = entry_age_range,
                              age = age_range) %>%
    left_join(salary_headcount_table, by = c("entry_age", "age")) %>%
    mutate(count = replace_na(count, 0)) %>%
    select(entry_age, age, count)
  
  # Terminated and refunded structure: will be filled over time
  wf_term_df <- wf_active_df %>% mutate(count = 0) %>% select(entry_age, age, count)
  wf_refund_df <- wf_term_df
  
  # Retired structure
  wf_retire_df <- expand_grid(entry_age = entry_age_range,
                              age = age_range,
                              term_year = term_year_range,
                              retire_year = retire_year_range) %>%
    mutate(count = 0)
  
  # Mortality probabilities
  mort_df_term <- expand_grid(entry_age = entry_age_range,
                              age = age_range,
                              year = year_range,
                              term_year = term_year_range) %>%
    left_join(mort_table, by = c("entry_age", "age" = "dist_age", "year" = "dist_year", "term_year")) %>%
    mutate(mort_rate = replace_na(mort_final, 0)) %>%
    select(entry_age, age, year, term_year, mort_rate)
  
  # Separation probabilities
  sep_df <- expand_grid(entry_age = entry_age_range,
                        age = age_range,
                        year = year_range) %>%
    mutate(entry_year = year - (age - entry_age)) %>%
    left_join(separation_rate_table, by = c("entry_age", "age" = "term_age", "entry_year")) %>%
    mutate(sep_rate = replace_na(separation_rate, 0)) %>%
    select(entry_age, age, year, sep_rate)
  
  # Optimal retirement and refund setup
  optimal_retire <- benefit_val_table %>%
    select(entry_year, entry_age, term_age, yos, dist_age, ben_decision) %>%
    mutate(refund = case_when(ben_decision == "refund" ~ 1,
                              ben_decision == "mix" ~ 1 - retire_refund_ratio,
                              TRUE ~ 0),
           retire = case_when(ben_decision == "retire" ~ 1,
                              ben_decision == "mix" ~ 1,
                              TRUE ~ 0),
           refund_age = term_age)
  
  # Retirement probabilities
  retire_df <- expand_grid(entry_age = entry_age_range,
                           age = age_range,
                           year = year_range,
                           term_year = term_year_range) %>%
    mutate(entry_year = year - (age - entry_age),
           term_age = age - (year - term_year),
           yos = term_age - entry_age) %>%
    filter(year - term_year >= 0, yos >= 0) %>%
    left_join(optimal_retire, by = c("entry_age", "age" = "dist_age", "entry_year", "term_age", "yos")) %>%
    mutate(retire_rate = replace_na(retire, 0)) %>%
    select(entry_age, age, year, term_year, retire_rate)
  
  # Refund probabilities
  refund_df <- expand_grid(entry_age = entry_age_range,
                           age = age_range,
                           year = year_range,
                           term_year = term_year_range) %>%
    mutate(entry_year = year - (age - entry_age),
           term_age = age - (year - term_year),
           yos = term_age - entry_age) %>%
    filter(year - term_year >= 0, yos >= 0) %>%
    left_join(optimal_retire, by = c("entry_age", "age" = "refund_age", "entry_year", "term_age", "yos")) %>%
    mutate(refund_rate = replace_na(refund, 0)) %>%
    select(entry_age, age, year, term_year, refund_rate)
  
  list(
    wf_active = wf_active_df,
    wf_term = wf_term_df,
    wf_refund = wf_refund_df,
    wf_retire = wf_retire_df,
    mort_df_term = mort_df_term,
    sep_df = sep_df,
    retire_df = retire_df,
    refund_df = refund_df
  )
}

init_list_df <- initialize_df(
  age_range,
  entry_age_range,
  year_range,
  term_year_range,
  retire_year_range,
  salary_headcount_table,
  mort_table,
  separation_rate_table,
  benefit_val_table,
  retire_refund_ratio = params$retire_refund_ratio_
)

list2env(init_list, envir = environment())

init_list$sep_array
init_list_df$sep_df %>% filter(year == 2022)

# Loop through arrays ----

loop_through_arrays <- function(wf_active,
                                wf_term, 
                                wf_refund, 
                                wf_retire,
                                mort_array_term,
                                sep_array,
                                retire_array,
                                refund_array,
                                age_range,
                                entry_age_range,
                                year_range,
                                entrant_profile_table,
                                pop_growth){
  
  # calculate dims - one less thing to pass to the function
  term_dim <- dim(wf_term)
  retire_dim <- dim(wf_retire)
  
  # Position matrix to add new hires
  position_matrix <- expand_grid(entry_age = entry_age_range, age = age_range) %>% 
    mutate(new = if_else(entry_age == age, 1, 0)) # djb: only 11 cells of 1,133 have 1
  
  position_matrix <- xtabs(new ~ entry_age + age, position_matrix) # djb: has 0 or 1; note xtab can create sparse matrices
  
  # transition matrix to shift the population to the right by 1 age after 1 year
  TM <-  diag(length(age_range) + 1)[-1, -(length(age_range) + 1)] 
  
  # possible array dimensions: entry_age, age, year, term_year, retire_year
  # Workforce projection loop ----
  for (i in 2:length(year_range)) {
    
    # calculate the # of newly terminated actives. 2-dimensional array
    active2term <- wf_active[,,i-1] * sep_array[,,i-1]   
    
    # deduct terminated members from the active workforce and shift the wf_active matrix to the right by one year
    wf_active[,,i] <- (wf_active[,,i-1] - active2term) %*% TM  
    
    # new entrants matrix to be added to the active workforce
    new_entrants <- pentools::add_new_entrants(g = pop_growth, # GLOBAL
                                               ne_dist = entrant_profile_table$entrant_dist, 
                                               wf1 = wf_active[,,i-1],
                                               wf2 = wf_active[,,i], 
                                               ea = entry_age_range, 
                                               age = age_range,
                                               position_matrix = position_matrix)
    
    wf_active[,,i] = wf_active[,,i] + new_entrants  # add new entrants
    
    term2death <- wf_term[,,i-1,] * mort_array_term[,,i-1,] # 3-dimensional array entry_age, age, year
    
    wf_term[,,i,] <- apply(wf_term[,,i-1,] - term2death, 3, function(x) x %*% TM) %>% array(term_dim[-3]) 
    
    wf_term[,,i,i] <- active2term %*% TM   # add newly terminated members to the term population
    
    # calculate the # of newly refunded members. 2-dimensional array: entry_age, age
    term2refund <- wf_term[,,i,i] * refund_array[,,i,i]  
    
    wf_term[,,i,i] <- wf_term[,,i,i] - term2refund # update wf_term
    wf_refund[,,i,i] <- term2refund # update wf_refund
    
    # calculate the # of newly retired members. 3-dimensional array: entry_age, age, ., term_year
    term2retire <- wf_term[,,i,] * retire_array[,,i,]  
    
    wf_term[,,i,] <- wf_term[,,i,] - term2retire # update wf_term
    
    # 4-dimensional array: entry_age, age, year, term_year # apply over each term_year??
    retire2death <- apply(wf_retire[,,i-1,,],
                          4, # djb: looks like term_year, should be able to use "term_year" -- check
                          function(x) x * mort_array_term[,,i-1,]) %>% array(retire_dim[-3])
    
    # update wf_retire for this year 5 dimensional: entry_age, age, year, term_year, retire_year
    wf_retire[,,i,,] <- apply(wf_retire[,,i-1,,] - retire2death, c(3,4), function(x) x %*% TM) %>% array(retire_dim[-3])
    wf_retire[,,i,,i] <- term2retire
  } # end loop
  
  return(list(wf_active=wf_active,
              wf_term=wf_term, 
              wf_refund=wf_refund, 
              wf_retire=wf_retire))
}

array_list <- loop_through_arrays(
  wf_active,
  wf_term, 
  wf_refund, 
  wf_retire,
  mort_array_term,
  sep_array,
  retire_array,
  refund_array,
  age_range,
  entry_age_range,
  year_range,
  entrant_profile_table,
  pop_growth=params$pop_growth_)

# Loop through dataframes ----

loop_through_df_full <- function(wf_active_df,
                                 wf_term_df,
                                 wf_refund_df,
                                 wf_retire_df,
                                 sep_df,
                                 refund_df,
                                 retire_df,
                                 mort_retire_df,
                                 age_range,
                                 entry_age_range,
                                 year_range,
                                 entrant_profile_df,
                                 pop_growth) {
  
  for (i in 2:length(year_range)) {
    this_year <- year_range[i]
    prev_year <- year_range[i - 1]
    
    # --- 1. Active → Terminated (and update active) ---
    active_prev <- wf_active_df %>%
      filter(year == prev_year) %>%
      left_join(sep_df, by = c("entry_age", "age", "year")) %>%
      mutate(sep_rate = replace_na(sep_rate, 0),
             term_count = count * sep_rate,
             remaining_count = count - term_count,
             year = this_year,
             age = age + 1) %>%
      select(entry_age, age, year, count = remaining_count)
    
    # --- 2. Add New Entrants ---
    new_entrants_df <- entrant_profile_df %>%
      mutate(year = this_year,
             age = entry_age,
             count = entrant_dist * pop_growth) %>%
      select(entry_age, age, year, count)
    
    wf_active_df <- bind_rows(wf_active_df, active_prev, new_entrants_df) %>%
      group_by(entry_age, age, year) %>%
      summarise(count = sum(count), .groups = "drop")
    
    # --- 3. Update Terminated ---
    term_add_df <- wf_active_df %>%
      filter(year == this_year, entry_age == age - 1) %>%
      left_join(sep_df %>% filter(year == prev_year), 
                by = c("entry_age", "age" = "age", "year" = "year")) %>%
      mutate(sep_rate = replace_na(sep_rate, 0),
             term_count = count * sep_rate,
             term_year = this_year) %>%
      transmute(entry_age, age, term_year, count = term_count)
    
    wf_term_df <- bind_rows(wf_term_df, term_add_df) %>%
      group_by(entry_age, age, term_year) %>%
      summarise(count = sum(count), .groups = "drop")
    
    # --- 4. Update Refunds ---
    refund_add_df <- term_add_df %>%
      left_join(refund_df %>% filter(year == this_year), 
                by = c("entry_age", "age", "term_year" = "year")) %>%
      mutate(refund_rate = replace_na(refund_rate, 0),
             count = count * refund_rate) %>%
      select(entry_age, age, term_year, count)
    
    wf_refund_df <- bind_rows(wf_refund_df, refund_add_df) %>%
      group_by(entry_age, age, term_year) %>%
      summarise(count = sum(count), .groups = "drop")
    
    # --- 5. Update Retired ---
    term_remaining_df <- term_add_df %>%
      left_join(refund_add_df, by = c("entry_age", "age", "term_year")) %>%
      mutate(count = count.x - replace_na(count.y, 0)) %>%
      select(entry_age, age, term_year, count)
    
    retire_add_df <- term_remaining_df %>%
      left_join(retire_df %>% filter(year == this_year), 
                by = c("entry_age", "age", "term_year" = "year")) %>%
      mutate(retire_rate = replace_na(retire_rate, 0),
             count = count * retire_rate,
             retire_year = this_year) %>%
      select(entry_age, age, term_year, retire_year, count)
    
    wf_retire_df <- bind_rows(wf_retire_df, retire_add_df) %>%
      group_by(entry_age, age, term_year, retire_year) %>%
      summarise(count = sum(count), .groups = "drop")
    
    # --- 6. Apply Mortality on Retirees ---
    wf_retire_df <- wf_retire_df %>%
      left_join(mort_retire_df %>% filter(year == this_year),
                by = c("entry_age", "age", "retire_year" = "year")) %>%
      mutate(mort_rate = replace_na(mort_rate, 0),
             count = count * (1 - mort_rate)) %>%
      select(entry_age, age, term_year, retire_year, count)
  }
  
  list(
    wf_active = wf_active_df,
    wf_term = wf_term_df,
    wf_refund = wf_refund_df,
    wf_retire = wf_retire_df
  )
}

df_list <- loop_through_df(
  wf_active,
  wf_term,
  wf_refund,
  wf_retire,
  mort_df_term,
  sep_df,
  retire_df,
  refund_df,
  age_range,
  entry_age_range,
  year_range,
  entrant_profile_table,
  pop_growth = params$pop_growth_
)


