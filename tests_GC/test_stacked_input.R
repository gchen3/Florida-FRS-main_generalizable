
entrant_profile_table_s = frs_data_env$entrant_profile_table
salary_headcount_table_s = frs_data_env$salary_headcount_table
salary_headcount_table_smort_table_S = frs_data_env$mort_table
mort_retire_table_S = frs_data_env$mort_retire_table
separation_rate_table_s = frs_data_env$separation_rate_table


# entrant_profile ---------------------------------------------------------

entrant_profile_table_s %>% filter(employee_class == "regular") %>%
  left_join(entrant_profile_table, by = c("entry_age", "start_sal")) %>%
  mutate(mismatch = ifelse(entrant_dist.x != entrant_dist.y, TRUE, FALSE)) %>%
  filter(mismatch == 1)

entrant_profile_table_s %>% filter(employee_class == "regular") %>%
  left_join(frs_data_env$regular_entrant_profile_table, by = c("entry_age", "start_sal")) %>%
  mutate(mismatch = ifelse(entrant_dist.x != entrant_dist.y, TRUE, FALSE)) %>%
  filter(mismatch == 1)

# salary_headcount --------------------------------------------------------

salary_headcount_table_s %>% filter(employee_class == "regular") %>%
  left_join(frs_data_env$regular_salary_headcount_table, by = c("entry_year", "entry_age", "age", "yos")) %>%
  mutate(mismatch = ifelse(count.x != count.y, TRUE, FALSE)) %>%
  mutate(mismatch = ifelse(entry_salary.x != entry_salary.y, TRUE, FALSE)) %>%
  filter(mismatch == 1)

salary_headcount_table_s %>% filter(employee_class == "eso") %>%
  left_join(frs_data_env$eso_salary_headcount_table, by = c("entry_year", "entry_age", "age", "yos")) %>%
  mutate(mismatch = ifelse(count.x != count.y, TRUE, FALSE)) %>%
  mutate(mismatch = ifelse(entry_salary.x != entry_salary.y, TRUE, FALSE)) %>%
  filter(mismatch == 1)


# mort --------------------------------------------------------------------
mort_table_s = frs_data_env$mort_table
mort_table_S %>% filter(employee_class == "regular") %>%
  left_join(frs_data_env$regular_mort_table, by = c("entry_year", "entry_age", "dist_year", "dist_age", "yos", "term_year", "tier_at_dist_age")) %>%
  mutate(mismatch = ifelse(mort_final.x != mort_final.y, TRUE, FALSE)) %>%
  filter(mismatch == 1)

mort_table_S %>% filter(employee_class == "eso") %>%
  left_join(frs_data_env$eso_mort_table, by = c("entry_year", "entry_age", "dist_year", "dist_age", "yos", "term_year", "tier_at_dist_age")) %>%
  mutate(mismatch = ifelse(mort_final.x != mort_final.y, TRUE, FALSE)) %>%
  filter(mismatch == 1)

# mort_retire -------------------------------------------------------------
mort_retire_table_s = frs_data_env$mort_retire_table
mort_retire_table_s %>% filter(employee_class == "regular") %>%
  left_join(frs_data_env$regular_mort_retire_table, by = c("base_age", "age", "year")) %>%
  mutate(mismatch = ifelse(mort_final.x != mort_final.y, TRUE, FALSE)) %>%
  filter(mismatch == 1)

mort_retire_table_s %>% filter(employee_class == "eso") %>%
  left_join(frs_data_env$eso_mort_retire_table, by = c("base_age", "age", "year")) %>%
  mutate(mismatch = ifelse(mort_final.x != mort_final.y, TRUE, FALSE)) %>%
  filter(mismatch == 1)

# separation_rate --------------------------------------------------------
separation_rate_table_s = frs_data_env$separation_rate_table
separation_rate_table_s %>% filter(employee_class == "regular") %>%
  left_join(frs_data_env$regular_separation_rate_table, by = c("entry_year", "entry_age", "term_age", "yos", "term_year")) %>%
  mutate(mismatch = ifelse(separation_rate.x != separation_rate.y, TRUE, FALSE),
         mismatch_2 = ifelse(remaining_prob.x != remaining_prob.y, TRUE, FALSE),
         mismatch_3 = ifelse(separation_prob.x != separation_prob.y, TRUE, FALSE)) %>%
  filter(mismatch == 1 | mismatch_2 == 1 | mismatch_3 == 1)

separation_rate_table_s %>% filter(employee_class == "eso") %>%
  left_join(frs_data_env$eso_separation_rate_table, by = c("entry_year", "entry_age", "term_age", "yos", "term_year")) %>%
  mutate(mismatch = ifelse(separation_rate.x != separation_rate.y, TRUE, FALSE),
         mismatch_2 = ifelse(remaining_prob.x != remaining_prob.y, TRUE, FALSE),
         mismatch_3 = ifelse(separation_prob.x != separation_prob.y, TRUE, FALSE)) %>%
  filter(mismatch == 1 | mismatch_2 == 1 | mismatch_3 == 1)  



# test salary_benefit_table ------------------------------------------
get_salary_benefit_table <- function(class_name,
                                     entrant_profile_table,
                                     class_salary_growth_table,
                                     salary_headcount_table,
                                     params){
  #Create a long-form table of entry year, entry age, and yos and merge with salary data
  #Note that "age" in the salary_table is active age
  # entry_age = entrant_profile_table$entry_age
  salary_benefit_table <- expand_grid(entry_year = params$entry_year_range_, 
                                      entry_age = entrant_profile_table$entry_age, 
                                      yos = params$yos_range_) %>% 
    mutate(
      term_age = entry_age + yos#,
      # term_year = entry_year + yos,
      #tier_at_term_age = frs_data_env$get_tier(class_name, entry_year, term_age, yos, params$new_year_)
    ) %>% 
    left_join(frs_data_env$tier_table %>% filter(class == class_name) , by = c("entry_year", "yos", "term_age"= "age")) %>%
    mutate(tier_at_term_age = tier) %>%
    filter(term_age <= params$max_age_) %>% 
    arrange(entry_year, entry_age, yos) %>% 
    left_join(entrant_profile_table, by = "entry_age") %>% 
    left_join(class_salary_growth_table, by = "yos") %>%
    #Join salary_head_count_table by entry_year and entry_age only to get historical entry_salary
    left_join(salary_headcount_table %>% select(entry_year, entry_age, entry_salary), 
              by = c("entry_year", "entry_age")) %>%
    mutate(
      salary = if_else(entry_year <= max(salary_headcount_table$entry_year), 
                       entry_salary * cumprod_salary_increase,
                       start_sal * cumprod_salary_increase * (1 + params$payroll_growth_)^(entry_year - max(salary_headcount_table$entry_year)))
      # ,
      # fas_period = if_else(str_detect(tier_at_term_age, "tier_1"), 5, 8)
    ) %>% 
    left_join(frs_data_env$fas_period_lookup, by = c("tier_at_term_age")) %>%
    group_by(entry_year, entry_age) %>%
    mutate(
      # fas = baseR.rollmean(salary, fas_period),
      # code below is much faster than baseR.rollmean
      fas = RcppRoll::roll_mean(c(NA, salary[-length(salary)]), # drop the current value
                                n = max(fas_period), align="right", fill = NA),
      db_ee_cont = params$db_ee_cont_rate_ * salary,
      db_ee_balance = pentools::get_cum_fv(params$db_ee_interest_rate_, db_ee_cont),
    ) %>%
    ungroup() %>%
    filter(!is.na(salary)) %>%
    distinct()

  return(salary_benefit_table)
}

class_name = "regular"
entrant_profile_table = frs_data_env$regular_entrant_profile_table
salary_headcount_table = frs_data_env$regular_salary_headcount_table
salary_headcount_table_smort_table = frs_data_env$regular_mort_table
mort_retire_table = frs_data_env$regular_mort_retire_table
separation_rate_table = frs_data_env$regular_separation_rate_table
class_salary_growth_table = frs_data_env$salary_growth_table %>% 
  filter(class == "regular") %>%
  select(yos, cumprod_salary_increase)

salary_benefit_table <- get_salary_benefit_table(class_name,
                                                 entrant_profile_table,
                                                 class_salary_growth_table,
                                                 salary_headcount_table,
                                                 params)

salary_benefit_table


get_salary_benefit_table_s <- function(entrant_profile_table,
                                     class_salary_growth_table,
                                     salary_headcount_table,
                                     params){
  
  salary_benefit_table <- expand_grid(entry_year = params$entry_year_range_, 
                                      entry_age = entrant_profile_table$entry_age, 
                                      yos = params$yos_range_,
                                      class = params$class_names_no_drop_frs_) %>% 
    mutate(
      term_age = entry_age + yos) %>%
    left_join(frs_data_env$tier_table, by = c("entry_year", "yos", "term_age"= "age", "class")) %>%
    mutate(tier_at_term_age = tier) %>%
    filter(term_age <= params$max_age_) %>% 
    arrange(entry_year, entry_age, yos) %>% 
    left_join(frs_data_env$entrant_profile_table, by = c("entry_age", "class" = "employee_class")) %>%
    left_join(frs_data_env$salary_growth_table, by = c("yos", "class")) %>%
    left_join(frs_data_env$salary_headcount_table %>% select(entry_year, entry_age, entry_salary, employee_class), 
              by = c("entry_year", "entry_age", "class" = "employee_class")) %>%
    mutate(
      salary = if_else(entry_year <= max(salary_headcount_table$entry_year), 
                       entry_salary * cumprod_salary_increase,
                       start_sal * cumprod_salary_increase * (1 + params$payroll_growth_)^(entry_year - max(salary_headcount_table$entry_year)))
    ) %>% 
    left_join(frs_data_env$fas_period_lookup, by = c("tier_at_term_age")) %>%
    distinct() %>%
    group_by(class, entry_year, entry_age) %>%
    mutate(
      fas = RcppRoll::roll_mean(c(NA, salary[-length(salary)]), # drop the current value
                                n = max(fas_period), align="right", fill = NA),
      db_ee_cont = params$db_ee_cont_rate_ * salary,
      db_ee_balance = pentools::get_cum_fv(params$db_ee_interest_rate_, db_ee_cont),
    ) %>%
    ungroup() %>%
    filter(!is.na(salary))
  
  return(salary_benefit_table)
}

entrant_profile_table = frs_data_env$entrant_profile_table
salary_headcount_table = frs_data_env$salary_headcount_table
mort_table = frs_data_env$mort_table
mort_retire_table = frs_data_env$mort_retire_table
separation_rate_table = frs_data_env$separation_rate_table
params = params

salary_benefit_table_s <- get_salary_benefit_table_s(entrant_profile_table,
                                                 class_salary_growth_table,
                                                 salary_headcount_table,
                                                 params) 

salary_benefit_table_s 
salary_benefit_table

salary_benefit_table %>%
  anti_join(salary_benefit_table_s)

salary_benefit_table_s %>%
  anti_join(salary_benefit_table)

salary_benefit_table %>% filter((entry_year == 2000) & (entry_age == 20) & (class == "regular"))
salary_benefit_table_s %>% filter((entry_year == 2000) & (entry_age == 20) & (class == "regular"))

salary_benefit_table_s %>% filter((entry_year == 2000) & (entry_age == 20) & (yos == 40) & (term_age == 60) & (class == "regular"))
salary_benefit_table_s %>% filter((entry_year == 2000) & (entry_age == 20) & (yos == 40) & (term_age == 60) & (class == "regular"))


# get_ann_table -----------------------------------------------------------
entrant_profile_table_s = frs_data_env$entrant_profile_table
salary_headcount_table_s = frs_data_env$salary_headcount_table
mort_table_s = frs_data_env$mort_table
mort_retire_table_s = frs_data_env$mort_retire_table
separation_rate_table_s = frs_data_env$separation_rate_table


get_annuity_factor_table_s <- function(
    mort_table,
    salary_benefit_table,
    params
) {
  ann_factor_table <- mort_table %>%
    semi_join(salary_benefit_table, by = c("entry_year", "entry_age", "employee_class" = "class")) %>%
    left_join(frs_data_env$dr_lookup, by = c("tier_at_dist_age")) %>%
    left_join(frs_data_env$cola_lookup, 
              by = c("tier_at_dist_age", "entry_year", "yos")) %>%
    group_by(entry_year, entry_age, yos) %>% 
    mutate(
      cum_dr = cumprod(1 + lag(dr, default = 0)),
      cum_mort = cumprod(1 - lag(mort_final, default = 0)),
      cum_cola = cumprod(1 + lag(cola, default = 0)),
      cum_mort_dr = cum_mort / cum_dr,
      cum_mort_dr_cola = cum_mort_dr * cum_cola,
      # ann_factor below is the annuity factor at distribution (retirement) age
      ann_factor = rev(cumsum(rev(cum_mort_dr_cola))) / cum_mort_dr_cola
    ) %>% 
    ungroup()
  
  return(ann_factor_table)
}

ann_factor_table_s <- get_annuity_factor_table_s(
  mort_table_s,
  salary_benefit_table_s,
  params)

get_annuity_factor_table <- function(
    mort_table,
    salary_benefit_table,
    params
) {
  # Survival Probability and Annuity Factor for active members
  ann_factor_table <- mort_table %>% 
    #Semi join the salary_benefit_able to reduce the size of the data that needs to be calculated
    semi_join(salary_benefit_table, by = c("entry_year", "entry_age")) %>%
    left_join(frs_data_env$dr_lookup, by = c("tier_at_dist_age")) %>%
    left_join(frs_data_env$cola_lookup, 
              by = c("tier_at_dist_age", "entry_year", "yos")) %>%
    group_by(entry_year, entry_age, yos) %>% 
    mutate(
      cum_dr = cumprod(1 + lag(dr, default = 0)),
      cum_mort = cumprod(1 - lag(mort_final, default = 0)),
      cum_cola = cumprod(1 + lag(cola, default = 0)),
      cum_mort_dr = cum_mort / cum_dr,
      cum_mort_dr_cola = cum_mort_dr * cum_cola,
      # ann_factor below is the annuity factor at distribution (retirement) age
      ann_factor = rev(cumsum(rev(cum_mort_dr_cola))) / cum_mort_dr_cola
    ) %>% 
    ungroup()
  
  return(ann_factor_table)
}

entrant_profile_table = frs_data_env$regular_entrant_profile_table
salary_headcount_table = frs_data_env$regular_salary_headcount_table
mort_table = frs_data_env$regular_mort_table
mort_retire_table = frs_data_env$regular_mort_retire_table
separation_rate_table = frs_data_env$regular_separation_rate_table
params = params

ann_factor_table <- get_annuity_factor_table(
  mort_table,
  salary_benefit_table,
  params)

ann_factor_table_s %>% filter(employee_class == "regular") %>% select(-employee_class) %>% 
  anti_join(ann_factor_table)


# get benefit table -------------------------------------------------------
get_benefit_table <- function(class_name,
                              ann_factor_table, 
                              salary_benefit_table,
                              params){
  benefit_table <- ann_factor_table %>%
    mutate(
      term_age = entry_age + yos, .before = term_year,
      class_name = class_name,
      is_norm_retire_elig = str_detect(tier_at_dist_age, "norm")
    ) %>%
    # dist_age is distribution age, and dist_year is distribution year.
    # distribution age means the age when the member starts to accept benefits (either a refund or a pension)
    left_join(salary_benefit_table,
              by = c("entry_year", "entry_age", "yos", "term_age")) %>%
    left_join(frs_data_env$ben_mult_lookup %>% filter(class_name == !!class_name) %>% select(-system),
              by = join_by(class_name, 
                           tier_at_dist_age,
                           dist_age >= dist_age_min_ge,
                           dist_age < dist_age_max_lt,
                           yos >= yos_min_ge,
                           yos < yos_max_lt,
                           dist_year >= dist_year_min_ge,
                           dist_year < dist_year_max_lt)) %>%
    left_join(frs_data_env$reduce_factor_lookup %>% filter(class_name == !!class_name),
              by = c("tier_at_dist_age", "dist_age")) %>%
    mutate(db_benefit = yos * ben_mult * fas * reduce_factor,
           
           #cal_factor is a calibration factor added to match the normal cost from the val report
           db_benefit = db_benefit * params$cal_factor_,
           
           #calculate the annuity factor at termination day
           ann_factor_term = ann_factor * cum_mort_dr,
           
           #calculate the actuarial present value of future DB benefits at termination day (discount the annual DB benefits back to termination day)
           pvfb_db_at_term_age = db_benefit * ann_factor_term
           
    )
  return(benefit_table)  
}

class_name = "regular"

benefit_table <- get_benefit_table(
  class_name,
  ann_factor_table,
  salary_benefit_table,
  params)


get_benefit_table_s <- function(ann_factor_table, 
                              salary_benefit_table,
                              params){
  
  benefit_table <- ann_factor_table %>%
    mutate(
      term_age = entry_age + yos, .before = term_year
    ) %>%
    # dist_age is distribution age, and dist_year is distribution year.
    # distribution age means the age when the member starts to accept benefits (either a refund or a pension)
    left_join(salary_benefit_table,
              by = c("entry_year", "entry_age", "yos", "term_age", "employee_class" = "class")) %>%
     left_join(frs_data_env$ben_mult_lookup %>% select(-system),
            by = join_by("employee_class" == "class_name", 
                         tier_at_dist_age,
                         dist_age >= dist_age_min_ge,
                         dist_age < dist_age_max_lt,
                         yos >= yos_min_ge,
                         yos < yos_max_lt,
                         dist_year >= dist_year_min_ge,
                         dist_year < dist_year_max_lt)) %>%
    left_join(frs_data_env$reduce_factor_lookup,
              by = c("tier_at_dist_age", "dist_age", "employee_class" = "class_name")) %>%
    mutate(db_benefit = yos * ben_mult * fas * reduce_factor,
           
           #cal_factor is a calibration factor added to match the normal cost from the val report
           db_benefit = db_benefit * params$cal_factor_,
           
           #calculate the annuity factor at termination day
           ann_factor_term = ann_factor * cum_mort_dr,
           
           #calculate the actuarial present value of future DB benefits at termination day (discount the annual DB benefits back to termination day)
           pvfb_db_at_term_age = db_benefit * ann_factor_term
           
    )
  return(benefit_table)  
}

benefit_table_s <- get_benefit_table_s(
  ann_factor_table_s,
  salary_benefit_table_s,
  params)

benefit_table_s %>% filter(employee_class == "regular") %>% select(-employee_class) %>%
  anti_join(benefit_table)

benefit_table_s
benefit_table
