# ================================================================
# FRS benefit adapter (S-compatible)
 # Wires FRS tables to the generic benefit_core engine.
 # Preserves outputs and structure of existing S implementation.
 # ================================================================

# ---- Core is expected to be sourced into the same env before this file

# Re-implement selected functions using core helpers while keeping names

# ---- FRS rule functions (pluggable policy layer)

dist_age_rule <- function(term_status, earliest_norm_age, term_age) {
  vested <- stringr::str_detect(term_status, "vested") & !stringr::str_detect(term_status, "non_vested")
  ifelse(vested, earliest_norm_age, term_age)
}

decision_rule <- function(sep_type, yos) {
  # Vectorized decision: NA for yos==0; retire>mix>refund otherwise
  out <- rep(NA_character_, length(yos))
  nonzero <- !(is.na(yos) | yos == 0)
  out[nonzero & sep_type == "retire"] <- "retire"
  out[nonzero & sep_type == "vested"] <- "mix"
  out[nonzero & !(sep_type %in% c("retire","vested"))] <- "refund"
  out
}

pv_blend_rule <- function(sep_type, pvfb_db_at_term_age, db_ee_balance, retire_refund_ratio) {
  out <- db_ee_balance
  out[sep_type == "retire"] <- pvfb_db_at_term_age[sep_type == "retire"]
  mix_idx <- sep_type == "vested"
  out[mix_idx] <- retire_refund_ratio * pvfb_db_at_term_age[mix_idx] + (1 - retire_refund_ratio) * db_ee_balance[mix_idx]
  out
}

get_agg_norm_cost_table_s <- function(
    indv_norm_cost_table_s,
    salary_headcount_table_s,
    salary_benefit_table_s){
  benefit_aggregate_normal_cost(indv_norm_cost_table_s,
                                salary_headcount_table_s,
                                salary_benefit_table_s,
                                group_cols = c("class"))
}

get_annuity_factor_retire_table_s <- function(
    mort_retire_table_s,
    params) {
  # Build dr/cola per-row to pass to core
  ann_factor_retire_table_s <- mort_retire_table_s %>% 
    dplyr::mutate(
      dr = params$dr_current_,
      cola_type = dplyr::if_else(params$one_time_cola_ == TRUE, "one_time", "normal"),
      cola = dplyr::if_else(cola_type == "one_time", 
                            dplyr::if_else(year == params$new_year_, params$cola_current_retire_one_, 0),
                            params$cola_current_retire_)
    ) %>% 
    benefit_annuity_factors_retire(
      group_cols = c("class", "base_age"),
      mort_col   = "mort_final",
      dr_col     = "dr",
      cola_col   = "cola",
      one_time_cola = params$one_time_cola_,
      out_ann_col = "ann_factor_retire"
    )
  ann_factor_retire_table_s
}

get_annuity_factor_table_s <- function(
    mort_table_s,
    salary_benefit_table_s,
    params
) {
  # Attach dr and cola via FRS lookups; then let core compute annuity series
  ann_factor_table_s <- mort_table_s %>%
    dplyr::semi_join(salary_benefit_table_s, by = c("entry_year", "entry_age", "class")) %>%
    dplyr::left_join(frs_data_env$dr_lookup, by = c("tier_at_dist_age")) %>%
    dplyr::left_join(frs_data_env$cola_lookup, 
                     by = c("tier_at_dist_age", "entry_year", "yos")) %>%
    benefit_annuity_factors(
      group_cols = c("class", "entry_year", "entry_age", "yos"),
      mort_col   = "mort_final",
      dr_col     = "dr",
      cola_col   = "cola",
      out_ann_col = "ann_factor"
    )
  ann_factor_table_s
}

get_benefit_table_s <- function(ann_factor_table_s, 
                                salary_benefit_table_s,
                                params){
  benefit_table_s <- ann_factor_table_s %>%
    dplyr::mutate(
      term_age = entry_age + yos, .before = term_year
    ) %>%
    dplyr::left_join(salary_benefit_table_s,
              by = c("entry_year", "entry_age", "yos", "term_age", "class")) %>%
    dplyr::left_join(frs_data_env$ben_mult_lookup %>% dplyr::select(-system),
              by = dplyr::join_by(class, 
                           tier_at_dist_age,
                           dist_age >= dist_age_min_ge,
                           dist_age < dist_age_max_lt,
                           yos >= yos_min_ge,
                           yos < yos_max_lt,
                           dist_year >= dist_year_min_ge,
                           dist_year < dist_year_max_lt)) %>%
    dplyr::left_join(frs_data_env$reduce_factor_lookup,
              by = c("tier_at_dist_age", "dist_age", "class")) %>%
    dplyr::mutate(db_benefit = compute_db_benefit(yos, ben_mult, fas, reduce_factor, params$cal_factor_),
           
           ann_factor_term = ann_factor * cum_mort_dr,
           
           pvfb_db_at_term_age = db_benefit * ann_factor_term
           
    ) %>% 
    dplyr::ungroup()
  benefit_table_s
}

get_benefit_val_table_s <- function(
    salary_benefit_table_s,
    final_benefit_table_s,
    separation_rate_table_s,
    params){
  tmp <- salary_benefit_table_s %>% 
    dplyr::left_join(final_benefit_table_s, by = c("class", "entry_year", "entry_age", "term_age")) %>%
    dplyr::left_join(separation_rate_table_s,
              by = c("class", "entry_year", "entry_age", "yos", "term_age")) %>%
    dplyr::left_join(frs_data_env$dr_lookup, by = c("tier" = "tier_at_dist_age")) %>%
    dplyr::mutate(
      sep_type = dplyr::case_when(
        stringr::str_detect(tier_at_term_age, "early|norm|reduced") ~ "retire",
        stringr::str_detect(tier_at_term_age, "non_vested") ~ "non_vested",
        stringr::str_detect(tier_at_term_age, "vested") & !stringr::str_detect(tier_at_term_age, "non_vested") ~ "vested",
        TRUE ~ NA_character_
      ),
      ben_decision = decision_rule(sep_type, yos),
      pvfb_db_wealth_at_term_age = pv_blend_rule(sep_type, pvfb_db_at_term_age, db_ee_balance, params$retire_refund_ratio_)
    )

  # Use core to compute PVFB at current age, PVFS, NC and PVFNC
  benefit_val_table_s <- benefit_compute_pv_and_nc(
    tmp,
    group_cols = c("class", "entry_year", "entry_age"),
    yos_col = "yos",
    pv_term_col = "pvfb_db_wealth_at_term_age",
    sep_rate_col = "separation_rate",
    remaining_prob_col = "remaining_prob",
    dr_col = "dr",
    salary_col = "salary",
    out_cols = list(
      pvfb_current = "pvfb_db_wealth_at_current_age",
      pvfs_current = "pvfs_at_current_age",
      indv_nc      = "indv_norm_cost",
      pvfnc        = "pvfnc_db"
    )
  )
  benefit_val_table_s
}

get_dist_age_table_s <- function(benefit_table_s){
  dist_age_table_s <- benefit_table_s %>%
    dplyr::mutate(is_norm_retire_elig = tier_at_dist_age %in% c("tier_1_norm", "tier_2_norm", "tier_3_norm")) %>%
    dplyr::group_by(class, entry_year, entry_age, term_age) %>%
    dplyr::summarise(
      earliest_norm_retire_age = n() - sum(is_norm_retire_elig) + min(dist_age),    
      term_status = tier_at_term_age[1], .groups = "drop") %>%
    dplyr::mutate(dist_age = dist_age_rule(term_status, earliest_norm_retire_age, term_age)) %>% 
    dplyr::select(class, entry_year, entry_age, term_age, dist_age)
  dist_age_table_s
}

get_final_benefit_table_s <- function(benefit_table_s, dist_age_table_s){
  final_benefit_table_s <- benefit_table_s %>% 
    dplyr::semi_join(dist_age_table_s,
              by = dplyr::join_by(class, entry_year, entry_age, dist_age, term_age)) %>% 
    dplyr::select(class, entry_year, entry_age, term_age, dist_age, db_benefit, pvfb_db_at_term_age, ann_factor_term) %>% 
    dplyr::mutate(
      db_benefit = dplyr::if_else(is.na(db_benefit), 0, db_benefit),
      pvfb_db_at_term_age = dplyr::if_else(is.na(pvfb_db_at_term_age), 0, pvfb_db_at_term_age)
    )
  final_benefit_table_s
}

get_salary_benefit_table_s <- function(entrant_profile_table_s,
                                       salary_growth_table_s,
                                       salary_headcount_table_s,
                                       params){
  salary_benefit_table_s <- tidyr::expand_grid(entry_year = params$entry_year_range_, 
                                        entry_age = entrant_profile_table_s$entry_age, 
                                        yos = params$yos_range_,
                                        class = params$class_names_no_drop_frs_) %>%
    dplyr::mutate(
      term_age = entry_age + yos) %>%
    dplyr::left_join(frs_data_env$tier_table, by = c("entry_year", "yos", "term_age"= "age", "class")) %>%
    dplyr::mutate(tier_at_term_age = tier) %>%
    dplyr::filter(term_age <= params$max_age_) %>% 
    dplyr::arrange(entry_year, entry_age, yos) %>% 
    dplyr::left_join(entrant_profile_table_s, by = c("entry_age", "class")) %>%
    dplyr::filter(is.na(start_sal) == FALSE) %>%
    dplyr::left_join(salary_growth_table_s, by = c("yos", "class")) %>%
    dplyr::left_join(salary_headcount_table_s %>% dplyr::select(entry_year, entry_age, entry_salary, class), 
              by = c("entry_year", "entry_age", "class")) %>%
    dplyr::mutate(ref_year = dplyr::if_else(class == "admin", 2015, 2020)) %>%
    dplyr::mutate(
      salary = dplyr::if_else(entry_year <= ref_year, 
                       entry_salary * cumprod_salary_increase,
                       start_sal * cumprod_salary_increase * (1 + params$payroll_growth_)^(entry_year - ref_year))
    ) %>% 
    dplyr::left_join(frs_data_env$fas_period_lookup, by = c("tier_at_term_age")) %>%
    dplyr::group_by(class, entry_year, entry_age) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      fas = compute_fas(salary, max(fas_period), drop_current = TRUE),
      db_ee_cont = params$db_ee_cont_rate_ * salary,
      db_ee_balance = accumulate_balance(params$db_ee_interest_rate_, db_ee_cont),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(salary)) %>%
    dplyr::filter(!is.na(start_sal)) %>%
    dplyr::select(-ref_year)
  salary_benefit_table_s
}

# get_benefit_data -- primary function ------------------------------------

get_benefit_data_s <- function(
    entrant_profile_table_s,
    salary_headcount_table_s,
    mort_table_s,
    mort_retire_table_s,
    separation_rate_table_s,
    params
) {
  salary_growth_table_s <- params$salary_growth_table

  salary_benefit_table_s <- get_salary_benefit_table_s(entrant_profile_table_s,
                                                       salary_growth_table_s,
                                                       salary_headcount_table_s,
                                                       params)

  ann_factor_table_s <- get_annuity_factor_table_s(
    mort_table_s,
    salary_benefit_table_s,
    params)

  ann_factor_retire_table_s <- get_annuity_factor_retire_table_s(
    mort_retire_table_s,
    params
  )

  benefit_table_s <- get_benefit_table_s(
    ann_factor_table_s,
    salary_benefit_table_s,
    params)

  dist_age_table_s <- get_dist_age_table_s(benefit_table_s)

  final_benefit_table_s <- get_final_benefit_table_s(benefit_table_s, dist_age_table_s)

  benefit_val_table_s <- get_benefit_val_table_s(
    salary_benefit_table_s,
    final_benefit_table_s,
    separation_rate_table_s,
    params)

  indv_norm_cost_table_s <- benefit_val_table_s %>% 
    dplyr::filter(yos == 0) %>% 
    dplyr::select(class, entry_year, entry_age, indv_norm_cost)

  agg_norm_cost_table_s <- get_agg_norm_cost_table_s(
    indv_norm_cost_table_s,
    salary_headcount_table_s,
    salary_benefit_table_s)

  list(
    ann_factor_table_s         = ann_factor_table_s,
    ann_factor_retire_table_s  = ann_factor_retire_table_s,
    benefit_table_s            = benefit_table_s,
    final_benefit_table_s      = final_benefit_table_s,
    benefit_val_table_s        = benefit_val_table_s,
    indv_norm_cost_table_s     = indv_norm_cost_table_s,
    agg_norm_cost_table_s      = agg_norm_cost_table_s
  )
}

# Precompute benefit_data_s for all classes (matches current behavior)
benefit_data_s <- get_benefit_data_s(
  frs_data_env$entrant_profile_table,
  frs_data_env$salary_headcount_table,
  frs_data_env$mort_table,
  frs_data_env$mort_retire_table,
  frs_data_env$separation_rate_table,
  params
)

# Class-specific extractor identical to existing get_benefit_data
get_benefit_data <- function(
    class_name,
    entrant_profile_table_s,
    salary_headcount_table_s,
    mort_table_s,
    mort_retire_table_s,
    separation_rate_table_s,
    params) {

  force(entrant_profile_table_s)
  force(salary_headcount_table_s)
  force(mort_table_s)
  force(mort_retire_table_s)
  force(separation_rate_table_s)

  benefit_data <- list(
    ann_factor_table        = bm_env$benefit_data_s$ann_factor_table_s %>% dplyr::ungroup() %>% dplyr::filter(class == class_name) %>% dplyr::select(-class),
    ann_factor_retire_table = bm_env$benefit_data_s$ann_factor_retire_table_s %>% dplyr::ungroup() %>% dplyr::filter(class == class_name) %>% dplyr::select(-class),
    benefit_table           = bm_env$benefit_data_s$benefit_table_s %>% dplyr::ungroup() %>% dplyr::filter(class == class_name) %>% dplyr::select(-class),
    final_benefit_table     = bm_env$benefit_data_s$final_benefit_table_s %>% dplyr::ungroup() %>% dplyr::filter(class == class_name) %>% dplyr::select(-class),
    benefit_val_table       = bm_env$benefit_data_s$benefit_val_table_s %>% dplyr::ungroup() %>% dplyr::filter(class == class_name) %>% dplyr::select(-class),
    indv_norm_cost_table    = bm_env$benefit_data_s$indv_norm_cost_table_s %>% dplyr::ungroup() %>% dplyr::filter(class == class_name) %>% dplyr::select(-class),
    agg_norm_cost_table     = bm_env$benefit_data_s$agg_norm_cost_table_s %>% dplyr::ungroup() %>% dplyr::filter(class == class_name) %>% dplyr::select(-class)
  )

  return(benefit_data)
}
