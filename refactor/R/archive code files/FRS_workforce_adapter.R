 # ================================================================
 # FRS workforce adapter (V3-compatible)
 # Wires FRS tables to the generic workforce_core engine.
 # Keeps behavior identical to V3 for FRS usage.
 # ================================================================

# Safety for joins: drop non-matches rather than recycling
options(datatable.nomatch = 0)

# Build probability arrays from FRS-formatted input tables
frs_build_probability_arrays <- function(
    age_range,
    entry_age_range,
    year_range,
    benefit_val_table,
    mort_table,
    separation_rate_table,
    retire_refund_ratio
) {
  # ---- Mortality (entry_age, age, year, term_year)
  mort_dt <- data.table::as.data.table(mort_table)[
    entry_age %in% entry_age_range &
      dist_age %in% age_range &
      dist_year %in% year_range &
      term_year %in% year_range,
    .(entry_age, age = dist_age, year = dist_year, term_year, mort = mort_final)
  ]
  mort_dt[is.na(mort), mort := 0]
  mort_array_term <- xtabs_full_4d(
    as.data.frame(mort_dt), "mort",
    ea_levels   = entry_age_range,
    age_levels  = age_range,
    year_levels = year_range,
    term_levels = year_range
  )

  # ---- Separation (entry_age, age, year) — entry-year join
  sep_df <- tidyr::expand_grid(
    entry_age = entry_age_range,
    age       = age_range,
    year      = year_range
  ) |>
    dplyr::mutate(entry_year = year - (age - entry_age)) |>
    dplyr::left_join(
      separation_rate_table,
      by = c("entry_age", "age" = "term_age", "entry_year")
    ) |>
    dplyr::transmute(
      entry_age, age, year,
      separation_rate = dplyr::if_else(is.na(separation_rate), 0.0, separation_rate)
    )
  sep_array <- xtabs_full_3d(
    sep_df, "separation_rate",
    ea_levels   = entry_age_range,
    age_levels  = age_range,
    year_levels = year_range
  )

  # ---- Retire / Refund arrays (consistent grid + join semantics)
  optimal_retire <- data.table::as.data.table(benefit_val_table)[
    , .(
      entry_year, entry_age, term_age, yos, dist_age,
      refund = data.table::fifelse(ben_decision == "refund", 1.0,
                                   data.table::fifelse(ben_decision == "mix", 1.0 - retire_refund_ratio, 0.0)),
      retire = data.table::fifelse(ben_decision %in% c("retire", "mix"), 1.0, 0.0),
      refund_age = term_age
    )
  ]
  # Coerce join keys to integer to avoid type mismatches in joins
  optimal_retire[, `:=`(
    entry_year = as.integer(entry_year),
    entry_age  = as.integer(entry_age),
    term_age   = as.integer(term_age),
    yos        = as.integer(yos),
    dist_age   = as.integer(dist_age),
    refund_age = as.integer(refund_age)
  )]

  # Retire grid
  retire_df <- tidyr::expand_grid(
    entry_age = entry_age_range,
    age       = age_range,
    year      = year_range,
    term_year = year_range
  ) |>
    dplyr::mutate(
      entry_year = year - (age - entry_age),
      term_age   = age - (year - term_year),
      yos        = term_age - entry_age,
      entry_year = as.integer(entry_year),
      term_age   = as.integer(term_age),
      yos        = as.integer(yos),
      entry_age  = as.integer(entry_age),
      age        = as.integer(age)
    ) |>
    dplyr::filter(year - term_year >= 0, yos >= 0) |>
    dplyr::left_join(
      as.data.frame(optimal_retire),
      by = c("entry_age", "age" = "dist_age", "entry_year", "term_age", "yos")
    ) |>
    dplyr::mutate(retire = dplyr::if_else(is.na(retire), 0, retire))

  retire_array <- xtabs_full_4d(
    retire_df, "retire",
    ea_levels   = entry_age_range,
    age_levels  = age_range,
    year_levels = year_range,
    term_levels = year_range
  )

  # Refund grid (same-year dimension used in propagation; index by term_age)
  refund_df <- tidyr::expand_grid(
    entry_age = entry_age_range,
    age       = age_range,
    year      = year_range,
    term_year = year_range
  ) |>
    dplyr::mutate(
      entry_year = year - (age - entry_age),
      term_age   = age - (year - term_year),
      yos        = term_age - entry_age,
      entry_year = as.integer(entry_year),
      term_age   = as.integer(term_age),
      yos        = as.integer(yos),
      entry_age  = as.integer(entry_age),
      age        = as.integer(age)
    ) |>
    dplyr::filter(year - term_year >= 0, yos >= 0) |>
    dplyr::left_join(
      as.data.frame(optimal_retire),
      by = c("entry_age", "age" = "refund_age", "entry_year", "term_age", "yos")
    ) |>
    dplyr::mutate(refund = dplyr::if_else(is.na(refund), 0, refund))

  refund_array <- xtabs_full_4d(
    refund_df, "refund",
    ea_levels   = entry_age_range,
    age_levels  = age_range,
    year_levels = year_range,
    term_levels = year_range
  )

  list(
    mort_array_term = mort_array_term,
    sep_array       = sep_array,
    retire_array    = retire_array,
    refund_array    = refund_array
  )
}

# Build benefit data once per session (matches V3 behavior)
benefit_data_s <- bm_env$get_benefit_data_s(
  frs_data_env$entrant_profile_table,
  frs_data_env$salary_headcount_table,
  frs_data_env$mort_table,
  frs_data_env$mort_retire_table,
  frs_data_env$separation_rate_table,
  params
)

# Main entry: identical signature to V3
get_wf_data_s <- function(class_name, params) {
  cat("\n\n"); message(sprintf("..preparing wf_data for class: %s", class_name))

  benefit_val_table      <- benefit_data_s$benefit_val_table     %>% dplyr::filter(class == class_name) %>% dplyr::select(-class)
  entrant_profile_table  <- frs_data_env$entrant_profile_table   %>% dplyr::filter(class == class_name) %>% dplyr::select(-class)
  salary_headcount_table <- frs_data_env$salary_headcount_table  %>% dplyr::filter(class == class_name) %>% dplyr::select(-class)
  mort_table             <- frs_data_env$mort_table              %>% dplyr::filter(class == class_name) %>% dplyr::select(-class)
  separation_rate_table  <- frs_data_env$separation_rate_table   %>% dplyr::filter(class == class_name) %>% dplyr::select(-class)

  entry_age_range <- entrant_profile_table$entry_age
  year_range      <- params$start_year_:(params$start_year_ + params$model_period_)
  age_range       <- min(entry_age_range):max(params$age_range_)

  # 1) init states
  a <- proc.time()
  state <- initialize_state_arrays(age_range, entry_age_range, year_range, salary_headcount_table)
  cat(" initialize_state_arrays: ", (proc.time() - a), "\n")

  # 2) probs from FRS tables
  a <- proc.time()
  probs <- frs_build_probability_arrays(
    age_range, entry_age_range, year_range,
    benefit_val_table, mort_table, separation_rate_table,
    retire_refund_ratio = params$retire_refund_ratio_
  )
  cat(" build_probability_arrays: ", (proc.time() - a), "\n")

  # 3) propagate
  a <- proc.time()
  state <- propagate_workforce(
    state, probs,
    age_range, entry_age_range, year_range,
    entrant_profile_table,
    pop_growth = params$pop_growth_
  )
  cat(" propagate_workforce: ", (proc.time() - a), "\n")

  # 4) arrays → dfs
  dfs <- arrays_to_dfs(state, entry_age_range, age_range, year_range)

  wf_data <- list(
    wf_active_df = dfs$wf_active_df,
    wf_term_df   = dfs$wf_term_df,
    wf_refund_df = dfs$wf_refund_df,
    wf_retire_df = dfs$wf_retire_df
  )
  saveRDS(wf_data, fs::path(iddir, paste0(class_name, "_wf_data.rds")))
  invisible(wf_data)
}

