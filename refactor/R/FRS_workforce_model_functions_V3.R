 # ================================================================
 # Workforce model — modular, robust (V3)
 # Standalone implementation with consistent refund indexing by term_age.
 # ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(data.table)
  library(fs)
})

# Safety for joins: drop non-matches rather than recycling
options(datatable.nomatch = 0)

# ---------------------------
# Utils
# ---------------------------

# Matrix shift via transition matrix TM (ages everyone by +1 year)
shift_with_TM <- function(mat, TM) mat %*% TM

# Position matrix (new entrants enter at diagonal entry_age==age)
make_position_matrix <- function(entry_age_range, age_range) {
  df <- tidyr::expand_grid(entry_age = entry_age_range, age = age_range) |>
    dplyr::mutate(new = as.integer(entry_age == age))
  xtabs(new ~ entry_age + age, df)
}

# Keep full grids when aggregating to arrays (avoid xtabs level dropping)
xtabs_full_3d <- function(df, val_col, ea_levels, age_levels, year_levels) {
  df$entry_age <- factor(df$entry_age, levels = ea_levels)
  df$age       <- factor(df$age,       levels = age_levels)
  df$year      <- factor(df$year,      levels = year_levels)
  xtabs(reformulate(c("entry_age","age","year"), response = val_col), df)
}

xtabs_full_4d <- function(df, val_col, ea_levels, age_levels, year_levels, term_levels) {
  df$entry_age <- factor(df$entry_age, levels = ea_levels)
  df$age       <- factor(df$age,       levels = age_levels)
  df$year      <- factor(df$year,      levels = year_levels)
  df$term_year <- factor(df$term_year, levels = term_levels)
  xtabs(reformulate(c("entry_age","age","year","term_year"), response = val_col), df)
}

# ---------------------------
# Probability arrays
# ---------------------------
build_probability_arrays <- function(
    age_range,
    entry_age_range,
    year_range,
    benefit_val_table,
    mort_table,
    separation_rate_table,
    retire_refund_ratio
) {
  

  # ---- Mortality (entry_age, age, year, term_year)
  mort_dt <- as.data.table(mort_table)[
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
  optimal_retire <- as.data.table(benefit_val_table)[
    , .(
      entry_year, entry_age, term_age, yos, dist_age,
      refund = fifelse(ben_decision == "refund", 1.0,
                       fifelse(ben_decision == "mix", 1.0 - retire_refund_ratio, 0.0)),
      retire = fifelse(ben_decision %in% c("retire", "mix"), 1.0, 0.0),
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
      yos        = term_age - entry_age
    ) |>
    dplyr::mutate(
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
      yos        = term_age - entry_age
    ) |>
    dplyr::mutate(
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

# ---------------------------
# State arrays (seed)
# ---------------------------
initialize_state_arrays <- function(
    age_range,
    entry_age_range,
    year_range,
    salary_headcount_table
) {
  active_dim       <- c(length(entry_age_range), length(age_range), length(year_range))
  term_dim         <- c(length(entry_age_range), length(age_range), length(year_range), length(year_range))
  retire_dim       <- c(length(entry_age_range), length(age_range), length(year_range), length(year_range), length(year_range))
  active_dim_names <- list(entry_age = entry_age_range, age = age_range, year = year_range)
  term_dim_names   <- list(entry_age = entry_age_range, age = age_range, year = year_range, term_year = year_range)
  retire_dim_names <- list(entry_age = entry_age_range, age = age_range, year = year_range,
                           term_year = year_range, retire_year = year_range)

  wf_active <- array(0, dim = active_dim, dimnames = active_dim_names)
  wf_term   <- array(0, dim = term_dim,   dimnames = term_dim_names)
  wf_refund <- array(0, dim = term_dim,   dimnames = term_dim_names)
  wf_retire <- array(0, dim = retire_dim, dimnames = retire_dim_names)

  # Seed actives (year 1) from salary_headcount_table (sum dups; NA->0)
  shc <- as.data.table(salary_headcount_table)[
    entry_age %in% entry_age_range & age %in% age_range,
    .(count = sum(ifelse(is.na(count), 0, count), na.rm = TRUE)),
    by = .(entry_age, age)
  ]
  seed_mat <- xtabs(count ~ entry_age + age, data = as.data.frame(shc))
  wf_active[, , 1] <- 0
  ea_idx <- match(rownames(seed_mat), dimnames(wf_active)$entry_age)
  ag_idx <- match(colnames(seed_mat), dimnames(wf_active)$age)
  if (length(ea_idx) && length(ag_idx)) {
    wf_active[ea_idx, ag_idx, 1] <- seed_mat
  }

  list(
    wf_active = wf_active,
    wf_term   = wf_term,
    wf_refund = wf_refund,
    wf_retire = wf_retire
  )
}

# ---------------------------
# Propagate year by year
# ---------------------------
propagate_workforce <- function(
    state, probs,
    age_range, entry_age_range, year_range,
    entrant_profile_table, pop_growth
) {
  wf_active <- state$wf_active
  wf_term   <- state$wf_term
  wf_refund <- state$wf_refund
  wf_retire <- state$wf_retire

  mort_array_term <- probs$mort_array_term
  sep_array       <- probs$sep_array
  retire_array    <- probs$retire_array
  refund_array    <- probs$refund_array

  term_dim   <- dim(wf_term)
  retire_dim <- dim(wf_retire)

  position_matrix <- make_position_matrix(entry_age_range, age_range)
  TM <- diag(length(age_range) + 1)[-1, -(length(age_range) + 1)]

  for (i in 2:length(year_range)) {
    # --- Actives to terminations in prior year
    active2term <- wf_active[,,i - 1] * sep_array[,,i - 1]

    # Age actives
    wf_active[,,i] <- shift_with_TM(wf_active[,,i - 1] - active2term, TM)

    # New entrants
    new_entrants <- pentools::add_new_entrants(
      g = pop_growth,
      ne_dist = entrant_profile_table$entrant_dist,
      wf1 = wf_active[,,i - 1],
      wf2 = wf_active[,,i],
      ea  = entry_age_range,
      age = age_range,
      position_matrix = position_matrix
    )
    wf_active[,,i] <- wf_active[,,i] + new_entrants

    # --- Terms: deaths + aging for existing term cohorts (prior year)
    term2death <- wf_term[,,i - 1, ] * mort_array_term[,,i - 1, ]
    wf_term[,,i, ] <- apply(wf_term[,,i - 1, ] - term2death, 3, shift_with_TM, TM = TM)

    # Add this year's new terminations (post-aging)
    wf_term[,,i, i] <- shift_with_TM(active2term, TM)

    # Same-year refunds
    term2refund <- wf_term[,,i, i] * refund_array[,,i, i]
    wf_term[,,i, i]   <- wf_term[,,i, i] - term2refund
    wf_refund[,,i, i] <- term2refund

    # Retirements from term pool
    term2retire <- wf_term[,,i, ] * retire_array[,,i, ]
    wf_term[,,i, ] <- wf_term[,,i, ] - term2retire

    # Retirees: deaths + aging
    retire2death <- apply(
      wf_retire[,,i - 1, , ],
      4, # across term_year
      function(x) x * mort_array_term[,,i - 1, ]
    )
    retire2death <- array(retire2death, dim = retire_dim[-3])
    wf_retire[,,i, , ] <- apply(wf_retire[,,i - 1, , ] - retire2death, c(3, 4), shift_with_TM, TM = TM)

    # Add new retirees (retire_year == i)
    wf_retire[,,i, , i] <- term2retire
  }

  list(wf_active = wf_active, wf_term = wf_term, wf_refund = wf_refund, wf_retire = wf_retire)
}

# ---------------------------
# Arrays → Data frames
# ---------------------------
arrays_to_dfs <- function(state, entry_age_range, age_range, year_range) {
  wf_active_df <- data.frame(
    expand.grid(entry_age = entry_age_range, age = age_range, year = year_range),
    n_active = as.vector(state$wf_active)
  ) %>% dplyr::filter(age >= entry_age)

  wf_term_df <- data.frame(
    expand.grid(entry_age = entry_age_range, age = age_range, year = year_range, term_year = year_range),
    n_term = as.vector(state$wf_term)
  ) %>% dplyr::filter(age >= entry_age, year >= term_year)

  wf_refund_df <- data.frame(
    expand.grid(entry_age = entry_age_range, age = age_range, year = year_range, term_year = year_range),
    n_refund = as.vector(state$wf_refund)
  ) %>% dplyr::filter(age >= entry_age, year >= term_year)

  # Build wf_retire_df: split by entry_age and CJ cross join
  retire_year_range <- year_range
  term_year_range   <- year_range
  wf_retire_list <- list()
  for (i in seq_along(entry_age_range)) {
    ea <- entry_age_range[i]
    # slice: age x year x term_year x retire_year
    slice <- state$wf_retire[i, , , , ]
    wf_retire_i <- data.table::data.table(
      data.table::CJ(retire_year = retire_year_range,
                     term_year = term_year_range,
                     year = year_range,
                     age = age_range),
      n_retire = as.vector(slice)
    )[n_retire > 0, ][, entry_age := ea][, .(entry_age, age, year, term_year, retire_year, n_retire)]
    wf_retire_list[[length(wf_retire_list) + 1]] <- wf_retire_i
  }
  wf_retire_df <- data.table::rbindlist(wf_retire_list, use.names = TRUE)

  list(
    wf_active_df = wf_active_df,
    wf_term_df   = wf_term_df,
    wf_refund_df = wf_refund_df,
    wf_retire_df = wf_retire_df
  )
}

# ================================================================
# Main
# ================================================================
benefit_data_s <- bm_env$get_benefit_data_s(
  params$entrant_profile_table,
  params$salary_headcount_table,
  params$mort_table,
  params$mort_retire_table,
  params$separation_rate_table,
  params
)

get_wf_data_s <- function(class_name, params) {
  cat("\n\n"); message(sprintf("..preparing wf_data for class: %s", class_name))

  benefit_val_table      <- benefit_data_s$benefit_val_table     %>% filter(class == class_name) %>% select(-class)
  entrant_profile_table  <- params$entrant_profile_table   %>% filter(class == class_name) %>% select(-class)
  salary_headcount_table <- params$salary_headcount_table  %>% filter(class == class_name) %>% select(-class)
  mort_table             <- params$mort_table              %>% filter(class == class_name) %>% select(-class)
  separation_rate_table  <- params$separation_rate_table   %>% filter(class == class_name) %>% select(-class)

  entry_age_range <- entrant_profile_table$entry_age
  year_range      <- params$start_year_:(params$start_year_ + params$model_period_)
  age_range       <- min(entry_age_range):max(params$age_range_)

  # 1) init states
  a <- proc.time()
  state <- initialize_state_arrays(age_range, entry_age_range, year_range, salary_headcount_table)
  cat(" initialize_state_arrays: ", (proc.time() - a), "\n")

  # 2) probs (direct from your tables)
  a <- proc.time()
  probs <- build_probability_arrays(
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
