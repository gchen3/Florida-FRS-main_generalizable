 # ================================================================
 # Workforce core (generic engine)
 # Contains generic utilities, state initialization, propagation, and
 # array->data.frame conversion. No FRS-specific table logic here.
 # ================================================================

# Keep explicit namespace calls where possible to avoid requiring library()

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
  xtabs(stats::reformulate(c("entry_age","age","year"), response = val_col), df)
}

xtabs_full_4d <- function(df, val_col, ea_levels, age_levels, year_levels, term_levels) {
  df$entry_age <- factor(df$entry_age, levels = ea_levels)
  df$age       <- factor(df$age,       levels = age_levels)
  df$year      <- factor(df$year,      levels = year_levels)
  df$term_year <- factor(df$term_year, levels = term_levels)
  xtabs(stats::reformulate(c("entry_age","age","year","term_year"), response = val_col), df)
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
  shc <- data.table::as.data.table(salary_headcount_table)[
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

