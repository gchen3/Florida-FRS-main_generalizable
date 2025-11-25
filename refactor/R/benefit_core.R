 # ================================================================
 # Benefit core (generic mechanics)
 # Generic, plan-agnostic helpers for annuity factors, PV/NC, and
 # aggregation. Adapters supply plan-specific joins, columns, and rules.
 # ================================================================

#' Compute annuity factors from mortality, discount, and COLA series
#' Assumes input already has mort/dr/cola columns and is grouped by keys
benefit_annuity_factors <- function(df,
                                    group_cols,
                                    mort_col = "mort_final",
                                    dr_col   = "dr",
                                    cola_col = "cola",
                                    out_ann_col = "ann_factor") {
  stopifnot(all(c(mort_col, dr_col, cola_col) %in% names(df)))
  g <- dplyr::group_by_at(df, group_cols)
  g <- dplyr::mutate(g,
                     cum_dr   = cumprod(1 + dplyr::lag(.data[[dr_col]], default = 0)),
                     cum_mort = cumprod(1 - dplyr::lag(.data[[mort_col]], default = 0)),
                     cum_cola = cumprod(1 + dplyr::lag(.data[[cola_col]], default = 0)),
                     cum_mort_dr      = cum_mort / cum_dr,
                     cum_mort_dr_cola = cum_mort_dr * cum_cola)
  g <- dplyr::mutate(g,
                     !!rlang::sym(out_ann_col) := rev(cumsum(rev(cum_mort_dr_cola))) / cum_mort_dr_cola)
  dplyr::ungroup(g)
}

#' Compute retire annuity factors using pentools::annfactor
#' Input must have mort/dr/cola columns and be grouped by base age (or keys)
benefit_annuity_factors_retire <- function(df,
                                           group_cols,
                                           mort_col = "mort_final",
                                           dr_col   = "dr",
                                           cola_col = "cola",
                                           one_time_cola = FALSE,
                                           out_ann_col = "ann_factor_retire") {
  stopifnot(all(c(mort_col, dr_col, cola_col) %in% names(df)))
  g <- dplyr::group_by_at(df, group_cols)
  g <- dplyr::mutate(g,
                     cum_dr   = cumprod(1 + dplyr::lag(.data[[dr_col]], default = 0)),
                     cum_mort = cumprod(1 - dplyr::lag(.data[[mort_col]], default = 0)),
                     cum_mort_dr = cum_mort / cum_dr,
                     !!rlang::sym(out_ann_col) := pentools::annfactor(cum_mort_dr,
                                                          cola_vec = .data[[cola_col]],
                                                          one_time_cola = one_time_cola))
  dplyr::ungroup(g)
}

#' Compute PV of benefits and normal cost from series inputs
benefit_compute_pv_and_nc <- function(df,
                                      group_cols,
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
                                      )) {
  req <- c(yos_col, pv_term_col, sep_rate_col, remaining_prob_col, dr_col, salary_col)
  stopifnot(all(req %in% names(df)))
  g <- dplyr::group_by_at(df, group_cols)
  pvfb_current_nm <- out_cols$pvfb_current
  pvfs_current_nm <- out_cols$pvfs_current
  indv_nc_nm      <- out_cols$indv_nc
  pvfnc_nm        <- out_cols$pvfnc

  g <- dplyr::mutate(g,
      !!rlang::sym(pvfb_current_nm) := pentools::get_pvfb(
        sep_rate_vec = .data[[sep_rate_col]],
        interest_vec = .data[[dr_col]],
        value_vec    = .data[[pv_term_col]]
      ),
      !!rlang::sym(pvfs_current_nm) := pentools::get_pvfs(
        remaining_prob_vec = .data[[remaining_prob_col]],
        interest_vec       = .data[[dr_col]],
        sal_vec            = .data[[salary_col]]
      )
  )
  g <- dplyr::mutate(g,
      !!rlang::sym(indv_nc_nm) := .data[[pvfb_current_nm]][.data[[yos_col]] == 0] /
                                   .data[[pvfs_current_nm]][.data[[yos_col]] == 0],
      !!rlang::sym(pvfnc_nm)   := .data[[indv_nc_nm]] * .data[[pvfs_current_nm]]
  )
  dplyr::ungroup(g)
}

#' Aggregate individual NC to payroll-weighted rate
benefit_aggregate_normal_cost <- function(indv_nc_df,
                                          salary_headcount_df,
                                          salary_benefit_df,
                                          group_cols = c("class")) {
  tmp <- indv_nc_df |>
    dplyr::left_join(salary_headcount_df, by = c("class", "entry_year", "entry_age")) |>
    dplyr::left_join(salary_benefit_df |> dplyr::select(class, entry_year, entry_age, yos, salary),
                     by = c("class", "entry_year", "entry_age", "yos")) |>
    dplyr::filter(!is.na(count)) |>
    dplyr::group_by_at(group_cols) |>
    dplyr::summarise(agg_normal_cost = sum(indv_norm_cost * salary * count) /
                                        sum(salary * count), .groups = "drop")
  tmp
}

# ---------------------------
# Additional small core helpers
# ---------------------------

# Compute FAS series as rolling mean of prior salaries
compute_fas <- function(salary_vec, window, drop_current = TRUE) {
  if (drop_current) {
    base <- c(NA, salary_vec[-length(salary_vec)])
  } else {
    base <- salary_vec
  }
  RcppRoll::roll_mean(base, n = window, align = "right", fill = NA)
}

# Accumulate employee contributions at constant rate
accumulate_balance <- function(interest_rate, contrib_vec) {
  pentools::get_cum_fv(interest_rate, contrib_vec)
}

# Compute DB benefit value from components
compute_db_benefit <- function(yos, ben_mult, fas, reduce_factor, cal_factor) {
  yos * ben_mult * fas * reduce_factor * cal_factor
}

# Lightweight validator for required columns
validate_required_cols <- function(df, cols, df_name = "data frame") {
  missing <- setdiff(cols, names(df))
  if (length(missing)) stop(sprintf("%s missing required columns: %s", df_name, paste(missing, collapse = ", ")))
  invisible(df)
}
