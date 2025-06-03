#' Simulate Workforce Projection Using Data Frame-Based Method
#'
#' This function uses pre-calculated cumulative separation and mortality probabilities
#' to simulate the flows of active, terminated, refunded, and retired members using data frames.
simulate_workforce_df <- function(
    entrant_profile_table,
    separation_rate_table,
    mort_table,
    benefit_val_table,
    start_year,
    model_period,
    retire_refund_ratio,
    pop_growth
) {
  library(dplyr)
  library(tidyr)
  
  entry_age_range <- unique(entrant_profile_table$entry_age)
  age_range <- min(entry_age_range):max(separation_rate_table$term_age)
  year_range <- start_year:(start_year + model_period)
  
  df <- expand_grid(entry_age = entry_age_range, age = age_range, entry_year = year_range) %>%
    mutate(year = entry_year + (age - entry_age)) %>%
    filter(year %in% year_range)
  
  # Precalculate cumulative probabilities
  df <- df %>%
    left_join(separation_rate_table, by = c("entry_age", "age" = "term_age", "entry_year")) %>%
    left_join(mort_table, by = c("entry_age", "age" = "dist_age", "year" = "dist_year")) %>%
    group_by(entry_age, entry_year) %>%
    arrange(age, .by_group = TRUE) %>%
    mutate(
      separation_rate = replace_na(separation_rate, 0),
      mort_rate = replace_na(mort_final, 0),
      cum_sep_survive = cumprod(1 - separation_rate),
      cum_mort_survive = cumprod(1 - mort_rate)
    ) %>%
    ungroup()
  
  # Add entrants and compute active/terminated populations
  df <- df %>%
    left_join(entrant_profile_table, by = "entry_age") %>%
    mutate(
      expected_active = entrant_dist * cum_sep_survive * cum_mort_survive,
      expected_term = entrant_dist * (1 - cum_sep_survive) * cum_mort_survive
    )
  
  # Add benefit decisions
  decision_table <- benefit_val_table %>%
    select(entry_year, entry_age, term_age, yos, dist_age, ben_decision) %>%
    mutate(
      refund_share = case_when(
        ben_decision == "refund" ~ 1,
        ben_decision == "mix" ~ 1 - retire_refund_ratio,
        TRUE ~ 0
      ),
      retire_share = case_when(
        ben_decision == "retire" ~ 1,
        ben_decision == "mix" ~ retire_refund_ratio,
        TRUE ~ 0
      )
    )
  
  df <- df %>%
    mutate(term_age = age, yos = age - entry_age) %>%
    left_join(decision_table, by = c("entry_age", "entry_year", "term_age", "yos", "age" = "dist_age")) %>%
    mutate(
      refund_share = replace_na(refund_share, 0),
      retire_share = replace_na(retire_share, 0),
      expected_refund = expected_term * refund_share,
      expected_retire = expected_term * retire_share,
      expected_retire_survive = expected_retire * cum_mort_survive
    )
  
  # Summarize results by year
  summary_df <- df %>%
    group_by(year) %>%
    summarise(
      active = sum(expected_active, na.rm = TRUE),
      term = sum(expected_term, na.rm = TRUE),
      refund = sum(expected_refund, na.rm = TRUE),
      retire = sum(expected_retire, na.rm = TRUE),
      retire_alive = sum(expected_retire_survive, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(summary_df)
}
