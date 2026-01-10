frs <- pendata::frs

extra_names <- c(
  "amortization_bases",
  "ben_mult_lookup",
  "benefit_rules",
  "benefit_rules_test_cases",
  "cola_lookup",
  "constants_assumptions",
  "constants_assumptions_tbl",
  "dr_lookup",
  "entrant_profile_table",
  "fas_period_lookup",
  "headcount_salary",
  "mort_retire_table",
  "mort_table",
  "plan_shortname",
  "reduce_factor_lookup",
  "retirees",
  "retirement_rates",
  "salary_headcount_table",
  "salarygrowth",
  "separation_rate_table",
  "tier_table",
  "withdrawal"
)

# New environment
params <- new.env(parent = emptyenv())

# Load all existing params_env entries
list2env(as.list(frs$params_env), envir = params)

# Add the extra items from frs
for (nm in extra_names) {
  assign(nm, frs[[nm]], envir = params)
}

ls(params)
