# Get benefit data for salaried employees
benefit_data_s <- bm_env$get_benefit_data_s(
  params$entrant_profile_table,
  params$salary_headcount_table,
  params$mort_table,
  params$mort_retire_table,
  params$separation_rate_table,
  params
)