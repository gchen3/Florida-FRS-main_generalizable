## define the parameters for tests
class_name = "regular"
entrant_profile_table = frs_data_env$regular_entrant_profile_table
salary_headcount_table = frs_data_env$regular_salary_headcount_table
mort_table = frs_data_env$regular_mort_table
mort_retire_table = frs_data_env$regular_mort_retire_table
separation_rate_table = frs_data_env$regular_separation_rate_table
params = params

##load the get_ann_factor_table function
source(fs::path(rdir, "FRS_benefit_model_get_benefit_data_function_GC.R")) 

class_salary_growth_table <- params$salary_growth_table %>% filter(class == class_name)

salary_benefit_table <- get_salary_benefit_table(class_name,
                                                 entrant_profile_table,
                                                 class_salary_growth_table,
                                                 salary_headcount_table,
                                                 params)

ann_factor_table <- get_annuity_factor_table(
  mort_table,
  salary_benefit_table,
  params)



benefit_table <- ann_factor_table %>%
  mutate(
    term_age = entry_age + yos, .before = term_year,
    class_name = class_name,
    is_norm_retire_elig = str_detect(tier_at_dist_age, "norm")
  ) %>%
  # dist_age is distribution age, and dist_year is distribution year.
  # distribution age means the age when the member starts to accept benefits (either a refund or a pension)
  # left_join(salary_benefit_table,
  #           by = c("entry_year", "entry_age", "yos", "term_age")) %>%
  left_join(ben_mult_lookup %>% filter(class_name == "regular"),
            by = c("tier_at_dist_age", "dist_age", "dist_year", "yos")) %>%
  mutate(
    ben_mult_2 = frs_data_env$get_ben_mult(
      tier = tier_at_dist_age,
      class_name = class_name,
      dist_age = dist_age,
      dist_year = dist_year,
      yos = yos))

x <- benefit_table$ben_mult
  
  summary_stats <- c(
    Q1 = round(quantile(x, 0.25, na.rm = TRUE), 3),
    Median = round(quantile(x, 0.5, na.rm = TRUE), 3),
    Q3 = round(quantile(x, 0.75, na.rm = TRUE), 3),
    n = sum(!is.na(x))
  )
  summary_stats
  
  compare_benefit_table <- benefit_table %>%
    mutate(mismatch = case_when(
      is.na(ben_mult) & is.na(ben_mult_2) ~ FALSE,                      # treat NA == NA as match
      !is.na(ben_mult) & !is.na(ben_mult_2) ~ !near(ben_mult, ben_mult_2, tol = 1e-6),  # compare numerics
      TRUE ~ TRUE  # one NA, one not => mismatch
    )) %>%
    filter(mismatch == TRUE)

  