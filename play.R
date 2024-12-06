
# library(pendata) # not needed as it is loaded by FRS_master.R
data(package="pendata")
frs # accesses the data

names(frs_data_env)
frs_data_env$eco_salary_table_


# Load the old workspace environment
oldpath <- here::here("refactor", "reason_results", "reason_workspace.RData")
load(oldpath, oldws <- new.env())


tmp <- stack_env$salary_headcount_table_stacked |> 
  select(class, entry_year, entry_age, entry_salary) |> 
  mutate(max_entry_year = max(entry_year),
         .by=class)

tmp <- salary_benefit_table_stacked |> 
  filter(is.na(entry_salary)) |> 
  select(class, entry_year, entry_age, yos, term_age)
count(tmp, class)

tmp2 <- count(tmp, entry_year)
tmp2a <- count(tmp |> filter(class=="admin"), entry_year)
tmp2a2 <- tmp |> filter(class=="admin", entry_year==1990)

tmp3 <- tmp |> 
  filter(entry_year == 1975)

check <-     lm_env$get_liability_data(class_name, 
                                       wf_data, 
                                       ben_payment_current, 
                                       retiree_pop_current,
                                       pvfb_term_current,
                                       entrant_profile_table,
                                       salary_headcount_table,
                                       mort_table,
                                       mort_retire_table,
                                       separation_rate_table,
                                       params)
names(check)
check


class_salary_growth_table <- bm_env$get_class_salary_growth_table(class_name, params$salary_growth_table_)
check <-  bm_env$get_salary_benefit_table(class_name,
                                                            entrant_profile_table,
                                                            class_salary_growth_table,
                                                            salary_headcount_table,
                                                            params)
skim(check)

check2 <- salary_benefit_table_stacked |> filter(class=="admin")
skim(check2)

comp <- check |> 
  left_join(check2 |> select(entry_year, entry_age, yos, term_age, tier_at_term_age, salary2=salary),
            by = join_by(entry_year, entry_age, yos, term_age, tier_at_term_age))



stack_env$salary_headcount_table_stacked |> 
  select(class, entry_year, entry_age, entry_salary) |> skim()

salary_headcount_table # 26 rows
stack_env$salary_headcount_table_stacked |> filter(class=="admin") # 26 rows

entrant_profile_table # 5 rows
stack_env$entrant_profile_table_stacked |> filter(class=="admin") # 5 rows

class_salary_growth_table # 71 rows
stack_env$salary_growth_table_stacked |> filter(class=="admin") # 71 rows

