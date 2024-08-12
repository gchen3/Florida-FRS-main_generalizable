# Salary & Headcount Processing -------------------------------------------

#Calculate salary cumulative growth 
salary_growth_table <- salary_growth_table_ %>% 
  bind_rows(tibble(yos = (max(salary_growth_table_$yos)+1):max(yos_range_))) %>% 
  fill(everything(), .direction = "down") %>% 
  mutate(across(contains("salary"), ~ cumprod(1 + lag(.x, default = 0)), .names = "cumprod_{.col}"), .keep = "unused")


#Joining headcount data, salary data, and salary growth data
#We account for the Investment Plan (DC plan) head count by inflating the DB head count by the ratio of total system head count to DB head count
#ECO, ESO, and Judges head counts are processed separately as the ACFR does not provide detailed head counts for these classes 
eco_eso_judges_active_member_adjustment_ratio <- eco_eso_judges_total_active_member_ / sum(eco_headcount_table_[-1] + eso_headcount_table_[-1] + judges_headcount_table_[-1])


#.. call get_salary_headcount_table -----------------------------------------

regular_salary_headcount_table <- get_salary_headcount_table(regular_salary_table_, regular_headcount_table_, salary_growth_table, "regular")$salary_headcount_table
regular_entrant_profile_table <- get_salary_headcount_table(regular_salary_table_, regular_headcount_table_, salary_growth_table, "regular")$entrant_profile

special_salary_headcount_table <- get_salary_headcount_table(special_salary_table_, special_headcount_table_, salary_growth_table, "special")$salary_headcount_table
special_entrant_profile_table <- get_salary_headcount_table(special_salary_table_, special_headcount_table_, salary_growth_table, "special")$entrant_profile

admin_salary_headcount_table <- get_salary_headcount_table(admin_salary_table_, admin_headcount_table_, salary_growth_table, "admin")$salary_headcount_table
admin_entrant_profile_table <- get_salary_headcount_table(admin_salary_table_, admin_headcount_table_, salary_growth_table, "admin")$entrant_profile

eco_salary_headcount_table <- get_salary_headcount_table(eco_salary_table_, eco_headcount_table_, salary_growth_table, "eco")$salary_headcount_table
eco_entrant_profile_table <- get_salary_headcount_table(eco_salary_table_, eco_headcount_table_, salary_growth_table, "eco")$entrant_profile

eso_salary_headcount_table <- get_salary_headcount_table(eso_salary_table_, eso_headcount_table_, salary_growth_table, "eso")$salary_headcount_table
eso_entrant_profile_table <- get_salary_headcount_table(eso_salary_table_, eso_headcount_table_, salary_growth_table, "eso")$entrant_profile

judges_salary_headcount_table <- get_salary_headcount_table(judges_salary_table_, judges_headcount_table_, salary_growth_table, "judges")$salary_headcount_table
judges_entrant_profile_table <- get_salary_headcount_table(judges_salary_table_, judges_headcount_table_, salary_growth_table, "judges")$entrant_profile

senior_management_salary_headcount_table <- get_salary_headcount_table(senior_management_salary_table_, 
                                                                       senior_management_headcount_table_, salary_growth_table, "senior management")$salary_headcount_table
senior_management_entrant_profile_table <- get_salary_headcount_table(senior_management_salary_table_, 
                                                                      senior_management_headcount_table_, salary_growth_table, "senior management")$entrant_profile


# Retirement & Separation Conditions --------------------------------------

# no actions related to this?


# Mortality Assumptions ---------------------------------------------------

#.. base mortality table -----------------------------------------------

base_general_mort_table <- get_base_mort_table(base_general_mort_table_)
base_teacher_mort_table <- get_base_mort_table(base_teacher_mort_table_)
base_safety_mort_table <- get_base_mort_table(base_safety_mort_table_)

#Create this mort table for regular employees who are either teachers or general employees
base_regular_mort_table <- (base_general_mort_table + base_teacher_mort_table)/2


# .. mortality improvement ------------------------------------------------

male_mp_table <- clean_mp_table(male_mp_table_, extend_2_yrs = T)
female_mp_table <- clean_mp_table(female_mp_table_, extend_2_yrs = T)

male_mp_final_table <- get_mp_final_table(male_mp_table, "male", 2010)
female_mp_final_table <- get_mp_final_table(female_mp_table, "female", 2010)

#.. mortality tables by class -----------------------------------------------

regular_mort_table <- get_mort_table("regular", base_regular_mort_table, male_mp_final_table, female_mp_final_table, regular_entrant_profile_table)
special_mort_table <- get_mort_table("special", base_safety_mort_table, male_mp_final_table, female_mp_final_table, special_entrant_profile_table)
admin_mort_table <- get_mort_table("admin", base_safety_mort_table, male_mp_final_table, female_mp_final_table, admin_entrant_profile_table)
eco_mort_table <- get_mort_table("eco", base_general_mort_table, male_mp_final_table, female_mp_final_table, eco_entrant_profile_table)
eso_mort_table <- get_mort_table("eso", base_general_mort_table, male_mp_final_table, female_mp_final_table, eso_entrant_profile_table)
judges_mort_table <- get_mort_table("judges", base_general_mort_table, male_mp_final_table, female_mp_final_table, judges_entrant_profile_table)
senior_management_mort_table <- get_mort_table("senior management", base_general_mort_table, male_mp_final_table, female_mp_final_table, senior_management_entrant_profile_table)


regular_mort_retire_table <- get_mort_retire_table(base_regular_mort_table, male_mp_final_table, female_mp_final_table)
special_mort_retire_table <- get_mort_retire_table(base_safety_mort_table, male_mp_final_table, female_mp_final_table)
admin_mort_retire_table <- get_mort_retire_table(base_safety_mort_table, male_mp_final_table, female_mp_final_table)
eco_mort_retire_table <- get_mort_retire_table(base_general_mort_table, male_mp_final_table, female_mp_final_table)
eso_mort_retire_table <- get_mort_retire_table(base_general_mort_table, male_mp_final_table, female_mp_final_table)
judges_mort_retire_table <- get_mort_retire_table(base_general_mort_table, male_mp_final_table, female_mp_final_table)
senior_management_mort_retire_table <- get_mort_retire_table(base_general_mort_table, male_mp_final_table, female_mp_final_table)

# Separation Assumptions --------------------------------------------------






