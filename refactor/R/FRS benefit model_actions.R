# Salary & Headcount Processing -------------------------------------------

#Calculate salary cumulative growth 
# salary_growth_table <- salary_growth_table_ %>% 
#   bind_rows(tibble(yos = (max(salary_growth_table_$yos)+1):max(yos_range_))) %>% 
#   fill(everything(), .direction = "down") %>% 
#   mutate(across(contains("salary"), ~ cumprod(1 + lag(.x, default = 0)), .names = "cumprod_{.col}"), .keep = "unused")


#Joining headcount data, salary data, and salary growth data


#.. call get_salary_headcount_table -----------------------------------------

# if (!class_name %in% c("eco", "eso", "judges")) {
#   assign("total_active_member", get(paste0(class_name, "_total_active_member_")))
# } else {
#   assign("total_active_member", get("eco_eso_judges_total_active_member_"))
# }

print("get salary_headcount and entrant_profile tables")
temp <- get_salary_headcount_table("regular", params)
regular_salary_headcount_table <- temp$salary_headcount_table
regular_entrant_profile_table <- temp$entrant_profile

temp <- get_salary_headcount_table("special", params)
special_salary_headcount_table <- temp$salary_headcount_table
special_entrant_profile_table <- temp$entrant_profile

temp <- get_salary_headcount_table("admin", params)
admin_salary_headcount_table <- temp$salary_headcount_table
admin_entrant_profile_table <- temp$entrant_profile

temp <- get_salary_headcount_table("eco", params)
eco_salary_headcount_table <- temp$salary_headcount_table
eco_entrant_profile_table <- temp$entrant_profile

temp <- get_salary_headcount_table("eso", params)
eso_salary_headcount_table <- temp$salary_headcount_table
eso_entrant_profile_table <- temp$entrant_profile

temp <- get_salary_headcount_table("judges", params)
judges_salary_headcount_table <- temp$salary_headcount_table
judges_entrant_profile_table <- temp$entrant_profile

temp <- get_salary_headcount_table("senior management", params)
senior_management_salary_headcount_table <- temp$salary_headcount_table
senior_management_entrant_profile_table <- temp$entrant_profile

rm(temp)


# Retirement & Separation Conditions --------------------------------------

# no actions related to this?


# Mortality Assumptions ---------------------------------------------------

print("get mortality tables")

#.. base mortality table -----------------------------------------------

base_general_mort_table <- get_base_mort_table(params$base_general_mort_table_)
base_teacher_mort_table <- get_base_mort_table(params$base_teacher_mort_table_)
base_safety_mort_table <- get_base_mort_table(params$base_safety_mort_table_)

#Create this mort table for regular employees who are either teachers or general employees
base_regular_mort_table <- (base_general_mort_table + base_teacher_mort_table)/2


# .. mortality improvement ------------------------------------------------

male_mp_table <- clean_mp_table(params$male_mp_table_, extend_2_yrs = TRUE)
female_mp_table <- clean_mp_table(params$female_mp_table_, extend_2_yrs = TRUE)

male_mp_final_table <- get_mp_final_table(male_mp_table, "male", 2010, params$age_range_, params$year_range_)
female_mp_final_table <- get_mp_final_table(female_mp_table, "female", 2010, params$age_range_, params$year_range_)

#.. mortality tables by class -----------------------------------------------

regular_mort_table <- get_mort_table("regular", base_regular_mort_table, male_mp_final_table, female_mp_final_table, regular_entrant_profile_table,
                                     params$entry_year_range_, params$age_range_, params$yos_range_, params$new_year_)
special_mort_table <- get_mort_table("special", base_safety_mort_table, male_mp_final_table, female_mp_final_table, special_entrant_profile_table,
                                     params$entry_year_range_, params$age_range_, params$yos_range_, params$new_year_)
admin_mort_table <- get_mort_table("admin", base_safety_mort_table, male_mp_final_table, female_mp_final_table, admin_entrant_profile_table,
                                   params$entry_year_range_, params$age_range_, params$yos_range_, params$new_year_)
eco_mort_table <- get_mort_table("eco", base_general_mort_table, male_mp_final_table, female_mp_final_table, eco_entrant_profile_table,
                                 params$entry_year_range_, params$age_range_, params$yos_range_, params$new_year_)
eso_mort_table <- get_mort_table("eso", base_general_mort_table, male_mp_final_table, female_mp_final_table, eso_entrant_profile_table,
                                 params$entry_year_range_, params$age_range_, params$yos_range_, params$new_year_)
judges_mort_table <- get_mort_table("judges", base_general_mort_table, male_mp_final_table, female_mp_final_table, judges_entrant_profile_table,
                                    params$entry_year_range_, params$age_range_, params$yos_range_, params$new_year_)
senior_management_mort_table <- get_mort_table("senior management", base_general_mort_table, male_mp_final_table, female_mp_final_table, senior_management_entrant_profile_table,
                                               params$entry_year_range_, params$age_range_, params$yos_range_, params$new_year_)


regular_mort_retire_table <- get_mort_retire_table(base_regular_mort_table, male_mp_final_table, female_mp_final_table,
                                                   params$age_range_, params$year_range_, params$start_year_)
special_mort_retire_table <- get_mort_retire_table(base_safety_mort_table, male_mp_final_table, female_mp_final_table,
                                                   params$age_range_, params$year_range_, params$start_year_)
admin_mort_retire_table <- get_mort_retire_table(base_safety_mort_table, male_mp_final_table, female_mp_final_table,
                                                 params$age_range_, params$year_range_, params$start_year_)
eco_mort_retire_table <- get_mort_retire_table(base_general_mort_table, male_mp_final_table, female_mp_final_table,
                                               params$age_range_, params$year_range_, params$start_year_)
eso_mort_retire_table <- get_mort_retire_table(base_general_mort_table, male_mp_final_table, female_mp_final_table,
                                               params$age_range_, params$year_range_, params$start_year_)
judges_mort_retire_table <- get_mort_retire_table(base_general_mort_table, male_mp_final_table, female_mp_final_table,
                                                  params$age_range_, params$year_range_, params$start_year_)
senior_management_mort_retire_table <- get_mort_retire_table(base_general_mort_table, male_mp_final_table, female_mp_final_table,
                                                             params$age_range_, params$year_range_, params$start_year_)


# Separation Assumptions --------------------------------------------------

print("get separation tables")

#.. Clean retirement and drop ----

drop_entry_table_col_names <- c("age", "regular_inst_female", "regular_inst_male",
                                "regular_non_inst_female", "regular_non_inst_male",
                                "special_risk_non_leo_female", "special_risk_non_leo_male",
                                "special_risk_leo_female", "special_risk_leo_male",
                                "other_female", "other_male")

normal_retire_table_col_names <- c("age", "regular_inst_female", "regular_inst_male",
                                   "regular_non_inst_female", "regular_non_inst_male",
                                   "special_risk_female", "special_risk_male",
                                   "eco_eso_jud_female", "eco_eso_jud_male",
                                   "senior_management_female", "senior_management_male")

early_retire_table_col_names <- c("age", "regular_non_inst_female", "regular_non_inst_male",
                                  "special_risk_female", "special_risk_male",
                                  "eco_eso_jud_female", "eco_eso_jud_male",
                                  "senior_management_female", "senior_management_male")


drop_entry_tier_1_table <- clean_retire_rate_table(params$drop_entry_tier_1_table_, drop_entry_table_col_names)
drop_entry_tier_2_table <- clean_retire_rate_table(params$drop_entry_tier_2_table_, drop_entry_table_col_names)

normal_retire_rate_tier_1_table <- clean_retire_rate_table(params$normal_retirement_tier_1_table_, normal_retire_table_col_names)
normal_retire_rate_tier_2_table <- clean_retire_rate_table(params$normal_retirement_tier_2_table_, normal_retire_table_col_names)

early_retire_rate_tier_1_table <- clean_retire_rate_table(params$early_retirement_tier_1_table_, early_retire_table_col_names)
early_retire_rate_tier_2_table <- clean_retire_rate_table(params$early_retirement_tier_2_table_, early_retire_table_col_names)


normal_retire_rate_tier_2_table <- normal_retire_rate_tier_2_table %>% 
  add_row(age=45:49, .before=1) %>% 
  mutate(across(everything(), ~replace(.x, is.na(.x), 0)))
# mutate_all(~replace(., is.na(.), 0))

special_risk_drop_entry_tier_1_table <- drop_entry_tier_1_table %>% 
  select(age, contains("special_risk")) %>% 
  mutate(
    special_risk_female = (special_risk_non_leo_female + special_risk_leo_female)/2,
    special_risk_male = (special_risk_non_leo_male + special_risk_leo_male)/2,
    .keep = "unused"
  )

special_risk_drop_entry_tier_2_table <- drop_entry_tier_2_table %>% 
  select(age, contains("special_risk")) %>% 
  mutate(
    special_risk_female = (special_risk_non_leo_female + special_risk_leo_female)/2,
    special_risk_male = (special_risk_non_leo_male + special_risk_leo_male)/2,
    .keep = "unused"
  )


#.. get retirement-rate tables ----------------------------------------------


regular_normal_retire_rate_tier_1_table <- get_normal_retire_rate_table(class_name = "regular",
                                                                        drop_entry_table = drop_entry_tier_1_table,
                                                                        normal_retire_rate_table = normal_retire_rate_tier_1_table)

regular_normal_retire_rate_tier_2_table <- get_normal_retire_rate_table(class_name = "regular",
                                                                        drop_entry_table = drop_entry_tier_2_table,
                                                                        normal_retire_rate_table = normal_retire_rate_tier_2_table)

special_normal_retire_rate_tier_1_table <- get_normal_retire_rate_table(class_name = "special",
                                                                        drop_entry_table = special_risk_drop_entry_tier_1_table,
                                                                        normal_retire_rate_table = normal_retire_rate_tier_1_table)

special_normal_retire_rate_tier_2_table <- get_normal_retire_rate_table(class_name = "special",
                                                                        drop_entry_table = special_risk_drop_entry_tier_2_table,
                                                                        normal_retire_rate_table = normal_retire_rate_tier_2_table)

admin_normal_retire_rate_tier_1_table <- get_normal_retire_rate_table(class_name = "admin",
                                                                      drop_entry_table = special_risk_drop_entry_tier_1_table,
                                                                      normal_retire_rate_table = normal_retire_rate_tier_1_table)

admin_normal_retire_rate_tier_2_table <- get_normal_retire_rate_table(class_name = "admin",
                                                                      drop_entry_table = special_risk_drop_entry_tier_2_table,
                                                                      normal_retire_rate_table = normal_retire_rate_tier_2_table)

eco_normal_retire_rate_tier_1_table <- get_normal_retire_rate_table(class_name = "eco",
                                                                    drop_entry_table = drop_entry_tier_1_table,
                                                                    normal_retire_rate_table = normal_retire_rate_tier_1_table)

eco_normal_retire_rate_tier_2_table <- get_normal_retire_rate_table(class_name = "eco",
                                                                    drop_entry_table = drop_entry_tier_2_table,
                                                                    normal_retire_rate_table = normal_retire_rate_tier_2_table)

eso_normal_retire_rate_tier_1_table <- get_normal_retire_rate_table(class_name = "eso",
                                                                    drop_entry_table = drop_entry_tier_1_table,
                                                                    normal_retire_rate_table = normal_retire_rate_tier_1_table)

eso_normal_retire_rate_tier_2_table <- get_normal_retire_rate_table(class_name = "eso",
                                                                    drop_entry_table = drop_entry_tier_2_table,
                                                                    normal_retire_rate_table = normal_retire_rate_tier_2_table)

judges_normal_retire_rate_tier_1_table <- get_normal_retire_rate_table(class_name = "judge",
                                                                       drop_entry_table = drop_entry_tier_1_table,
                                                                       normal_retire_rate_table = normal_retire_rate_tier_1_table)

judges_normal_retire_rate_tier_2_table <- get_normal_retire_rate_table(class_name = "judge",
                                                                       drop_entry_table = drop_entry_tier_2_table,
                                                                       normal_retire_rate_table = normal_retire_rate_tier_2_table)


senior_management_normal_retire_rate_tier_1_table <- get_normal_retire_rate_table(class_name = "senior management",
                                                                                  drop_entry_table = drop_entry_tier_1_table,
                                                                                  normal_retire_rate_table = normal_retire_rate_tier_1_table)

senior_management_normal_retire_rate_tier_2_table <- get_normal_retire_rate_table(class_name = "senior management",
                                                                                  drop_entry_table = drop_entry_tier_2_table,
                                                                                  normal_retire_rate_table = normal_retire_rate_tier_2_table)

#.. get early retirement rate tables ----------------------------------------


regular_early_retire_rate_tier_1_table <- get_early_retire_rate_table(class_name = "regular",
                                                                      init_early_retire_rate_table = early_retire_rate_tier_1_table)

regular_early_retire_rate_tier_2_table <- get_early_retire_rate_table(class_name = "regular",
                                                                      init_early_retire_rate_table = early_retire_rate_tier_2_table)


special_early_retire_rate_tier_1_table <- get_early_retire_rate_table(class_name = "special",
                                                                      init_early_retire_rate_table = early_retire_rate_tier_1_table)

special_early_retire_rate_tier_2_table <- get_early_retire_rate_table(class_name = "special",
                                                                      init_early_retire_rate_table = early_retire_rate_tier_2_table)


admin_early_retire_rate_tier_1_table <- get_early_retire_rate_table(class_name = "admin",
                                                                    init_early_retire_rate_table = early_retire_rate_tier_1_table)

admin_early_retire_rate_tier_2_table <- get_early_retire_rate_table(class_name = "admin",
                                                                    init_early_retire_rate_table = early_retire_rate_tier_2_table)

eco_early_retire_rate_tier_1_table <- get_early_retire_rate_table(class_name = "eco",
                                                                  init_early_retire_rate_table = early_retire_rate_tier_1_table)

eco_early_retire_rate_tier_2_table <- get_early_retire_rate_table(class_name = "eco",
                                                                  init_early_retire_rate_table = early_retire_rate_tier_2_table)

eso_early_retire_rate_tier_1_table <- get_early_retire_rate_table(class_name = "eso",
                                                                  init_early_retire_rate_table = early_retire_rate_tier_1_table)

eso_early_retire_rate_tier_2_table <- get_early_retire_rate_table(class_name = "eso",
                                                                  init_early_retire_rate_table = early_retire_rate_tier_2_table)

judges_early_retire_rate_tier_1_table <- get_early_retire_rate_table(class_name = "judge",
                                                                     init_early_retire_rate_table = early_retire_rate_tier_1_table)

judges_early_retire_rate_tier_2_table <- get_early_retire_rate_table(class_name = "judge",
                                                                     init_early_retire_rate_table = early_retire_rate_tier_2_table)


senior_management_early_retire_rate_tier_1_table <- get_early_retire_rate_table(class_name = "senior management",
                                                                                init_early_retire_rate_table = early_retire_rate_tier_1_table)

senior_management_early_retire_rate_tier_2_table <- get_early_retire_rate_table(class_name = "senior management",
                                                                                init_early_retire_rate_table = early_retire_rate_tier_2_table)


# ..get separation rate tables --------------------------------------------

regular_separation_rate_table <- get_separation_table("regular", age_range_, entry_year_range_, yos_range_, new_year_)
special_separation_rate_table <- get_separation_table("special", age_range_, entry_year_range_, yos_range_, new_year_)
admin_separation_rate_table <- get_separation_table("admin", age_range_, entry_year_range_, yos_range_, new_year_)
eco_separation_rate_table <- get_separation_table("eco", age_range_, entry_year_range_, yos_range_, new_year_)
eso_separation_rate_table <- get_separation_table("regular", age_range_, entry_year_range_, yos_range_, new_year_)
judges_separation_rate_table <- get_separation_table("judges", age_range_, entry_year_range_, yos_range_, new_year_)
senior_management_separation_rate_table <- get_separation_table("senior management", age_range_, entry_year_range_, yos_range_, new_year_)

print("All done with separation tables")

