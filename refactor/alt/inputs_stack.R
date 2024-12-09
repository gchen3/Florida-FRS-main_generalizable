
# create stacked versions of the tables that are saved by FRS get_benefit_data()
#   ann_factor_table_stacked
#   ann_factor_retire_table_stacked
#   benefit_table_stacked
#   final_benefit_table_stacked   
#   benefit_val_table_stacked
#   indv_norm_cost_table_stacked
#   agg_norm_cost_table_stacked

# create tables in the current global environment, then save them in a stacked
# environment and delete the ones in the current environment

# overview
# fm_env$get funding data(...)
#   lm_env$get_liability_data(...)
#     bm_env$get_benefit_data(...)
#        get_class_salary_growth_table(class_name, params$salary_growth_table_) [within bm_env]


#.. unpack scalars into a tibble ----
#.... scalar groups are c("_ben_payment_current_", "_retiree_pop_current_", "_retiree_pop_current_")
scalar_groups <- c("_ben_payment_current_", "_retiree_pop_current_", "_retiree_pop_current_")
scalar_names <- outer(params$class_names_no_drop_frs_, scalar_groups, paste0) |> as.vector()
scalar_names

selected_elements <- mget(scalar_names, envir = params)

scalars_stacked <- enframe(selected_elements, name = "name", value = "value") |> 
  mutate(value=unlist(value)) |> 
  separate_class(col = "name", into = c("class", "variable")) |> 
  select(class, variable, value)

scalars_stacked


#.. workforce data, return list of 4 stacked data frames ----
# wf_active, wf_term, wf_refund, wf_retire
# each will have class, entry_age, age, year, and then vars such as  n_active

# this needs special handling because of the nested sets of 4 tibbles

wf_data_stacked_list <- ups_wfdata(params$wf_data_list)
# names(wf_data_stacked_list)
# wf_data_stacked_list$wf_active_stacked


#.. entrant_profile_table ----
# ns(params$entrant_profile_table_list) |> str_subset("entrant_profile_table")
entrant_profile_table_stacked <- stack_list(params$entrant_profile_table_list, "_entrant_profile_table")

#.. salary_growth_table GET RID OF RELIANCE ON frs$salary growth ----
salary_growth_table_stacked <- pendata::frs$salary_growth |> 
  select(class, yos, cumprod_salary_increase = cumprod_increase)

#.. salary_headcount_table ----
# ns(params$salary_headcount_table_list) |> str_subset("_salary_headcount_table")
salary_headcount_table_stacked <- stack_list(params$salary_headcount_table_list, "_salary_headcount_table")
# count(salary_headcount_table_stacked, class)
# params$salary_headcount_table_list$admin_salary_headcount_table # 26 admin rows

#.. mort_table ----
# ns(params$mort_table_list) |> str_subset("_mort_table")
mort_table_stacked <- stack_list(params$mort_table_list, "_mort_table")

#.. mort_retire_table ----
# ns(params$mort_retire_table_list) |> str_subset("_mort_retire_table")
mort_retire_table_stacked <- stack_list(params$mort_retire_table_list, "_mort_retire_table")

#.. separation_rate_table ----
# ns(params$separation_rate_table_list) |> str_subset("_separation_rate_table")
separation_rate_table_stacked <- stack_list(params$separation_rate_table_list, "_separation_rate_table")


# create and save a stacked environment ----
#   ann_factor_table_stacked
#   ann_factor_retire_table_stacked
#   benefit_table_stacked
#   final_benefit_table_stacked   
#   benefit_val_table_stacked
#   indv_norm_cost_table_stacked
#   agg_norm_cost_table_stacked

inputs_stack_env <- new.env()

list2env(wf_data_stacked_list, envir = .GlobalEnv)

data_list <- named_list(
  mort_retire_table_stacked,
  mort_table_stacked,
  entrant_profile_table_stacked,
  salary_growth_table_stacked,
  salary_headcount_table_stacked,
  scalars_stacked,
  separation_rate_table_stacked,
  wf_active_stacked,
  wf_refund_stacked,
  wf_retire_stacked,
  wf_term_stacked
  )

list2env(data_list, envir = inputs_stack_env)
ns(inputs_stack_env)
system.time(save(inputs_stack_env, file = fs::path(stackdir, "inputs_stack_env.RData")))

rm(wf_data_stacked_list)
ns(wf_data_stacked_list)

# delete intermediate objects ----


