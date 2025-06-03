wfm_env$get_wf_data_s(class_name = "regular", params = params)
wfm_env$get_wf_data_s(class_name = "special", params = params)
wfm_env$get_wf_data_s(class_name = "admin", params = params)
wfm_env$get_wf_data_s(class_name = "eco", params = params)
wfm_env$get_wf_data_s(class_name = "eso", params = params)
wfm_env$get_wf_data_s(class_name = "judges", params = params)
wfm_env$get_wf_data_s(class_name = "senior_management", params = params)

# wf_active_bf_s <- bind_rows(
#   regular = wf_data$regular_wf_data$wf_active_df,
#   special = wf_data$special_wf_data$wf_active_df,
#   admin = wf_data$admin_wf_data$wf_active_df,
#   eco = wf_data$eco_wf_data$wf_active_df,
#   eso = wf_data$eso_wf_data$wf_active_df,
#   judges = wf_data$judges_wf_data$wf_active_df,
#   senior_mgmt = wf_data$senior_management_wf_data$wf_active_df,
#   .id = "class"
# )
# 
# wf_term_df_s <- bind_rows(
#   regular = wf_data$regular_wf_data$wf_term_df,
#   special = wf_data$special_wf_data$wf_term_df,
#   admin = wf_data$admin_wf_data$wf_term_df,
#   eco = wf_data$eco_wf_data$wf_term_df,
#   eso = wf_data$eso_wf_data$wf_term_df,
#   judges = wf_data$judges_wf_data$wf_term_df,
#   senior_mgmt = wf_data$senior_management_wf_data$wf_term_df,
#   .id = "class"
# )
# 
# wf_refund_df_s <- bind_rows(
#   regular = wf_data$regular_wf_data$wf_refund_df,
#   special = wf_data$special_wf_data$wf_refund_df,
#   admin = wf_data$admin_wf_data$wf_refund_df,
#   eco = wf_data$eco_wf_data$wf_refund_df,
#   eso = wf_data$eso_wf_data$wf_refund_df,
#   judges = wf_data$judges_wf_data$wf_refund_df,
#   senior_mgmt = wf_data$senior_management_wf_data$wf_refund_df,
#   .id = "class"
# )
# 
# wf_retire_df_s <- bind_rows(
#   regular = wf_data$regular_wf_data$wf_retire_df,
#   special = wf_data$special_wf_data$wf_retire_df,
#   admin = wf_data$admin_wf_data$wf_retire_df,
#   eco = wf_data$eco_wf_data$wf_retire_df,
#   eso = wf_data$eso_wf_data$wf_retire_df,
#   judges = wf_data$judges_wf_data$wf_retire_df,
#   senior_mgmt = wf_data$senior_management_wf_data$wf_retire_df,
#   .id = "class"
# )
# wfm_env$get_wf_data(class_name = "regular", 
#                     entrant_profile_table = frs_data_env$entrant_profile_table,
#                     salary_headcount_table = frs_data_env$salary_headcount_table,
#                     mort_table = frs_data_env$mort_table,
#                     mort_retire_table = frs_data_env$mort_retire_table,
#                     separation_rate_table = frs_data_env$separation_rate_table,
#                     params = params)
# 
# wfm_env$get_wf_data(class_name = "special", 
#                     entrant_profile_table = frs_data_env$entrant_profile_table,
#                     salary_headcount_table = frs_data_env$salary_headcount_table,
#                     mort_table = frs_data_env$mort_table,
#                     mort_retire_table = frs_data_env$mort_retire_table,
#                     separation_rate_table = frs_data_env$separation_rate_table,
#                     params = params)
# 
# wfm_env$get_wf_data(class_name = "admin", 
#                     entrant_profile_table = frs_data_env$entrant_profile_table,
#                     salary_headcount_table = frs_data_env$salary_headcount_table,
#                     mort_table = frs_data_env$mort_table,
#                     mort_retire_table = frs_data_env$mort_retire_table,
#                     separation_rate_table = frs_data_env$separation_rate_table,
#                     params = params)
# 
# wfm_env$get_wf_data(class_name = "eco", 
#                     entrant_profile_table = frs_data_env$entrant_profile_table,
#                     salary_headcount_table = frs_data_env$salary_headcount_table,
#                     mort_table = frs_data_env$mort_table,
#                     mort_retire_table = frs_data_env$mort_retire_table,
#                     separation_rate_table = frs_data_env$separation_rate_table,
#                     params = params)
# 
# wfm_env$get_wf_data(class_name = "eso", 
#                     entrant_profile_table = frs_data_env$entrant_profile_table,
#                     salary_headcount_table = frs_data_env$salary_headcount_table,
#                     mort_table = frs_data_env$mort_table,
#                     mort_retire_table = frs_data_env$mort_retire_table,
#                     separation_rate_table = frs_data_env$separation_rate_table,
#                     params = params)
# 
# wfm_env$get_wf_data(class_name = "judges", 
#                     entrant_profile_table = frs_data_env$entrant_profile_table,
#                     salary_headcount_table = frs_data_env$salary_headcount_table,
#                     mort_table = frs_data_env$mort_table,
#                     mort_retire_table = frs_data_env$mort_retire_table,
#                     separation_rate_table = frs_data_env$separation_rate_table,
#                     params = params)
# 
# wfm_env$get_wf_data(class_name = "senior_management", 
#                     entrant_profile_table = frs_data_env$entrant_profile_table,
#                     salary_headcount_table = frs_data_env$salary_headcount_table,
#                     mort_table = frs_data_env$mort_table,
#                     mort_retire_table = frs_data_env$mort_retire_table,
#                     separation_rate_table = frs_data_env$separation_rate_table,
#                     params = params)
# 
