benefit_data_s <- bm_env$get_benefit_data_s(
  params$entrant_profile_table,
  params$salary_headcount_table,
  params$mort_table,
  params$mort_retire_table,
  params$separation_rate_table,
  params
)

# Get and save workforce data for Government of Canada employees (special, regular, admin, eco, eso, judges, senior management) - simplified
wf_data_s <- params$class_names_no_drop_frs_ |> 
  set_names() |> 
  map(wfm_env$get_wf_data_s, params = params)

wf_active_df_s <- wf_data_s |>
  map("wf_active_df") |>
  bind_rows(.id = "class")

wf_term_df_s <- wf_data_s |>
  map("wf_term_df") |>
  bind_rows(.id = "class")

wf_refund_df_s <- wf_data_s |>
  map("wf_refund_df") |>
  bind_rows(.id = "class")

wf_retire_df_s <- wf_data_s |>
  map("wf_retire_df") |>
  bind_rows(.id = "class")
# GC: below is just for comparison purpuses, can be deleted later
regular_wf_data <- wf_data_s$regular
special_wf_data <- wf_data_s$special
admin_wf_data <- wf_data_s$admin
eco_wf_data <- wf_data_s$eco
eso_wf_data <- wf_data_s$eso
judges_wf_data <- wf_data_s$judges
senior_management_wf_data <- wf_data_s$senior_management
# End of comparison



# 
# 
# regular_wf_data <- wfm_env$get_wf_data_s(class_name = "regular", params = params)
# special_wf_data <- wfm_env$get_wf_data_s(class_name = "special", params = params)
# admin_wf_data <- wfm_env$get_wf_data_s(class_name = "admin", params = params)
# eco_wf_data <- wfm_env$get_wf_data_s(class_name = "eco", params = params)
# eso_wf_data <- wfm_env$get_wf_data_s(class_name = "eso", params = params)
# judges_wf_data <- wfm_env$get_wf_data_s(class_name = "judges", params = params)
# senior_management_wf_data <- wfm_env$get_wf_data_s(class_name = "senior_management", params = params)
# 
# wf_active_df_s <- bind_rows(
#   regular = regular_wf_data$wf_active_df,
#   special = special_wf_data$wf_active_df,
#   admin = admin_wf_data$wf_active_df,
#   eco = eco_wf_data$wf_active_df,
#   eso = eso_wf_data$wf_active_df,
#   judges = judges_wf_data$wf_active_df,
#   senior_management = senior_management_wf_data$wf_active_df
#   ,
#   .id = "class"
# )
# 
# wf_term_df_s <- bind_rows(
#   regular =regular_wf_data$wf_term_df,
#   special =special_wf_data$wf_term_df,
#   admin =admin_wf_data$wf_term_df,
#   eco =eco_wf_data$wf_term_df,
#   eso =eso_wf_data$wf_term_df,
#   judges =judges_wf_data$wf_term_df,
#   senior_management =senior_management_wf_data$wf_term_df,
#   .id = "class"
# )
# 
# wf_refund_df_s <- bind_rows(
#   regular =regular_wf_data$wf_refund_df,
#   special =special_wf_data$wf_refund_df,
#   admin =admin_wf_data$wf_refund_df,
#   eco =eco_wf_data$wf_refund_df,
#   eso =eso_wf_data$wf_refund_df,
#   judges =judges_wf_data$wf_refund_df,
#   senior_management =senior_management_wf_data$wf_refund_df,
#   .id = "class"
# )
# 
# wf_retire_df_s <- bind_rows(
#   regular =regular_wf_data$wf_retire_df,
#   special =special_wf_data$wf_retire_df,
#   admin =admin_wf_data$wf_retire_df,
#   eco =eco_wf_data$wf_retire_df,
#   eso =eso_wf_data$wf_retire_df,
#   judges =judges_wf_data$wf_retire_df,
#   senior_management =senior_management_wf_data$wf_retire_df,
#   .id = "class"
# )