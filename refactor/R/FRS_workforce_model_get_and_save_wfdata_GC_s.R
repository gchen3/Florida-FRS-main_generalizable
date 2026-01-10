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