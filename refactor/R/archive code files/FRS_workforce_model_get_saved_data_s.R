
# regular_wf_data <- readRDS(fs::path(iddir, "regular_wf_data.rds"))
# special_wf_data <- readRDS(fs::path(iddir, "special_wf_data.rds"))
# admin_wf_data <- readRDS(fs::path(iddir, "admin_wf_data.rds"))
# eco_wf_data <- readRDS(fs::path(iddir, "eco_wf_data.rds"))
# eso_wf_data <- readRDS(fs::path(iddir, "eso_wf_data.rds"))
# judges_wf_data <- readRDS(fs::path(iddir, "judges_wf_data.rds"))
# senior_management_wf_data <- readRDS(fs::path(iddir, "senior_management_wf_data.rds"))

wf_active_df_s <- bind_rows(
  regular = wf_data_env$regular_wf_data$wf_active_df,
  special = wf_data_env$special_wf_data$wf_active_df,
  admin = wf_data_env$admin_wf_data$wf_active_df,
  eco = wf_data_env$eco_wf_data$wf_active_df,
  eso = wf_data_env$eso_wf_data$wf_active_df,
  judges = wf_data_env$judges_wf_data$wf_active_df,
  senior_management = wf_data_env$senior_management_wf_data$wf_active_df
  ,
  .id = "class"
)

wf_term_df_s <- bind_rows(
  regular =wf_data_env$regular_wf_data$wf_term_df,
  special =wf_data_env$special_wf_data$wf_term_df,
  admin =wf_data_env$admin_wf_data$wf_term_df,
  eco =wf_data_env$eco_wf_data$wf_term_df,
  eso =wf_data_env$eso_wf_data$wf_term_df,
  judges =wf_data_env$judges_wf_data$wf_term_df,
  senior_management =wf_data_env$senior_management_wf_data$wf_term_df,
  .id = "class"
)

wf_refund_df_s <- bind_rows(
  regular =wf_data_env$regular_wf_data$wf_refund_df,
  special =wf_data_env$special_wf_data$wf_refund_df,
  admin =wf_data_env$admin_wf_data$wf_refund_df,
  eco =wf_data_env$eco_wf_data$wf_refund_df,
  eso =wf_data_env$eso_wf_data$wf_refund_df,
  judges =wf_data_env$judges_wf_data$wf_refund_df,
  senior_management =wf_data_env$senior_management_wf_data$wf_refund_df,
  .id = "class"
)

wf_retire_df_s <- bind_rows(
  regular =wf_data_env$regular_wf_data$wf_retire_df,
  special =wf_data_env$special_wf_data$wf_retire_df,
  admin =wf_data_env$admin_wf_data$wf_retire_df,
  eco =wf_data_env$eco_wf_data$wf_retire_df,
  eso =wf_data_env$eso_wf_data$wf_retire_df,
  judges =wf_data_env$judges_wf_data$wf_retire_df,
  senior_management =wf_data_env$senior_management_wf_data$wf_retire_df,
  .id = "class"
)

