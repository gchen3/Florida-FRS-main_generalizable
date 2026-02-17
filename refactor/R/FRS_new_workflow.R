rm(list = ls())

# --- Libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)
library(purrr)
# remove.packages("pentools");devtools::install_github("gchen3/pentools")
library(pentools)
# remove.packages("pendata"); devtools::install_github("donboyd5/pendata")
library(pendata)

# --- Paths -------------------------------------------------------------------
iddir    <- here::here("refactor", "interim_data")
rdir     <- here::here("refactor", "R")
sddir    <- here::here("refactor", "source_data")
tooldir  <- here::here("refactor", "tools")
wddir    <- here::here("refactor", "working_data")
xidir    <- here::here("refactor", "source_data", "Reports", "extracted inputs")
stackdir <- here::here("refactor", "stacked_data")
outdir   <- here::here("refactor", "new_results")

if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# --- Load model parameters from Pandata -----------------------------------
params_env <- new.env()
params <- list2env(as.list(pendata::frs$params_env))

params$component_meta <- readxl::read_excel(
  here::here("refactor","R","component.xlsx"),
  sheet = "component_meta"
)

params$plan_alloc <- readxl::read_excel(
  here::here("refactor","R","component.xlsx"),
  sheet = "plan_alloc"
)

params$ref_year <- readxl::read_excel(
  path  = here::here("refactor", "R", "component.xlsx"),
  sheet = "salary_proj_reference_year"
)

params$get_fas <- function(salary_vec, fas_period) {
  x <- c(NA_real_, salary_vec[-length(salary_vec)])  # lag 1, preserve length
  RcppRoll::roll_mean(x, n = fas_period, align = "right", fill = NA_real_)
}

# --- Benefit model helpers ----------------------------------------------------
message("sourcing FRS_benefit_model_helper_functions and data function...")
bm_env <- new.env()
source(fs::path(rdir, "FRS_benefit_model_functions_2.R"), local = bm_env)

# --- Load workforce, liability, funding functions -----------------------------
message("Loading model functions...")

# Workforce
message("sourcing FRS_workforce_model_functions....")
wfm_env <- new.env()
source(fs::path(rdir, "FRS_workforce_model_functions_V3.R"), local = wfm_env)

# Liability
message("sourcing FRS_liability_model_functions...")
lm_env <- new.env()
source(fs::path(rdir, "FRS_liability_model_functions_2.R"), local = lm_env)

# Funding
message("sourcing funding model functions...")
fm_env <- new.env()
source(fs::path(rdir, "FRS_funding_amort.R"),                                local = fm_env)
source(fs::path(rdir, "FRS_funding_model_functions_loop_without_drop_V5.R"), local = fm_env)
source(fs::path(rdir, "FRS_funding_model_functions_drop_only.R"),            local = fm_env)

# --- Prepare benefit data for modeling -------------------------------------------------
message("sourcing FRS_benefit_get_saved_data.R...")
bf_data_env <- new.env()
source(fs::path(rdir, "FRS_benefit_model_get_and_save_bendata.R"), local = bf_data_env)

# --- Prepare workforce data for modeling -------------------------------------------------
message("sourcing FRS_workforce_get_saved_data.R...")
wf_data_env <- new.env()
source(fs::path(rdir, "FRS_workforce_model_get_and_save_wfdata_GC_s.R"), local = wf_data_env)

# --- Prepare liability data for modeling -------------------------------------------------
message("sourcing FRS_liability_get_saved_data.R...")
liab_data_env <- new.env()
source(fs::path(rdir, "FRS_liability_model_get_and_save_liabdata.R"), local = liab_data_env)

# --- Funding & amortization inputs --------------------------------------------
params$funding_list <- fm_env$get_all_classes_funding_list(params$init_funding_data, params)
params$current_amort_layers_table <- fm_env$get_current_amort_layers_summary_table(params$current_amort_layers_table_)

# --- Baseline results ----------------------------------------------------------
message("Calculating baseline funding results...")
params$enable_drop_ <- TRUE
baseline_funding <- fm_env$get_funding_data(liab_data_env, params)

# Save full workspace 
save.image(fs::path(outdir, "new_workspace.RData")) 

# --- Tests ---------------------------------------------------------------------
source(fs::path(tooldir, "run_allobjects_tests.R"))
