rm(list = ls())

# --- Libraries ---------------------------------------------------------------
library(readxl)
library(tidyverse)
library(zoo)
library(profvis)
library(data.table)
library(openxlsx)
library(janitor)
library(rio)
library(parallel)
library(gt)
library(purrr)
# devtools::install_github("donboyd5/btools")
library(btools)
# remove.packages("pentools"); devtools::install_github("gchen3/pentools")
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

# --- Build model parameters & frs_data envs -----------------------------------
message("This is a full run using GC version, so creating model parameters and frs_data environments from scratch...")

params <- pendata::frs$params_env

frs_data_env <- list2env(readRDS(fs::path(wddir, "frs_data_env.rds")), parent = emptyenv())

ns(params)
ns(frs_data_env)

# --- Benefit model helpers ----------------------------------------------------
message("sourcing FRS_benefit_model_helper_functions and data function...")
bm_env <- new.env()
source(fs::path(rdir, "FRS_benefit_model_get_benefit_data_function_GC_s.R"), local = bm_env)

# --- Load workforce, liability, funding functions -----------------------------
message("Loading model functions...")

# Workforce
message("sourcing FRS_workforce_model_functions....")
wfm_env <- new.env()
source(fs::path(rdir, "FRS_workforce_model_functions_V3.R"), local = wfm_env)

# Liability
message("sourcing FRS_liability_model_functions...")
lm_env <- new.env()
source(fs::path(rdir, "FRS_liability_model_functions_s.R"), local = lm_env)

# Funding
message("sourcing funding model functions...")
fm_env <- new.env()
source(fs::path(rdir, "FRS_funding_amort.R"),                                local = fm_env)
source(fs::path(rdir, "FRS_funding_model_functions_loop_without_drop_V5.R"), local = fm_env)
source(fs::path(rdir, "FRS_funding_model_functions_drop_only.R"),            local = fm_env)

# --- Prepare data for modeling -------------------------------------------------
message("sourcing FRS_workforce_model_get_and_save_wfdata.R...")
source(fs::path(rdir, "FRS_workforce_model_get_and_save_wfdata_GC_s.R"))

message("sourcing FRS_workforce_model_get_saved_data.R...")
wf_data_env <- new.env()
source(fs::path(rdir, "FRS_workforce_model_get_saved_data_s.R"), local = wf_data_env)

# --- Funding & amortization inputs --------------------------------------------
params$funding_list <- fm_env$get_all_classes_funding_list(params$init_funding_data, params)
params$current_amort_layers_table <- fm_env$get_current_amort_layers_summary_table(params$current_amort_layers_table_)

message("Done building model...")

# --- Baseline results ----------------------------------------------------------
params$enable_drop_ <- TRUE
baseline_funding <- fm_env$get_funding_data(params = params, return = "stacked")

# Save full workspace 
save.image(fs::path(outdir, "new_workspace.RData")) 

# --- Tests ---------------------------------------------------------------------
source(fs::path(tooldir, "run_allobjects_tests.R"))
