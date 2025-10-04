rm(list = ls())

#Load required libraries ------------------
library(readxl)
library(tidyverse)
library(zoo)
library(profvis)
library(data.table)
library(openxlsx)
library(janitor)
library(rio)
library(parallel)
library(janitor)
library(gt)
library(purrr)
#devtools::install_github("donboyd5/btools")
library(btools) # ns, ht
# load pension-specific libraries
# remove.packages("pentools")
# devtools::install_github("gchen3/pentools")
library(pentools) # use this instead of sourcing "FRS_utility_functions.R"
# remove.packages("pendata")
# devtools::install_github("donboyd5/pendata")
library(pendata)


# define directories -------------------------------------------------------------

iddir <- here::here("refactor", "interim_data")
rdir <- here::here("refactor", "R")
sddir <- here::here("refactor", "source_data")
tooldir <- here::here("refactor", "tools")
wddir <- here::here("refactor", "working_data")
xidir <- here::here("refactor", "source_data", "Reports", "extracted inputs")
stackdir <- here::here("refactor", "stacked_data")

# if a full run, create model parameters and frs_data environments --------

# Get FRS model parameters, constants, raw initial data, and derived initial data

print("This is a full run using GC version, so creating model parameters and frs_data environments from scratch...")

modparm_data_env <- new.env()
# 48 objects - scalars and vectors
source(fs::path(rdir, "FRS_model_parameters_GC.R"), local = modparm_data_env) 

frs_data_env <- new.env()
# 13 secs only reads data and sets variable values - no functions
# 102 objects - mostly scalars and lists that are tables
source(fs::path(rdir, "FRS_data_cleaning_functions.R"), local = frs_data_env) # put data, params into frs_data_env
source(fs::path(rdir, "FRS_data_cleaning.R"), local = frs_data_env) # put data, params into frs_data_env
source(fs::path(rdir, "FRS_stacked_tables.R"), local = frs_data_env) # put data, params into frs_data_env
#source(fs::path(rdir, "FRS_rules_functions.R"), local = frs_data_env) # put data, params into frs_data_env
source(fs::path(rdir, "FRS_rules_tables.R"), local = frs_data_env) # put data, params into frs_data_env
source(fs::path(rdir, "FRS_liability_tables.R"), local = frs_data_env) # put data, params into frs_data_env
# ls(envir = modparm_data_env)

# create params environment -----------------------------------------------
# create params environment BEFORE FRS_benefit_model_actions.R, as it is needed
# for that

source(fs::path(rdir, "FRS_create_params_env_GC.R")) 

params <- pendata::frs$params_env

keep <- c("entrant_profile_table_s","salary_headcount_table",
  "mort_table","mort_retire_table","separation_rate_table",
  "dr_lookup","cola_lookup","ben_mult_lookup","reduce_factor_lookup",
  "tier_table","fas_period_lookup","entrant_profile_table")
rm(list = setdiff(ls(frs_data_env, all.names = TRUE), keep), envir = frs_data_env)

ns(params)
ns(frs_data_env)

# Get benefit model environment -----------------------------------------------
print("sourcing FRS_benefit_model_helper_functions.R and FRS_benefit_model_get_benefit_data_function.R...")
bm_env <- new.env()
#source(fs::path(rdir, "FRS_benefit_model_helper_functions.R"), local = bm_env) # only creates functions - no live code, puts them into the bm_env environment
source(fs::path(rdir, "FRS_benefit_model_get_benefit_data_function_GC_s.R"), local = bm_env) # only creates functions - no live code, also into the bm_env environment

# create derived data -----------------------------------------------

# get initial data derived from raw model data - does NOT require modeling assumptions
# print("sourcing FRS_benefit_model_actions.R...") 
# 
# benefit_model_data_env <- new.env()
# # uses: bm_env and params
# source(fs::path(rdir, "FRS_benefit_model_actions.R"), local = benefit_model_data_env)
# save(benefit_model_data_env, file = fs::path(wddir, "benefit_model_data_env.RData"))
# 
# load(fs::path(wddir, "benefit_model_data_env.RData"))
# list2env(as.list(benefit_model_data_env), envir = .GlobalEnv)
# rm(benefit_model_data_env)
# creates for each class: salary_headcount, entrant_profile, mort, retire_mort, drop entry, retire, early retire, sep rates


# Load workforce, liability, and funding model functions into separate environments -------------------------------

print("Loading model functions...")

# Note: do not load actuarial and financial functions as they are now in pentools

# Get workforce model
print("sourcing FRS_workforce_model_functions....")
wfm_env <- new.env()
source(fs::path(rdir, "FRS_workforce_model_functions_s.R"), local = wfm_env) # only creates function - no live code

#Get liability model
print("sourcing FRS_liability_model.R...")
lm_env <- new.env()
source(fs::path(rdir, "FRS_liability_model_functions_s.R"), local = lm_env) # only creates function - no live code

#Get funding model
print("sourcing FRS_funding_model_functions.R...")
fm_env <- new.env()
#source(fs::path(rdir, "FRS_funding_model_functions_s.R"), local = fm_env) # only creates function - no live code
source(fs::path(rdir, "FRS_funding_amort.R"), local = fm_env) # only creates function - no live code
source(fs::path(rdir, "FRS_funding_model_functions_loop_only.R"), local = fm_env) # only creates function - no live code

# Prepare data for modeling -----------------------------------------------

#Get workforce data (run this model only when workforce data is updated, otherwise use the rds files)
print("sourcing FRS_workforce_model_get_and_save_wfdata.R...")
system.time(source(fs::path(rdir, "FRS_workforce_model_get_and_save_wfdata_GC_s.R")))

# depends on assumptions in the model: 

cat("\n")
print("sourcing FRS_workforce_model_get_saved_data.R...")
wf_data_env <- new.env()
# djb NEXT: stack the workforce data ----
system.time(source(fs::path(rdir, "FRS_workforce_model_get_saved_data_s.R"), local = wf_data_env)) # < 1 sec -- only gets saved data - no functions
# simply loads wf data -- 4 table types per 7 classes

# prepare global lists to pass as needed - eventually replace with stacked data frames

# update params with new lists --------------------------------------------

# wf_data_list: each class has 4 tables: entrant_profile_table, salary_headcount_table, mort_table, separation_rate_table
# params$wf_data_list <- mget(paste0(params$class_names_no_drop_frs_, "_wf_data"), envir = wf_data_env) # does not waste memory because R is copy on modify
# params$entrant_profile_table_list <- frs_data_env$entrant_profile_table_list # previously created
# 
# params$salary_headcount_table_list <- mget(paste0(params$class_names_no_drop_frs_, "_salary_headcount_table"), envir = frs_data_env)
# 
# params$mort_table_list <- mget(paste0(params$class_names_no_drop_frs_, "_mort_table"), envir = frs_data_env)
# params$mort_retire_table_list <- mget(paste0(params$class_names_no_drop_frs_, "_mort_retire_table"), envir = frs_data_env)
# 
# params$separation_rate_table_list <- mget(paste0(params$class_names_no_drop_frs_, "_separation_rate_table"), envir = frs_data_env)
# 
# params$normal_retire_rate_tier_1_table_list <- mget(paste0(params$class_names_no_drop_frs_, "_normal_retire_rate_tier_1_table"), envir = frs_data_env) # defined in benefit model actions
# params$normal_retire_rate_tier_2_table_list <- mget(paste0(params$class_names_no_drop_frs_, "_normal_retire_rate_tier_2_table"), envir = frs_data_env) # defined in benefit model actions
# 
# params$early_retire_rate_tier_1_table_list <- mget(paste0(params$class_names_no_drop_frs_, "_early_retire_rate_tier_1_table"), envir = frs_data_env) # defined in benefit model actions
# params$early_retire_rate_tier_2_table_list <- mget(paste0(params$class_names_no_drop_frs_, "_early_retire_rate_tier_2_table"), envir = frs_data_env) # defined in benefit model actions

# get funding and amortization data --------------------------------------------
params$funding_list <- fm_env$get_all_classes_funding_list(params$init_funding_data, params)
params$current_amort_layers_table <- fm_env$get_current_amort_layers_summary_table(params$current_amort_layers_table_)

# ns(.GlobalEnv) |> str_subset("separation_rate_table")
# ns(benefit_model_data_env)

print("Done building model...")

# generate baseline results
baseline_funding <- fm_env$get_funding_data(params = params, return = "stacked")

save.image(here::here("refactor", "new_results", "new_workspace.RData")) # save the entire workspace, ~ 15-20 secs

# run one or the other of the following tests

# system.time(source(fs::path(tooldir, "run_tests.R"))) # run selected tests
# tooldir <- here::here("refactor", "tools") # run if needed
source(fs::path(tooldir, "run_allobjects_tests.R")) # run tests on all objects appropriate to compare ~ 15 secs
