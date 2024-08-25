# rm(list = ls())

#Loading required libraries ------------------
library(readxl)
library(tidyverse)
library(zoo)
library(profvis)
library(data.table)
library(openxlsx)
library(janitor)
library(rio)
library(parallel)

# boyd additions to libraries
library(purrr)
library(btools) # ns, ht

# load pension-specific libraries
# For first time installing package, or each time package is updated, install as follows
# devtools::install(pkg = here::here("pentools"))
library(pentools) # use this instead of sourcing "FRS utility_functions.R"


# set FULL_RUN boolean ----------------------------------------------------

FULL_RUN <- FALSE
BENEFIT_RUN <- TRUE


# directories -------------------------------------------------------------

iddir <- here::here("refactor", "interim_data")
rdir <- here::here("refactor", "R")
sddir <- here::here("refactor", "source_data")
tooldir <- here::here("refactor", "tools")
wddir <- here::here("refactor", "working_data")
xidir <- here::here("refactor", "source_data", "Reports", "extracted inputs")


# Load functions ---------------------------------------------------------------

print("Loading model functions...")

#Get actuarial and financial functions
# print("NOT sourcing FRS utility_functions.R but loading pentools...")
# source(fs::path(rdir, "FRS utility_functions.R")) # only creates functions - no live code

#Get benefit model
print("sourcing FRS benefit model_helper_functions.R...")
source(fs::path(rdir, "FRS benefit model_helper_functions.R")) # only creates functions - no live code

print("sourcing FRS benefit model_get_benefit_data_function.R...")
source(fs::path(rdir, "FRS benefit model_get_benefit_data_function.R")) # only creates functions - no live code

# Get workforce model
print("sourcing FRS workforce model_functions....")
source(fs::path(rdir, "FRS workforce model_functions.R")) # only creates function - no live code

#Get liability model
print("sourcing FRS liability model.R...")
source(fs::path(rdir, "FRS liability model.R")) # only creates function - no live code

#Get funding model
print("sourcing FRS funding model_functions.R...")
source(fs::path(rdir, "FRS funding model_functions.R")) # only creates function - no live code


# Get FRS model parameters, constants, raw initial data, and derived initial data --------------------------

print("sourcing FRS model parameters.R...")
if(FULL_RUN){
  modparm_data_env <- new.env()
  source(fs::path(rdir, "FRS_model_parameters.R"), local = modparm_data_env)
  save(modparm_data_env, file = fs::path(wddir, "modparm_data_env.RData"))
  ls(envir = modparm_data_env)
}
load(fs::path(wddir, "modparm_data_env.RData"))
list2env(as.list(modparm_data_env), envir = .GlobalEnv)
# rm(modparm_data_env)


print("sourcing FRS model input.R or equivalent...") # this gets init_funding_data
if(FULL_RUN){
  frs_data_env <- new.env()
  # 13 secs only reads data and sets variable values - no functions
  source(fs::path(rdir, "FRS_import_input_data_and_constants.R"), local = frs_data_env)
  save(frs_data_env, file = fs::path(wddir, "frs_data_env.RData"))
}
load(fs::path(wddir, "frs_data_env.RData"))
list2env(as.list(frs_data_env), envir = .GlobalEnv)
# rm(frs_data_env)


# create params environment -----------------------------------------------
source(fs::path(rdir, "FRS create_params_env.R")) 
params <- get_params(frs_data_env, modparm_data_env)
ns(params)


# create derived data -----------------------------------------------

# get initial data derived from raw model data - does NOT require modeling assumptions
print("sourcing FRS benefit model_actions.R...") 
if(BENEFIT_RUN){
  benefit_model_data_env <- new.env()
  source(fs::path(rdir, "FRS benefit model_actions.R"), local = benefit_model_data_env)
  save(benefit_model_data_env, file = fs::path(wddir, "benefit_model_data_env.RData"))
}
load(fs::path(wddir, "benefit_model_data_env.RData"))
list2env(as.list(benefit_model_data_env), envir = .GlobalEnv)
# rm(benefit_model_data_env)
# creates for each class: salary_headcount, entrant_profile, mort, retire_mort, drop entry, retire, early retire, sep rates


# Prepare data for modeling -----------------------------------------------
#Get workforce data (run this model only when workforce data is updated, otherwise use the rds files)
print("sourcing FRS workforce model_get_and_save_wfdata.R...")
system.time(source(fs::path(rdir, "FRS workforce model_get_and_save_wfdata.R"))) # 2 mins -- only saves objects - no functions
# depends on assumptions in the model: 

print("sourcing FRS workforce model_get_saved_data.R...")
system.time(source(fs::path(rdir, "FRS workforce model_get_saved_data.R"))) # < 1 sec -- only gets saved data - no functions
# simply loads wf data -- 4 table types per 7 classes

# prepare global lists to pass as needed - eventually replace with stacked data frames
underscored_class_names <- str_replace(params$class_names_no_drop_frs_, " ", "_")

# djb return here ----
# wf_data_list: each class has 4 tables: entrant_profile_table, salary_headcount_table, mort_table, sep_rate_table
wf_data_list <- mget(paste0(underscored_class_names, "_wf_data"), envir = .GlobalEnv) # does not waste memory because R is copy on modify
entrant_profile_table_list <- mget(paste0(underscored_class_names, "_entrant_profile_table"), envir = .GlobalEnv)
salary_headcount_table_list <- mget(paste0(underscored_class_names, "_salary_headcount_table"), envir = .GlobalEnv)
mort_table_list <- mget(paste0(underscored_class_names, "_mort_table"), envir = .GlobalEnv)
mort_retire_table_list <- mget(paste0(underscored_class_names, "_mort_retire_table"), envir = .GlobalEnv)
separation_rate_table_list <- mget(paste0(underscored_class_names, "_separation_rate_table"), envir = .GlobalEnv)

# ns(.GlobalEnv) |> str_subset("separation_rate_table")


# Get funding data
print("sourcing FRS funding model_actions.R...")
# gets current_amort_layers_table and funding_list with all classes
system.time(source(fs::path(rdir, "FRS funding model_actions.R"))) # < 1 sec only creates objects - no functions

print("Done building model...")

##############################################TESTING############################################

# baseline_funding <- get_funding_data(funding_list, current_amort_layers_table, params=params)

# baseline_funding <- get_funding_data()
# 
# funding_72return <- get_funding_data(return_scen = "model",
#                                      model_return = 0.072)
# 
# funding_62return <- get_funding_data(return_scen = "model",
#                                      model_return = 0.062)
# 
# 
# check_72 <- funding_72return$frs %>% 
#   select(year, total_ual_mva, total_ual_ava, fr_mva, fr_ava)
# 
# 
# check_62 <- funding_62return$frs %>% 
#   select(year, total_ual_mva, total_ual_ava, fr_mva, fr_ava)
# 
# 
# export(baseline_funding, "baseline_funding.xlsx") OLD LOCATION
# 
# 
# baseline_liability <- get_liability_data()
# 
# cola1_constant_liability <- get_liability_data(cola_tier_1_active_constant = "yes")

# baseline_liability <- get_liability_data()
# baseline <- get_funding_data()
# 
# lower_amo <- get_funding_data(funding_policy = "ADC",
#                               amo_period_current = 23,
#                               amo_period_new = 23)
# 
# 
# anthony_scen2 <- get_funding_data(cola_current_active = 0.0071,
#                                  cola_new_active = 0.0071,
#                                  cola_current_retire = 0.0071,
#                                  funding_policy = "ADC",
#                                  amo_period_current = 50,
#                                  amo_period_new = 15,
#                                  dr_new = 0.06)
# 
# 
# anthony_scen_liab2 <- get_liability_data(cola_current_active = 0.0071,
#                                         cola_new_active = 0.0071,
#                                         cola_current_retire = 0.0071,
#                                         dr_new = 0.06)
# 
# write.csv(lower_amo, "lower_amo.csv")
# test_nc <- get_benefit_data(retire_refund_ratio = 0.3)
# 
# test_nc$nc_agg$normal_cost_aggregate_DB
# 
# test_liability <- get_liability_data(retire_refund_ratio = 0.3,
#                                      cal_factor = 10.6/12.9)
# 
# baseline <- get_liability_data()
# lower_dr_new <- get_liability_data(dr_new = 0.06)
# lower_dr <- get_liability_data(dr_new = 0.06, dr_current = 0.06)
# cola_everyone <- get_liability_data(cola_current_active = 0.03,
#                                     cola_new_active = 0.03,
#                                     cola_current_retire = 0.03)
# 
# cola_one <- get_liability_data(one_time_cola = T, cola_current_retire_one = 0.03, cal_factor = 10.8 / 13)
