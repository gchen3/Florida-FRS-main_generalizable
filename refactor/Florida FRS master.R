rm(list = ls())

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

library(purrr)

# load pension-specific libraries
# For first time installing package, or each time package is updated, install as follows
# devtools::install(pkg = here::here("pentools"))
library(pentools) # use this instead of sourcing utility_functions.R

# Functions ---------------------------------------------------------------

print("Loading model functions...")

#Get actuarial and financial functions
# print("NOT sourcing utility_functions.R but loading pentools...")
# source(here::here("refactor", "utility_functions.R")) # only creates functions - no live code

#Get benefit model
# replace separate "Florida FRS benefit model.R" into functions and actions
# source("Florida FRS benefit model.R")
# source("Florida FRS benefit model_functions.R") # only creates functions - no live code
print("sourcing Florida FRS benefit model_helper_functions.R...")
source(here::here("refactor", "Florida FRS benefit model_helper_functions.R")) # only creates functions - no live code

print("sourcing Florida FRS benefit model_get_benefit_data_function.R...")
source(here::here("refactor", "Florida FRS benefit model_get_benefit_data_function.R")) # only creates functions - no live code

# Get workforce model
# source("Florida FRS workforce model.R") # only creates function - no live code
print("sourcing Florida FRS workforce model_functions....")
source(here::here("refactor", "Florida FRS workforce model_functions.R")) # only creates function - no live code

#Get liability model
print("sourcing Florida FRS liability model.R...")
source(here::here("refactor", "Florida FRS liability model.R")) # only creates function - no live code

#Get funding model
# source("Florida FRS funding model.R")
print("sourcing Florida FRS funding model_functions.R...")
source(here::here("refactor", "Florida FRS funding model_functions.R")) # only creates function - no live code


# Load FRS data, constants, and model parameters --------------------------

print("sourcing Florida FRS model parameters.R...")
source(here::here("refactor", "FRS_model_parameters.R"))

print("sourcing Florida FRS model input.R or equivalent...") # this gets init_funding_data
# ONETIME: save the frs_data_env to a file
# frs_data_env <- new.env()
# source(here::here("refactor", "FRS_import_input_data_and_constants.R"), local = frs_data_env)
# save(frs_data_env, file = here::here("refactor", "working_data", "frs_data_env.RData"))

# system.time(source(here::here("refactor", "FRS_import_input_data_and_constants.R"))) # 13 secs only reads data and sets variable values - no functions
load(here::here("refactor", "working_data", "frs_data_env.RData"))
list2env(as.list(frs_data_env), envir = .GlobalEnv)
# rm(frs_data_env)

FIXED_CLASS_NAMES <- init_funding_data$class
FIXED_CLASS_NAMES_NO_DROP_FRS <- FIXED_CLASS_NAMES[!FIXED_CLASS_NAMES %in% c("drop", "frs")]
FIXED_CLASS_NAMES_NO_FRS <- FIXED_CLASS_NAMES[!FIXED_CLASS_NAMES %in% c("frs")]

# get_fixed_class_names <- function(init_funding_data){
#   class_names <- init_funding_data$class
#   class_names_no_drop_frs <- class_names[!class_names %in% c("drop", "frs")]
#   class_names_no_frs <- class_names[!class_names %in% c("frs")]
# }


# Actions -----------------------------------------------------------------

#Get model inputs and assumptions
print("Start data construction based on system data and model parameters...")

print("sourcing Florida FRS benefit model_actions.R...") 
# ONETIME: save the benefit_model_data_env to a file
# benefit_model_data_env <- new.env()
# source(here::here("refactor", "Florida FRS benefit model_actions.R"), local = benefit_model_data_env)
# save(benefit_model_data_env, file = here::here("refactor", "working_data", "benefit_model_data_env.RData"))

# system.time(source(here::here("refactor", "Florida FRS benefit model_actions.R"))) # 21 secs only creates objects - no functions
load(here::here("refactor", "working_data", "benefit_model_data_env.RData"))
list2env(as.list(benefit_model_data_env), envir = .GlobalEnv)
# rm(benefit_model_data_env)
# creates for each class: salary_headcount, entrant_profile, mort, retire_mort, drop entry, retire, early retire, sep rates

#Get workforce data (run this model only when workforce data is updated, otherwise use the rds files)
print("sourcing Florida FRS workforce model_get_and_save_wfdata.R...")
system.time(source(here::here("refactor", "Florida FRS workforce model_get_and_save_wfdata.R"))) # 2 mins -- only saves objects - no functions
# depends on assumptions in the model: 

print("sourcing Florida FRS workforce model_get_saved_data.R...")
system.time(source(here::here("refactor", "Florida FRS workforce model_get_saved_data.R"))) # < 1 sec -- only gets saved data - no functions
# simply loads wf data -- 4 table types per 7 classes

# Get funding data
print("sourcing Florida FRS funding model_actions.R...")
system.time(source(here::here("refactor", "Florida FRS funding model_actions.R"))) # < 1 sec only creates objects - no functions

print("Done building model...")

##############################################TESTING############################################

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
# export(baseline_funding, "baseline_funding.xlsx")
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
