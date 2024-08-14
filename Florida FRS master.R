rm(list = ls())

#Loading required libraries ------------------
library("readxl")
library(tidyverse)
library(zoo)
library(profvis)
library(data.table)
library(openxlsx)
library(janitor)
library(rio)
library(parallel)


# Functions ---------------------------------------------------------------

print("Start loading model data, parameters, and functions...")

#Get actuarial and financial functions
print("sourcing utility_functions.R...")
source("utility_functions.R") # only creates functions - no live code

#Get benefit model
# replace separate "Florida FRS benefit model.R" into functions and actions
# source("Florida FRS benefit model.R")
# source("Florida FRS benefit model_functions.R") # only creates functions - no live code
print("sourcing Florida FRS benefit model_helper_functions.R...")
source("Florida FRS benefit model_helper_functions.R") # only creates functions - no live code

print("sourcing Florida FRS benefit model_get_benefit_data_function.R...")
source("Florida FRS benefit model_get_benefit_data_function.R") # only creates functions - no live code

# Get workforce model
# source("Florida FRS workforce model.R") # only creates function - no live code
print("sourcing Florida FRS workforce model_functions....")
source("Florida FRS workforce model_functions.R") # only creates function - no live code

#Get liability model
print("sourcing Florida FRS liability model.R...")
source("Florida FRS liability model.R") # only creates function - no live code

#Get funding model
# source("Florida FRS funding model.R")
print("sourcing Florida FRS funding model_functions.R...")
source("Florida FRS funding model_functions.R") # only creates function - no live code


# Actions -----------------------------------------------------------------

#Get model inputs and assumptions
print("Start data construction based on system data and model parameters...")

#Get model inputs and assumptions
print("sourcing Florida FRS model input.R...")
system.time(source("Florida FRS model input.R")) # only reads data and sets variable values - no functions

print("sourcing Florida FRS benefit model_actions.R...")
system.time(source("Florida FRS benefit model_actions.R")) # only creates objects - no functions

#Get workforce data (run this model only when workforce data is updated, otherwise use the rds files)
print("sourcing Florida FRS workforce model_get_and_save_wfdata.R...")
system.time(source("Florida FRS workforce model_get_and_save_wfdata.R")) # only saves objects - no functions

print("sourcing Florida FRS workforce model_get_saved_data.R...")
system.time(source("Florida FRS workforce model_get_saved_data.R")) # only gets saved data - no functions

# Get funding data
print("sourcing Florida FRS funding model_actions.R...")
system.time(source("Florida FRS funding model_actions.R")) # only creates objects - no functions

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
