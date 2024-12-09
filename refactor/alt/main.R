# rm(list = ls())

# Ctrl-Shift-F10 to restart R

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

# boyd additions
library(Rcpp)
library(RcppRoll)

# boyd additions to libraries
library(purrr)
library(btools) # ns, ht

# load pension-specific libraries
# For first time installing package, or each time package is updated, install as follows
# devtools::install_github("donboyd5/pentools")
# devtools::install_github("donboyd5/pendata")
library(pentools) # use this instead of sourcing "FRS_utility_functions.R"
library(pendata)

# define directories -------------------------------------------------------------

iddir <- here::here("refactor", "interim_data")
rdir <- here::here("refactor", "R")
sddir <- here::here("refactor", "source_data")
tooldir <- here::here("refactor", "tools")
wddir <- here::here("refactor", "working_data")
xidir <- here::here("refactor", "source_data", "Reports", "extracted inputs")

stackdir <- here::here("refactor", "stacked_data")

# load environments created by FRS_master.R ---------------------------------------

load(fs::path(wddir, "benefit_model_data_env.RData"))
load(fs::path(wddir, "bm_env.RData")) # functions only
load(fs::path(wddir, "fm_env.RData")) # funding model functions
load(fs::path(wddir, "frs_data_env.RData")) # this gets init_funding_data
load(fs::path(wddir, "modparm_data_env.RData"))
load(fs::path(wddir, "params.RData"))
load(fs::path(wddir, "wf_data_env.RData")) 

# load stacked data created by fm_env$get_funding_data() ---------------------------------------
funding_list_stacked <- readRDS(fs::path(stackdir, "funding_list_stacked.rds"))
liability_list_stacked <- readRDS(fs::path(stackdir, "liability_list_stacked.rds"))

# load stacked environments ----




