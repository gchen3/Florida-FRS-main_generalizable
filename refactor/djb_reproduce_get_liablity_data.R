
# overview
# fm_env$get funding data(...)
#   lm_env$get_liability_data(...)
#     bm_env$get_benefit_data(...)
#        get_class_salary_growth_table(class_name, params$salary_growth_table_) [within bm_env]

# get_class_salary_growth_table <- function(class_name, salary_growth_table){
#   
#   class_salary_growth_table <- salary_growth_table %>% 
#     select(yos, contains(class_name)) %>% 
#     rename(cumprod_salary_increase = 2)
#   
#   return(class_salary_growth_table)
# }


# make liability model available
lm_env <- new.env()
source(fs::path(rdir, "FRS_liability_model_functions.R"), local = lm_env)

# load(fs::path(wddir, "wf_data_env.RData"))
# load wf data -- 4 table types per 7 classes

# key inputs to the model (stacked)
source(here::here("refactor", "functions_unpack_stack.R"))

# key inputs to the liability model ----
# funding_list_stacked <- bind_rows(funding_list, .id = "class")
funding_list_stacked <- readRDS(fs::path(wddir, "funding_list_stacked.rds"))

# class_name 
# wf_data 
# ben_payment_current 
# retiree_pop_current
# pvfb_term_current
# entrant_profile_table
# salary_headcount_table
# mort_table
# mort_retire_table
# separation_rate_table
# params


#.. workforce data, return list of 4 stacked data frames ----
# wf_active, wf_term, wf_refund, wf_retire
# each will have class, entry_age, age, year, and then vars such as  n_active

# this needs special handling because of the nested sets of 4 tibbles

# wf_data_list <- params$wf_data_list
# element_name <- paste0(class_name, "_wf_data")
# wf_data <- wf_data_list[[element_name]]

wf_data_stacked_list <- ups_wfdata(params$wf_data_list)
names(wf_data_stacked_list)
wf_data_stacked_list$wf_active_stacked

#.. unpack scalars into a tibble ----
#.... scalar groups are c("_ben_payment_current_", "_retiree_pop_current_", "_retiree_pop_current_") ----
scalar_groups <- c("_ben_payment_current_", "_retiree_pop_current_", "_retiree_pop_current_")
scalar_names <- outer(params$class_names_no_drop_frs_, scalar_groups, paste0) |> as.vector()
scalar_names

selected_elements <- mget(scalar_names, envir = params)

scalars_stacked <- enframe(selected_elements, name = "name", value = "value") |> 
  mutate(value=unlist(value)) |> 
  separate_class(col = "name", into = c("class", "variable")) |> 
  select(class, variable, value)

scalars_stacked

#.. entrant_profile_table ----
# ns(params$entrant_profile_table_list) |> str_subset("entrant_profile_table")
entrant_profile_table_stacked <- stack_list(params$entrant_profile_table_list, "_entrant_profile_table")

#.. salary_growth_table ----
salary_growth_table_stacked <- pendata::frs$salary_growth |> 
  select(class, yos, cumprod_salary_increase = cumprod_increase)

#.. salary_headcount_table ----
# ns(params$salary_headcount_table_list) |> str_subset("_salary_headcount_table")
salary_headcount_table_stacked <- stack_list(params$salary_headcount_table_list, "_salary_headcount_table")
count(salary_headcount_table_stacked, class)
params$salary_headcount_table_list$admin_salary_headcount_table # 26 admin rows

thelist <- params$salary_headcount_table_list

stack_list <- function(thelist, suffix) {
  tmp <- bind_rows(thelist, .id = "name") |> 
    mutate(class = str_remove(name, suffix)) |>
    select(-name) |> 
    select(class, everything())
}


#.. mort_table ----
# ns(params$mort_table_list) |> str_subset("_mort_table")
mort_table_stacked <- stack_list(params$mort_table_list, "_mort_table")

#.. mort_retire_table ----
# ns(params$mort_retire_table_list) |> str_subset("_mort_retire_table")
mort_retire_table_stacked <- stack_list(params$mort_retire_table_list, "_mort_retire_table")

#.. separation_rate_table ----
# ns(params$separation_rate_table_list) |> str_subset("_separation_rate_table")
separation_rate_table_stacked <- stack_list(params$separation_rate_table_list, "_separation_rate_table")

# create a stacked environment ----
stack_env <- new.env()
stack_env$scalars_stacked <- scalars_stacked
list2env(wf_data_stacked_list, envir = stack_env)
other_list <- named_list(
  entrant_profile_table_stacked,
  salary_growth_table_stacked,
  salary_headcount_table_stacked,
  mort_table_stacked,
  mort_retire_table_stacked,
  separation_rate_table_stacked)
list2env(other_list, envir = stack_env)
ns(stack_env)
system.time(save(stack_env, file = fs::path(wddir, "stack_env.RData")))


system.time(load(file = fs::path(wddir, "stack_env.RData")) )
ns(stack_env)
stack_env$scalars_stacked
stack_env$entrant_profile_table_stacked

# END stacked environment ----


#.. get_benefit_data ----

# overview
# fm_env$get funding data(...)
#   lm_env$get_liability_data(...)
#     bm_env$get_benefit_data(...)
#        get_class_salary_growth_table(class_name, params$salary_growth_table_) [within bm_env]
#        get_salary_benefit_table(class_name, entrant_profile_table, class_salary_growth_table, salary_headcount_table, params)


#.... salary benefit table ----


# stacked salary benefit table ----
# this is fast and, based on testing, equivalent to the Reason approach
library(RcppRoll)

stubs <- crossing(stack_env$entrant_profile_table_stacked |> 
                    select(class, entry_age),
                  entry_year = params$entry_year_range_,
                  yos = params$yos_range_) |> 
  mutate(term_age = entry_age + yos) |> 
  filter(term_age <= params$max_age_)
count(stubs, class)

# we need max_entry_year of JUST the records in the headcount table
max_entry_year <- stack_env$salary_headcount_table_stacked |> 
  summarise(max_entry_year = max(entry_year, na.rm = TRUE),
         .by=class)

salary_benefit_table_stacked <- stubs |> 
  left_join(stack_env$entrant_profile_table_stacked,
            by = join_by(class, entry_age)) |> 
  left_join(stack_env$salary_growth_table_stacked,
            by = join_by(class, yos)) |> 
  filter(term_age <= params$max_age_) |> 
  mutate(tier_at_term_age = bm_env$get_tier(class, entry_year, term_age, yos, params$new_year_)) |> 
  # Join salary_head_count_table_stacked by entry_year and entry_age only to get historical entry_salary
  left_join(stack_env$salary_headcount_table_stacked |> 
              select(class, entry_year, entry_age, entry_salary),
            by = join_by(class, entry_age, entry_year)) |>
  left_join(max_entry_year, by = join_by(class)) |> 
  # if_else respects grouping, ifelse does not
  # but we don't need grouping below because we merged in proper class max_entry_year above
  mutate(salary = if_else(entry_year <= max_entry_year, # max(entry_year), # max_entry_year, max(entry_year)
                          entry_salary * cumprod_salary_increase,
                          start_sal * cumprod_salary_increase * (1 + params$payroll_growth_)^(entry_year - max_entry_year)), # max(entry_year)
         fas_period = if_else(str_detect(tier_at_term_age, "tier_1"), 5, 8)) |> 
  mutate(
    # this rolling mean is faster than the Reason approach but equivalent
    fas = RcppRoll::roll_mean(c(NA, salary[-length(salary)]), # drop the current value
                               n = max(fas_period), align="right", fill = NA),
    db_ee_cont = params$db_ee_cont_rate_ * salary,
    db_ee_balance = pentools::get_cum_fv(params$db_ee_interest_rate_, db_ee_cont),
    .by=c(class, entry_year, entry_age)) |> 
  filter(!is.na(salary))


# explore
skim(salary_benefit_table_stacked)
count(salary_benefit_table_stacked, class)
count(salary_benefit_table_stacked, tier_at_term_age)
count(salary_benefit_table_stacked, tier_at_term_age, class) |> 
  pivot_wider(names_from = class, values_from = n) # ?? why are eco and judges the same??

# stacked ann_factor_table ----
stack_env$mort_table_stacked
salary_benefit_table_stacked

# semi_join() return all rows from x with a match in y.
# here all rows from mort_table_stacked (16m recs) with a match in salary_benefit_table (158k rows)

# create a tier lookup table
tier_lookup <- tibble(
  tier_at_dist_age = unique(mort_table_stacked$tier_at_dist_age)
) |> 
  mutate(dr = if_else(str_detect(tier_at_dist_age, "tier_3"), params$dr_new_, params$dr_current_))
tier_lookup

sbt2 <- salary_benefit_table_stacked |> 
  mutate(y2 = pmin(pmax(2011 - entry_year, 0), yos))
glimpse(sbt2)

sbt2 <- salary_benefit_table_stacked |>
  select(class, entry_year, entry_age) |> 
  distinct()

  

a <- proc.time()
ann_factor_table_stacked <- mort_table_stacked |> 
  # semi_join simply filters -- gets all rows from mort_table_stacked that
  # match salary_benefit_table_stacked on the join variables -- it does not
  # bring in other variables
  semi_join(salary_benefit_table_stacked,
            by = join_by(class, entry_year, entry_age)) |> 
  left_join(tier_lookup, by = join_by(tier_at_dist_age)) |> 
  mutate(
    # clamp yos_b4_2011 between 0 and yos
    yos_b4_2011 = pmin(pmax(2011 - entry_year, 0), yos),
    # djb come back here ----
    cola = case_when(
      #Tier 1 cola (current policy) = 3% * YOS before 2011 / Total YOS
      str_detect(tier_at_dist_age, "tier_1") & params$cola_tier_1_active_constant_ == "no" ~
        if_else(yos > 0, params$cola_tier_1_active_ * yos_b4_2011 / yos, 0),
      str_detect(tier_at_dist_age, "tier_1") & params$cola_tier_1_active_constant_ == "yes" ~
        params$cola_tier_1_active_,
      str_detect(tier_at_dist_age, "tier_2") ~
        params$cola_tier_2_active_,
      str_detect(tier_at_dist_age, "tier_3") ~
        params$cola_tier_3_active_
    )
  ) 
b <- proc.time()
b - a

ann_factor_table <- mort_table %>% 
  #Semi join the salary_benefit_able to reduce the size of the data that needs to be calculated
  semi_join(salary_benefit_table, by = c("entry_year", "entry_age")) %>%
  mutate(
    dr = if_else(str_detect(tier_at_dist_age, "tier_3"), params$dr_new_, params$dr_current_),
    yos_b4_2011 = pmin(pmax(2011 - entry_year, 0), yos),
    cola = case_when(
      #Tier 1 cola (current policy) = 3% * YOS before 2011 / Total YOS
      str_detect(tier_at_dist_age, "tier_1") & params$cola_tier_1_active_constant_ == "no" ~ 
        if_else(yos > 0, params$cola_tier_1_active_ * yos_b4_2011 / yos, 0),
      str_detect(tier_at_dist_age, "tier_1") & params$cola_tier_1_active_constant_ == "yes" ~ 
        params$cola_tier_1_active_,
      str_detect(tier_at_dist_age, "tier_2") ~ 
        params$cola_tier_2_active_,
      str_detect(tier_at_dist_age, "tier_3") ~ 
        params$cola_tier_3_active_
    )
  ) %>% 
  group_by(entry_year, entry_age, yos) %>% 
  mutate(
    cum_dr = cumprod(1 + lag(dr, default = 0)),
    cum_mort = cumprod(1 - lag(mort_final, default = 0)),
    cum_cola = cumprod(1 + lag(cola, default = 0)),
    cum_mort_dr = cum_mort / cum_dr,
    cum_mort_dr_cola = cum_mort_dr * cum_cola,
    # ann_factor below is the annuity factor at distribution (retirement) age
    ann_factor = rev(cumsum(rev(cum_mort_dr_cola))) / cum_mort_dr_cola
  ) %>% 
  ungroup()

#.. END get_benefit_data ----



names(params$wf_data_list)




# outputs from the model (stacked)
liability_list_stacked <- readRDS(fs::path(wddir, "liability_list_stacked.rds"))



# current call in FRS_funding_model_functions.R ----
# Use mclapply to run the liability model in parallel. May not work properly
# with Windows OS or API. Switch back to lapply if needed. When working,
# mclapply will be about twice as fast as lapply.
a <- proc.time()


# get values of arguments to get_liability_data for this class and then call it
call_get_liability_data <- function(class_name) {
  # create lists of data frames so that get_liablity_data does not have to (dangerously) pull data from the global environment with assign
  
  element_name <- paste0(class_name, "_wf_data")
  wf_data <- wf_data_list[[element_name]]
  
  ben_payment_current <- params[[paste0(class_name, "_ben_payment_current_")]]
  retiree_pop_current <- params[[paste0(class_name, "_retiree_pop_current_")]]
  pvfb_term_current <- params[[paste0(class_name, "_pvfb_term_current_")]]
  
  element_name <- paste0(class_name, "_entrant_profile_table")
  entrant_profile_table <- entrant_profile_table_list[[element_name]]
  
  element_name <- paste0(class_name, "_salary_headcount_table")
  salary_headcount_table <- salary_headcount_table_list[[element_name]]    
  
  element_name <- paste0(class_name, "_mort_table")
  mort_table <- mort_table_list[[element_name]]     
  
  element_name <- paste0(class_name, "_separation_rate_table")
  separation_rate_table <- separation_rate_table_list[[element_name]]         
  
  element_name <- paste0(class_name, "_mort_retire_table")
  mort_retire_table <- mort_retire_table_list[[element_name]]         
  
  lm_env$get_liability_data(class_name, 
                            wf_data, 
                            ben_payment_current, 
                            retiree_pop_current,
                            pvfb_term_current,
                            entrant_profile_table,
                            salary_headcount_table,
                            mort_table,
                            mort_retire_table,
                            separation_rate_table,
                            params)
}

liability_list <- mclapply(
  X = params$class_names_no_drop_frs_, 
  FUN = call_get_liability_data,
  # Set mc.cores to 1 for compatibility with Windows
  mc.cores = 1
)
names(liability_list) <- params$class_names_no_drop_frs_
b <- proc.time()


# checks ----

#.. salary_growth_table_checks ----
class_name <- "regular"
sgt1 <- params$salary_growth_table_ |> 
  select(yos, contains(class_name)) |> 
  rename(cumprod_salary_increase = 2)

sgt2 <- pendata::frs$salary_growth |> 
  rename(cumprod_salary_increase = cumprod_increase)

check <- bind_rows(sgt1 |> mutate(src="reason"),
                   sgt2 |> 
                     filter(class=="regular") |> 
                     select(yos, cumprod_salary_increase) |>
                     mutate(src="pendata"))
check |> 
  pivot_wider(names_from = src, values_from = cumprod_salary_increase) |> 
  ht()

