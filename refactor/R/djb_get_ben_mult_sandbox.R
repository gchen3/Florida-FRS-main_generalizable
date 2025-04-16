
# set up to examine alternative approaches to get_ben_mult function ----

# previously I saved 7 data files (1 per class) to the djb folder -- bentable_regular, bentable_special, ...
# these have the data immediately prior to calling get_ben_mult
# my goal is to develop a fast and easily modifiable way to do what get_ben_mult_GC does


# analysis ----------------------------------------------------------------

library(tidyverse)
library(fs)
library(btools)


# Gang's get_ben_mult from FRS_rules_functions.R (I added _GC suffix) ----
# note this is in frs_data_env ?? 
get_ben_mult_GC <- function(tier, class_name, dist_age, dist_year, yos) {
  tier_1 <- c("tier_1_non_vested", "tier_1_vested", "tier_1_early", "tier_1_norm")
  tier_2 <- c("tier_2_non_vested", "tier_2_vested", "tier_2_early", "tier_2_norm")
  tier_3 <- c("tier_3_non_vested", "tier_3_vested", "tier_3_early", "tier_3_norm")
  tier_early <- c("tier_1_early", "tier_2_early", "tier_3_early")
  
  result <- case_when(
    # Tier 1: Regular
    tier %in% tier_1 & class_name == "regular" ~ case_when(
      (dist_age >= 65 & yos >= 6) | yos >= 33 ~ 0.0168,
      (dist_age >= 64 & yos >= 6) | yos >= 32 ~ 0.0165,
      (dist_age >= 63 & yos >= 6) | yos >= 31 ~ 0.0163,
      (dist_age >= 62 & yos >= 6) | yos >= 30 ~ 0.0160,
      tier %in% tier_early ~ 0.0160,
      TRUE ~ NA
    ),
    
    # Tier 1, 2 and 3: Special
    tier %in% c(tier_1, tier_2, tier_3) & class_name == "special" ~ if_else(dist_year <= 1974, 0.02, 0.03),
    
    # Tier 1: Admin
    tier %in% tier_1 & class_name == "admin" ~ case_when(
      (dist_age >= 58 & yos >= 6) | yos >= 28 ~ 0.0168,
      (dist_age >= 57 & yos >= 6) | yos >= 27 ~ 0.0165,
      (dist_age >= 56 & yos >= 6) | yos >= 26 ~ 0.0163,
      (dist_age >= 55 & yos >= 6) | yos >= 25 ~ 0.0160,
      tier %in% tier_early ~ 0.0160,
      TRUE ~ NA
    ),
    
    # Tier 2 & 3: Regular
    tier %in% c(tier_2, tier_3) & class_name == "regular" ~ case_when(
      (dist_age >= 68 & yos >= 8) | yos >= 36 ~ 0.0168,
      (dist_age >= 67 & yos >= 8) | yos >= 35 ~ 0.0165,
      (dist_age >= 66 & yos >= 8) | yos >= 34 ~ 0.0163,
      (dist_age >= 65 & yos >= 8) | yos >= 33 ~ 0.0160,
      tier %in% tier_early ~ 0.0160,
      TRUE ~ NA
    ),
    
    # Tier 2: Admin
    tier %in% tier_2 & class_name == "admin" ~ case_when(
      (dist_age >= 63 & yos >= 8) | yos >= 33 ~ 0.0168,
      (dist_age >= 62 & yos >= 8) | yos >= 32 ~ 0.0165,
      (dist_age >= 61 & yos >= 8) | yos >= 31 ~ 0.0163,
      (dist_age >= 60 & yos >= 8) | yos >= 30 ~ 0.0160,
      tier %in% tier_early ~ 0.0160,
      TRUE ~ NA
    ),
    
    # Tier 3: Admin
    tier %in% tier_3 & class_name == "admin" ~ case_when(
      dist_age >= 63 & yos >= 8  ~ 0.0168,
      dist_age >= 62 & yos >= 8  ~ 0.0165,
      dist_age >= 61 & yos >= 8  ~ 0.0163,
      dist_age >= 60 & yos >= 8 ~ 0.0160,
      tier %in% tier_early ~ 0.0160,
      TRUE ~ NA
    ),
    
    # Flat values
    class_name %in% c("eco", "eso") ~ 0.03,
    class_name == "judges" ~ 0.0333,
    class_name == "senior_management" ~ 0.02,
    
    # Final fallback
    TRUE ~ NA
  )
  
  return(result)
}


# get a data file ---------------------------------------------------------

bentable <- readRDS(here::here("djb", "bentable_regular.rds"))
glimpse(bentable)

df <- bentable |> 
  select(tier = tier_at_dist_age,
         class_name, dist_age, dist_year, yos) # 1.4m regular records

df2 <- df |> distinct() # 456k records

res <- df2 |> 
  mutate(benmult = get_ben_mult_GC(tier, class_name, dist_age, dist_year, yos))
glimpse(res)  
summary(res) # 115.8k are NA

res2 <- res |> 
  na.omit() # 340k recs
count(res2, benmult) # 4 unique values

res2

tier_1 <- c("tier_1_non_vested", "tier_1_vested", "tier_1_norm") # I removed "tier_1_early"
tier_2 <- c("tier_2_non_vested", "tier_2_vested", "tier_2_norm") # I removed "tier_2_early", 
tier_3 <- c("tier_3_non_vested", "tier_3_vested", "tier_3_norm") # I removed tier_3_early", 
tier_early <- c("tier_1_early", "tier_2_early", "tier_3_early")

djb1 <- res2 |> 
  mutate(tier_group = case_when(
    tier %in% tier_1 ~ "tier_1",
    tier %in% tier_2 ~ "tier_2",
    tier %in% tier_3 ~ "tier_3",
    tier %in% tier_early ~ tier,
    .default = "ERROR"))
count(djb1, tier_group, tier)
glimpse(djb1)

# regt1 <- res2 |> 
#   filter()

# regular tier_group 1 or tier_early
# (dist_age >= 65 & yos >= 6) | yos >= 33 ~ 0.0168,
# (dist_age >= 64 & yos >= 6) | yos >= 32 ~ 0.0165,
# (dist_age >= 63 & yos >= 6) | yos >= 31 ~ 0.0163,
# (dist_age >= 62 & yos >= 6) | yos >= 30 ~ 0.0160,
# tier %in% tier_early ~ 0.0160, # note that this is the same for all

# regular tier_group 2 or 3, or tier_early
# (dist_age >= 68 & yos >= 8) | yos >= 36 ~ 0.0168,
# (dist_age >= 67 & yos >= 8) | yos >= 35 ~ 0.0165,
# (dist_age >= 66 & yos >= 8) | yos >= 34 ~ 0.0163,
# (dist_age >= 65 & yos >= 8) | yos >= 33 ~ 0.0160,
# tier %in% tier_early ~ 0.0160, 

# regular rules - note tier_early rule is same for all
rules <- read_csv(
"class_name, tier_group, dist_age_low, dist_age_high, dist_year_low, dist_year_high, yos_low, yos_high, benmult

regular, tier_1, 0, Inf, 0, Inf, 33, Inf, .0168
regular, tier_1, 65, Inf, 0, Inf, 6, Inf, .0168

regular, tier_1, 0, 64, 0, Inf, 32, 32, .0165
regular, tier_1, 64, 64, 0, Inf, 6, 32, .0165

regular, tier_1, 0, 63, 0, Inf, 31, 31, .0163
regular, tier_1, 63, 63, 0, Inf, 6, 31, .0163

regular, tier_1, 0, 62, 0, Inf, 30, 30, .0160
regular, tier_1, 62, 62, 0, Inf, 6, 30, .0160

regular, tier_1_early, 0, 61, 0, Inf, 0, 29, .0160



regular, tier_2, 0, Inf, 0, Inf, 36, Inf, .0168
regular, tier_2, 68, Inf, 0, Inf, 8, Inf, .0168

regular, tier_2, 0, 67, 0, Inf, 35, 35, .0165
regular, tier_2, 67, 67, 0, Inf, 8, 35, .0165

regular, tier_2, 0, 66, 0, Inf, 34, 34, .0163
regular, tier_2, 66, 66, 0, Inf, 8, 34, .0163

regular, tier_2, 0, 65, 0, Inf, 33, 33, .0160
regular, tier_2, 65, 65, 0, Inf, 8, 33, .0160

regular, tier_2_early, 0, 64, 0, Inf, 0, 32, .0160



regular, tier_3, 0, Inf, 0, Inf, 36, Inf, .0168
regular, tier_3, 68, Inf, 0, Inf, 8, Inf, .0168

regular, tier_3, 0, 67, 0, Inf, 35, 35, .0165
regular, tier_3, 67, 67, 0, Inf, 8, 35, .0165

regular, tier_3, 0, 66, 0, Inf, 34, 34, .0163
regular, tier_3, 66, 66, 0, Inf, 8, 34, .0163

regular, tier_3, 0, 65, 0, Inf, 33, 33, .0160
regular, tier_3, 65, 65, 0, Inf, 8, 33, .0160

regular, tier_3_early, 0, 64, 0, Inf, 0, 32, .0160

")

rules <- read_csv(
  "class_name, tier_group, dist_age_low, dist_age_high, dist_year_low, dist_year_high, yos_low, yos_high, benmult

regular, tier_1, 0, Inf, 0, Inf, 33, Inf, .0168
regular, tier_1, 65, Inf, 0, Inf, 6, 32, .0168

")

rules

djb2 <- djb1 |> 
  rename(benmult_true = benmult) |> 
  left_join(rules,
            by = join_by(class_name, tier_group, # equality
                         
                         # inequalities
                         dist_age >= dist_age_low,
                         dist_age <= dist_age_high,
                         
                         dist_year >= dist_year_low,
                         dist_year <= dist_year_high,                         
                         
                         yos >= yos_low,
                         yos <= yos_high))

check <- djb2 |> 
  filter(!is.na(benmult))

check |> 
  filter(benmult != benmult_true)

dups1 <- djb1 |> 
  mutate(n = n(), 
         .by=c(class_name, tier_group, tier, dist_age, dist_year, yos))
count(dups1, n)  


dups2 <- djb2 |> 
  mutate(n = n(), 
         .by=c(class_name, tier_group, tier, dist_age, dist_year, yos))
count(dups2, n)

dups2a <- dups2 |> 
  filter(n==2) |> 
  select(class_name, tier_group, tier, 
         starts_with("dist_age"),
         starts_with("dist_year"),
         starts_with("yos"),
         starts_with("benmult")) |> 
  arrange(class_name, tier_group, tier, dist_age, dist_year, yos)



result <- case_when(
  # Tier 1: Regular
  tier %in% tier_1 & class_name == "regular" ~ case_when(
    (dist_age >= 65 & yos >= 6) | yos >= 33 ~ 0.0168,
    (dist_age >= 64 & yos >= 6) | yos >= 32 ~ 0.0165,
    (dist_age >= 63 & yos >= 6) | yos >= 31 ~ 0.0163,
    (dist_age >= 62 & yos >= 6) | yos >= 30 ~ 0.0160,
    tier %in% tier_early ~ 0.0160,
    TRUE ~ NA
  ),

  tier %in% c(tier_2, tier_3) & class_name == "regular" ~ case_when(
    (dist_age >= 68 & yos >= 8) | yos >= 36 ~ 0.0168,
    (dist_age >= 67 & yos >= 8) | yos >= 35 ~ 0.0165,
    (dist_age >= 66 & yos >= 8) | yos >= 34 ~ 0.0163,
    (dist_age >= 65 & yos >= 8) | yos >= 33 ~ 0.0160,
    tier %in% tier_early ~ 0.0160,
    TRUE ~ NA
  ),


# Gang's call to get_ben_mult in ----

# get_funding_data()
  # call_get_liability_data()
    # get_liability_data()
      # bm_env$get_benefit_data() in FRS_liability_model_functions.R
        # get_benefit_table()
          #  get_ben_mult()

# benefit_table <- get_benefit_table(
#   class_name,
#   ann_factor_table,
#   salary_benefit_table,
#   params)

#   frs_data_env$get_ben_mult() line 116 of FRS_benefit_model_get_benefit_data_function_GC.R
#     in get_benefit_table <- function(class_name, ann_factor_table, salary_benefit_table, params)

# ben_mult = frs_data_env$get_ben_mult(
#   tier = tier_at_dist_age,
#   class_name = class_name,
#   dist_age = dist_age,
#   dist_year = dist_year,
#   yos = yos),

