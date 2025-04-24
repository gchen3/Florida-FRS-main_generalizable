
# set up to examine alternative approaches to get_ben_mult function ----

# previously I saved 7 data files (1 per class) to the djb folder -- bentable_regular, bentable_special, ...
# these have the data immediately prior to calling get_ben_mult
# my goal is to develop a fast and easily modifiable way to do what get_ben_mult_GC does


# libraries ----------------------------------------------------------------

library(tidyverse)
library(fs)
library(btools)
library(readxl)

# Gang's get_ben_mult ----------------------------------------------------------------
# Gang's get_ben_mult from FRS_rules_functions.R (I added _GC suffix)
# note this is in frs_data_env ?? 
# tier_2_vested regular          50      2052    32


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


# get benefit table data ---------------------------------------------------------

classes <- c("admin", "eco", "eso", "judges", "regular", "special", "senior_management")
f <- function(class){
  fname <- paste0("bentable_", class, ".rds")
  print(fname)
  readRDS(here::here("djb", fname))
}

benstack <- purrr::map(classes, f) |> 
  list_rbind()
glimpse(benstack)
count(benstack, class_name)

tier_1 <- c("tier_1_non_vested", "tier_1_vested", "tier_1_norm") # I removed "tier_1_early"
tier_2 <- c("tier_2_non_vested", "tier_2_vested", "tier_2_norm") # I removed "tier_2_early", 
tier_3 <- c("tier_3_non_vested", "tier_3_vested", "tier_3_norm") # I removed tier_3_early", 
tier_early <- c("tier_1_early", "tier_2_early", "tier_3_early")

data <- benstack |> 
  select(tier = tier_at_dist_age,
         class_name, dist_age, dist_year, yos) |> 
  mutate(tier_group = case_when(
    tier %in% tier_1 ~ "tier_1",
    tier %in% tier_2 ~ "tier_2",
    tier %in% tier_3 ~ "tier_3",
    tier %in% tier_early ~ tier,
    .default = "ERROR"))

# get benmult_true ---------------------------------------------------------------

a <- proc.time()
res <- data |> 
  mutate(benmult_true = get_ben_mult_GC(tier, class_name, dist_age, dist_year, yos))
b <- proc.time()
b - a # ~4 secs on full file

glimpse(res)  
summary(res) # ~510k are NA; none of the function inputs are NA -- did Reason have this, too?


# get benmult using rules-tibble approach -----------------------------------

rules <- read_excel(here::here("djb", "benefit_rules.xlsx")) |> 
  filter(!is.na(system))
rules

a1 <- proc.time()
res2 <- res |> 
  left_join(rules |> select(-system),
            by = join_by(class_name, 
                         tier_group,
                         dist_age >= dist_age_min_ge,
                         dist_age < dist_age_max_lt,
                         yos >= yos_min_ge,
                         yos < yos_max_lt,
                         dist_year >= dist_year_min_ge,
                         dist_year < dist_year_max_lt)) |> 
  relocate(benmult_true, .before = benmult) |> 
  mutate(rownum = row_number())
b1 <- proc.time()
b1 - a1 # ~ 2 secs

# CAUTION: check nrow(check) == nrow(djb1) !!!!!
nrow(res2) == nrow(res)
summary(res2)

check <- res2 |> 
  filter(!is.na(benmult)) |> 
  filter(benmult != benmult_true)

check <- res2 |> 
  filter(is.na(benmult) | is.na(benmult_true))
summary(check)


rules2 <- rules |> 
  mutate(benmult = case_when(tier_group == "tier_1" ~ benmult + .01,
                             .default = benmult))


# turn the merge approach into a function ----
params2 <- list() # normally we'd use the params environment
params2$rules <- rules

get_ben_mult_merge <- function(tier, class_name, dist_age, dist_year, yos, params = params2){ # use rules from environment
  # take the inputs, turn them into a tibble, do the merge, return a vector
  df <- tibble(tier, class_name, dist_age, dist_year, yos) |> 
    mutate(tier_group = case_when(
      tier %in% tier_1 ~ "tier_1",
      tier %in% tier_2 ~ "tier_2",
      tier %in% tier_3 ~ "tier_3",
      tier %in% tier_early ~ tier,
      .default = "ERROR")) |> 
    left_join(params$rules |> select(-system),
              by = join_by(class_name, 
                           tier_group,
                           dist_age >= dist_age_min_ge,
                           dist_age < dist_age_max_lt,
                           yos >= yos_min_ge,
                           yos < yos_max_lt,
                           dist_year >= dist_year_min_ge,
                           dist_year < dist_year_max_lt))
    return(df$benmult)
}

a2 <- proc.time()
res3 <- benstack |> 
  mutate(benmult_merge = get_ben_mult_merge(tier_at_dist_age, class_name, dist_age, dist_year, yos))
b2 <- proc.time()
b2 - a2

summary(res)
summary(res3)
tibble(benmult_true = res$benmult_true, benmult_merge = res3$benmult_merge) |> 
  mutate(diff = benmult_merge - benmult_true) |> 
  arrange(desc(abs(diff)))
  



#**************************************************************************************************************----
#**************************************************************************************************************----

# APPENDIX: how many unique combinations do we have? ---------------------------------------------------------------

df <- benstack |> 
  select(tier = tier_at_dist_age,
         class_name, dist_age, dist_year, yos) # 1.4m regular records, 7.3m total

df2 <- df |> distinct() # 456k regular records, 2.7m total


# APPENDIX: why do we have benmult = na recs? ---------------------------------------

narecs <- res |> filter(is.na(benmult)) |> select(-benmult)
glimpse(narecs)
summary(narecs)
# max yos is 39
ht(narecs)

count(narecs, class_name) # only admin and regular classes; don't see other patterns yet
count(narecs, class_name, tier)
count(narecs, yos)
count(narecs, dist_age)
count(narecs, dist_year) |> tail()

regna <- narecs |> filter(class_name == "regular")

head(regna)

tmp <- regna |> filter(yos==max(yos)) |> arrange(dist_age) # yos 32
head(tmp) # why do these fall through the cracks? 
# because we must have yos >= 33 or else dist_age >= 65
# so why do we keep them in the data?
# (dist_age >= 65 & yos >= 8) | yos >= 33 ~ 0.0160,
# # A tibble: 6 × 5
# tier          class_name dist_age dist_year   yos
# <chr>         <chr>         <dbl>     <dbl> <dbl>
# 1 tier_2_vested regular          50      2052    32
# 2 tier_2_vested regular          50      2053    32
# 3 tier_2_vested regular          50      2054    32
# 4 tier_2_vested regular          50      2055    32
# 5 tier_3_vested regular          50      2056    32
# 6 tier_3_vested regular          50      2057    32


