# tier %in% tier_1 & class_name == "regular" ~ case_when(
#   (dist_age >= 65 & yos >= 6) | yos >= 33 ~ 0.0168,
#   (dist_age >= 64 & yos >= 6) | yos >= 32 ~ 0.0165,
#   (dist_age >= 63 & yos >= 6) | yos >= 31 ~ 0.0163,
#   (dist_age >= 62 & yos >= 6) | yos >= 30 ~ 0.0160,
#   tier %in% tier_early ~ 0.0160,
#   TRUE ~ NA
# ),
# 
# # Tier 1, 2 and 3: Special
# tier %in% c(tier_1, tier_2, tier_3) & class_name == "special" ~ if_else(dist_year <= 1974, 0.02, 0.03),

# Tier 1: Admin
# tier %in% tier_1 & class_name == "admin" ~ case_when(
#   (dist_age >= 58 & yos >= 6) | yos >= 28 ~ 0.0168,
#   (dist_age >= 57 & yos >= 6) | yos >= 27 ~ 0.0165,
#   (dist_age >= 56 & yos >= 6) | yos >= 26 ~ 0.0163,
#   (dist_age >= 55 & yos >= 6) | yos >= 25 ~ 0.0160,
#   tier %in% tier_early ~ 0.0160,
#   TRUE ~ NA
# ),
# 
# # Tier 2 & 3: Regular
# tier %in% c(tier_2, tier_3) & class_name == "regular" ~ case_when(
#   (dist_age >= 68 & yos >= 8) | yos >= 36 ~ 0.0168,
#   (dist_age >= 67 & yos >= 8) | yos >= 35 ~ 0.0165,
#   (dist_age >= 66 & yos >= 8) | yos >= 34 ~ 0.0163,
#   (dist_age >= 65 & yos >= 8) | yos >= 33 ~ 0.0160,
#   tier %in% tier_early ~ 0.0160,
#   TRUE ~ NA
# ),
# 
# # Tier 2: Admin
# tier %in% tier_2 & class_name == "admin" ~ case_when(
#   (dist_age >= 63 & yos >= 8) | yos >= 33 ~ 0.0168,
#   (dist_age >= 62 & yos >= 8) | yos >= 32 ~ 0.0165,
#   (dist_age >= 61 & yos >= 8) | yos >= 31 ~ 0.0163,
#   (dist_age >= 60 & yos >= 8) | yos >= 30 ~ 0.0160,
#   tier %in% tier_early ~ 0.0160,
#   TRUE ~ NA
# ),
# 
# # Tier 3: Admin
# tier %in% tier_3 & class_name == "admin" ~ case_when(
#   dist_age >= 63 & yos >= 8  ~ 0.0168,
#   dist_age >= 62 & yos >= 8  ~ 0.0165,
#   dist_age >= 61 & yos >= 8  ~ 0.0163,
#   dist_age >= 60 & yos >= 8 ~ 0.0160,
#   tier %in% tier_early ~ 0.0160,
#   TRUE ~ NA
# ),
# 
# # Flat values
# class_name %in% c("eco", "eso") ~ 0.03,
# class_name == "judges" ~ 0.0333,
# class_name == "senior_management" ~ 0.02,


# regular -----------------------------------------------------------------
#   (dist_age >= 65 & yos >= 6) | yos >= 33 ~ 0.0168,
#   (dist_age >= 64 & yos >= 6) | yos >= 32 ~ 0.0165,
#   (dist_age >= 63 & yos >= 6) | yos >= 31 ~ 0.0163,
#   (dist_age >= 62 & yos >= 6) | yos >= 30 ~ 0.0160,



regular_tier1 <- tribble(
  ~tier_group, ~dist_age_low, ~dist_age_high, ~yos_low, ~yos_high, ~benmult,
  "tier_1", 65, Inf, 6, Inf, .0168,
  "tier_1", 0, 65, 33, Inf, .0168,
  
  "tier_1", 64, 65, 6, Inf, .0165,
  "tier_1", 0, 64, 32, 33, .0165,
  
  "tier_1", 0, 63, 31, 32, .0163,
  "tier_1", 63, 64, 6, 31, .0163,
  
  "tier_1", 0, 62, 30, 31, .0160,
  "tier_1", 62, 63, 6, 30, .0160,
  
  "tier_1_early", 0, 62, 0, Inf, .0160
) |> 
  mutate(class_name = "regular") |> 
  select(class_name, tier_group, benmult, everything())
# regular_tier1

regular_tier2 <- tribble(
  ~tier_group, ~dist_age_low, ~dist_age_high, ~yos_low, ~yos_high, ~benmult,
  "tier_2", 0, Inf, 36, Inf, .0168,
  "tier_2", 68, Inf, 8, 35, .0168,
  
  "tier_2", 0, 67, 35, 35, .0165,
  "tier_2", 67, 67, 8, 34, .0165,
  
  "tier_2", 0, 66, 34, 34, .0163,
  "tier_2", 66, 66, 8, 33, .0163,
  
  "tier_2", 0, 65, 33, 33, .0160,
  "tier_2", 65, 65, 8, 32, .0160,
  
  "tier_2_early", 0, 64, 0, Inf, .0160
) |> 
  mutate(class_name = "regular") |> 
  select(class_name, tier_group, benmult, everything())
# regular_tier2

regular_tier3 <- regular_tier2 |> 
  mutate(tier_group = str_replace(tier_group, "tier_2", "tier_3"))
# regular_tier3


# special -----------------------------------------------------------------
#     tier %in% c(tier_1, tier_2, tier_3) & class_name == "special" ~ if_else(dist_year <= 1974, 0.02, 0.03),

# special <-  tribble(
#   ~tier_group, ~dist_age_low, ~dist_age_high, ~yos_low, ~yos_high, ~dist_year_low, dist_year_high, ~benmult,
#   "tier_1", 0, Inf, 0, Inf, 0, 1974, .02,
#   "tier_1", 0, Inf, 0, Inf, 0, 1974, .03,


# eco eso judges senior management ----------------------------------------

eco_eso_judges_srmgt <- crossing(
  class_name = c("eco", "eso", "judges", "senior_management"),
  tier_group = c("tier_1", "tier_2", "tier_3",
                 "tier_1_early", "tier_2_early", "tier_3_early")) |> 
  mutate(benmult = case_when(
    class_name %in% c("eco", "eso") ~ 0.03,
    class_name == "judges" ~ 0.0333,
    class_name == "senior_management" ~ 0.02,
    .default = -Inf)) |> 
  mutate(dist_age_low = 0, dist_age_high = Inf,
         yos_low = 0, yos_high = Inf)
# count(eco_eso_judges_srmgt, class_name, benmult)



# combine rules -----------------------------------------------------------

rules <- bind_rows(
  regular_tier1,
  regular_tier2,
  regular_tier3,
  eco_eso_judges_srmgt,
  tibble(dist_year_low = NA_real_, 
         dist_year_high = NA_real_)) |> 
  mutate(dist_year_low = ifelse(is.na(dist_year_low), 0, dist_year_low),
         dist_year_high = ifelse(is.na(dist_year_high), Inf, dist_year_high)) |> 
  filter(!is.na(class_name))


# old stuff --------------------------------------------------------------

# rules <- read_csv(
#   "class_name, tier_group, dist_age_low, dist_age_high, dist_year_low, dist_year_high, yos_low, yos_high, benmult
# 
# regular, tier_1, 0, Inf, 0, Inf, 33, Inf, .0168
# regular, tier_1, 65, Inf, 0, Inf, 6, 32, .0168
# 
# regular, tier_1, 0, 64, 0, Inf, 32, 32, .0165
# regular, tier_1, 64, 64, 0, Inf, 6, 31, .0165
# 
# regular, tier_1, 0, 63, 0, Inf, 31, 31, .0163
# regular, tier_1, 63, 63, 0, Inf, 6, 30, .0163
# 
# regular, tier_1, 0, 62, 0, Inf, 30, 30, .0160
# regular, tier_1, 62, 62, 0, Inf, 6, 29, .0160
# 
# regular, tier_1_early, 0, 61, 0, Inf, 0, Inf, .0160
# 
# 
# 
# regular, tier_2, 0, Inf, 0, Inf, 36, Inf, .0168
# regular, tier_2, 68, Inf, 0, Inf, 8, 35, .0168
# 
# regular, tier_2, 0, 67, 0, Inf, 35, 35, .0165
# regular, tier_2, 67, 67, 0, Inf, 8, 34, .0165
# 
# regular, tier_2, 0, 66, 0, Inf, 34, 34, .0163
# regular, tier_2, 66, 66, 0, Inf, 8, 33, .0163
# 
# regular, tier_2, 0, 65, 0, Inf, 33, 33, .0160
# regular, tier_2, 65, 65, 0, Inf, 8, 32, .0160
# 
# regular, tier_2_early, 0, 64, 0, Inf, 0, Inf, .0160
# 
# 
# 
# regular, tier_3, 0, Inf, 0, Inf, 36, Inf, .0168
# regular, tier_3, 68, Inf, 0, Inf, 8, 35, .0168
# 
# regular, tier_3, 0, 67, 0, Inf, 35, 35, .0165
# regular, tier_3, 67, 67, 0, Inf, 8, 34, .0165
# 
# regular, tier_3, 0, 66, 0, Inf, 34, 34, .0163
# regular, tier_3, 66, 66, 0, Inf, 8, 33, .0163
# 
# regular, tier_3, 0, 65, 0, Inf, 33, 33, .0160
# regular, tier_3, 65, 65, 0, Inf, 8, 32, .0160
# 
# regular, tier_3_early, 0, 64, 0, Inf, 0, Inf, .0160
# 
# 
# ")

data <- djb1 |> 
  filter(class_name == "regular", tier_group == "tier_1") |> 
  rename(benmult_true = benmult)

rules <- read_csv("
class_name, tier_group, rule
regular, tier_1, yos >= 6 & dist_age >= 62 | yos >= 33
")

check <- data |> 
  left_join(rules,
            by = join_by(class_name, tier_group)) |> 
  # mutate(benmult = eval(rule)) |> 
  mutate(benmult = eval(parse(text = rule)))

# (dist_age >= 65 & yos >= 6) | yos >= 33 ~ 0.0168,
# rules <- tribble(
#   ~class_name, ~tier_group, ~dist_age_min, ~dist_age_max, ~yos_min, ~yos_max, ~benmult,
#   "regular", "tier_1", 65, Inf, 6, Inf, .0168,
#   "regular", "tier_1", -Inf, 65, 33, Inf, .0168)
# rules

rules <- read_excel(here::here("djb", "benefit_rules.xlsx")) |> 
  filter(!is.na(system))
rules

check <- data |> 
  left_join(rules |> select(-system),
            by = join_by(class_name, tier_group,
                         dist_age >= dist_age_min_ge,
                         dist_age < dist_age_max_lt,
                         yos >= yos_min_ge,
                         yos < yos_max_lt,
                         dist_year >= dist_year_min_ge,
                         dist_year < dist_year_max_lt)) |> 
  relocate(benmult_true, .before = benmult) |> 
  mutate(rownum = row_number())
# CAUTION: check nrow(check) == nrow(data) !!!!!
nrow(check) == nrow(data)

dups <- check |> 
  mutate(n = n(),
         .by = c(class_name, tier_group, dist_age, yos, dist_year)) |> 
  filter(n > 1)

# 63 2018 30
# 64 2019 30
tmp <- check |> 
  filter(dist_age==64, dist_year==2019, yos==30, class_name=="regular", tier_group == "tier_1")

check2 <- check |> 
  filter(!is.na(benmult)) |> 
  filter(benmult != benmult_true)


