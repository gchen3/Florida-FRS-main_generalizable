
# set up to examine alternative approaches to get_ben_mult function ----




# Gang's call to get_ben_mult in ----

get_benefit_table



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
