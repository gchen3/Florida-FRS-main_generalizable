
# Create benefit multiplier function --------------------------------------

get_ben_mult <- function(tier, class_name, dist_age, dist_year, yos) {
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


# # The first version of get_ben_mult was wrong because it didn't consider the difference in Admin class between tier 2 and tier 3 multipliers
#  ben_mult_lookup <- expand.grid(
#    tier_at_dist_age = c("tier_1_non_vested", "tier_1_vested", "tier_1_early", "tier_1_norm", 
#             "tier_2_non_vested", "tier_2_vested", "tier_2_early", "tier_2_norm", 
#             "tier_3_non_vested", "tier_3_vested", "tier_3_early", "tier_3_norm"),
#    class_name = frs_data_env$class_names_no_drop_frs_,
#    dist_age = frs_data_env$age_range_,
#    yos = frs_data_env$yos_range_,
#    dist_year = c(1973, 2022)) %>%
#    mutate(ben_mult = get_ben_mult(tier = tier_at_dist_age, class_name, dist_age, dist_year, yos),
#           ben_mult_2 = if_else(str_detect(tier_at_dist_age, "tier_1"),
#                              if_else(class_name == "regular", 
#                                      case_when(
#                                        (dist_age >= 65 & yos >= 6) | (yos >= 33) ~ 0.0168,
#                                        (dist_age >= 64 & yos >= 6) | (yos >= 32) ~ 0.0165,
#                                        (dist_age >= 63 & yos >= 6) | (yos >= 31) ~ 0.0163,
#                                        (dist_age >= 62 & yos >= 6) | (yos >= 30) ~ 0.0160,
#                                        str_detect(tier_at_dist_age, "early") ~ 0.0160
#                                      ),
#                                      if_else(class_name == "special", if_else(dist_year <= 1974, 0.02, 0.03),
#                                              if_else(class_name == "admin", 
#                                                      case_when(
#                                                        (dist_age >= 58 & yos >= 6) | (yos >= 28) ~ 0.0168,
#                                                        (dist_age >= 57 & yos >= 6) | (yos >= 27) ~ 0.0165,
#                                                        (dist_age >= 56 & yos >= 6) | (yos >= 26) ~ 0.0163,
#                                                        (dist_age >= 55 & yos >= 6) | (yos >= 25) ~ 0.0160,
#                                                        str_detect(tier_at_dist_age, "early") ~ 0.0160
#                                                      ),
#                                                      if_else(class_name %in% c("eco", "eso"), 0.03,
#                                                              if_else(class_name == "judges", 0.0333, if_else(class_name == "senior_management", 0.02, NA)
#                                                              )
#                                                      )
#                                              )
#                                      )
#                              ),
#                              if_else(str_detect(tier_at_dist_age, "tier_2"),
#                                      if_else(class_name == "regular",
#                                              case_when(
#                                                (dist_age >= 68 & yos >= 8) | (yos >= 36) ~ 0.0168,
#                                                (dist_age >= 67 & yos >= 8) | (yos >= 35) ~ 0.0165,
#                                                (dist_age >= 66 & yos >= 8) | (yos >= 34) ~ 0.0163,
#                                                (dist_age >= 65 & yos >= 8) | (yos >= 33) ~ 0.0160,
#                                                str_detect(tier_at_dist_age, "early") ~ 0.0160
#                                              ),
#                                              if_else(class_name == "special", if_else(dist_year <= 1974, 0.02, 0.03),
#                                                      if_else(class_name == "admin",
#                                                              case_when(
#                                                                (dist_age >= 63 & yos >= 8) | (yos >= 33) ~ 0.0168,
#                                                                (dist_age >= 62 & yos >= 8) | (yos >= 32) ~ 0.0165,
#                                                                (dist_age >= 61 & yos >= 8) | (yos >= 31) ~ 0.0163,
#                                                                (dist_age >= 60 & yos >= 8) | (yos >= 30) ~ 0.0160,
#                                                                str_detect(tier_at_dist_age, "early") ~ 0.0160
#                                                              ),
#                                                              if_else(class_name %in% c("eco", "eso"), 0.03,
#                                                                      if_else(class_name == "judges", 0.0333, if_else(class_name == "senior_management", 0.02, NA)
#                                                                      )
#                                                              )
#                                                      )
#                                              )
#                                      ),
#                                      if_else(str_detect(tier_at_dist_age, "tier_3"),
#                                              if_else(class_name == "regular",
#                                                      case_when(
#                                                        (dist_age >= 68 & yos >= 8) | (yos >= 36) ~ 0.0168,
#                                                        (dist_age >= 67 & yos >= 8) | (yos >= 35) ~ 0.0165,
#                                                        (dist_age >= 66 & yos >= 8) | (yos >= 34) ~ 0.0163,
#                                                        (dist_age >= 65 & yos >= 8) | (yos >= 33) ~ 0.0160,
#                                                        str_detect(tier_at_dist_age, "early") ~ 0.0160
#                                                      ),
#                                                      if_else(class_name == "special", if_else(dist_year <= 1974, 0.02, 0.03),
#                                                              if_else(class_name == "admin",
#                                                                      case_when(
#                                                                        dist_age >= 63 & yos >= 8 ~ 0.0168,
#                                                                        dist_age >= 62 & yos >= 8 ~ 0.0165,
#                                                                        dist_age >= 61 & yos >= 8 ~ 0.0163,
#                                                                        dist_age >= 60 & yos >= 8 ~ 0.0160,
#                                                                        str_detect(tier_at_dist_age, "early") ~ 0.0160
#                                                                      ),
#                                                                      if_else(class_name %in% c("eco", "eso"), 0.03,
#                                                                              if_else(class_name == "judges", 0.0333, if_else(class_name == "senior_management", 0.02, NA)
#                                                                              )
#                                                                      )
#                                                              )
#                                                      )
#                                              ), NA)
#                              )
#           ))
#  
#  
# 
#  compare_benefit_mult <-  ben_mult_lookup %>%
#    select(tier_at_dist_age, class_name, dist_year, ben_mult, ben_mult_2) %>%
#    mutate(
#      mismatch = (ben_mult != ben_mult_2) |
#        xor(is.na(ben_mult), is.na(ben_mult_2))
#    ) %>%
#    filter(mismatch == TRUE)

# Validate the calculation results:
# compare_benefit_mult <- benefit_table %>%
#   select(ben_mult, ben_mult_2) %>%
#   mutate(mismatch = ben_mult != ben_mult_2) %>%
#   filter(mismatch == TRUE)


# Benefit reduction factor function ---------------------------------------

get_reduce_factor <- function(tier, class_name, dist_age) {
  # Define tier groups
  tier_1 <- c("tier_1_non_vested", "tier_1_vested", "tier_1_early", "tier_1_norm")
  tier_2 <- c("tier_2_non_vested", "tier_2_vested", "tier_2_early", "tier_2_norm")
  tier_3 <- c("tier_3_non_vested", "tier_3_vested", "tier_3_early", "tier_3_norm")
  tier_early <- c("tier_1_early", "tier_2_early", "tier_3_early")
  tier_norm  <- c("tier_1_norm", "tier_2_norm", "tier_3_norm")
  
  case_when(
    tier %in% tier_norm ~ 1,
    
    tier %in% tier_early & class_name == "special" ~ case_when(
      tier %in% tier_1 ~ (1 - 0.05 * (55 - dist_age)),
      tier %in% c(tier_2, tier_3) ~ (1 - 0.05 * (60 - dist_age)),
      TRUE ~ NA
    ),
    
    tier %in% tier_early & class_name != "special" ~ case_when(
      tier %in% tier_1 ~ (1 - 0.05 * (62 - dist_age)),
      tier %in% tier_2 ~ (1 - 0.05 * (65 - dist_age)),
      tier %in% tier_3 ~ (1 - 0.05 * (65 - dist_age)),
      TRUE ~ NA
    ),
    
    TRUE ~ NA
  )
}

## Test
# reduce_factor_lookup <- expand.grid(
#   tier_at_dist_age = c("tier_1_non_vested", "tier_1_vested", "tier_1_early", "tier_1_norm",
#                        "tier_2_non_vested", "tier_2_vested", "tier_2_early", "tier_2_norm",
#                        "tier_3_non_vested", "tier_3_vested", "tier_3_early", "tier_3_norm"),
#   class_name = frs_data_env$class_names_no_drop_frs_,
#   dist_age = frs_data_env$age_range_,
#   yos = frs_data_env$yos_range_,
#   dist_year = c(1973, 2022)) %>%
#   mutate(reduce_factor = get_reduce_factor(tier = tier_at_dist_age,
#                                                         class_name = class_name,
#                                                         dist_age = dist_age),
#          reduce_factor_reason = if_else(str_detect(tier_at_dist_age, "norm"), 1,
#                                         if_else(str_detect(tier_at_dist_age, "early"),
#                                                 if_else(class_name == "special",
#                                                         case_when(
#                                                           str_detect(tier_at_dist_age, "tier_1") ~ (1 - 0.05*(55 - dist_age)),
#                                                           str_detect(tier_at_dist_age, "tier_2") ~ (1 - 0.05*(60 - dist_age)),
#                                                           str_detect(tier_at_dist_age, "tier_3") ~ (1 - 0.05*(60 - dist_age))
#                                                         ),
#                                                         case_when(
#                                                           str_detect(tier_at_dist_age, "tier_1") ~ (1 - 0.05*(62 - dist_age)),
#                                                           str_detect(tier_at_dist_age, "tier_2") ~ (1 - 0.05*(65 - dist_age)),
#                                                           str_detect(tier_at_dist_age, "tier_3") ~ (1 - 0.05*(65 - dist_age))
#                                                         )
#                                                 ), NA
#                                         )
#          ))
# 
# compare_reduce_factor <- reduce_factor_lookup %>%
#   mutate(
#     mismatch = (reduce_factor != reduce_factor_reason) |
#       xor(is.na(reduce_factor), is.na(reduce_factor_reason))
#   ) %>%
#   filter(mismatch == TRUE)


# Cost-of-living adjustment factor ----------------------------------------
get_cola <- function(tier, yos, entry_year, params) {
  yos_b4_2011 <- pmin(pmax(2011 - entry_year, 0), yos)
  
  out <- rep(0, length(tier))
  
  is_t1 <- str_detect(tier, "tier_1")
  if (!is.null(params$cola_tier_1_active_constant_) && params$cola_tier_1_active_constant_ == "no") {
    out[is_t1] <- ifelse(yos[is_t1] > 0, params$cola_tier_1_active_ * yos_b4_2011[is_t1] / yos[is_t1], 0)
  } else {
    out[is_t1] <- params$cola_tier_1_active_
  }
  
  out[str_detect(tier, "tier_2")] <- params$cola_tier_2_active_
  out[str_detect(tier, "tier_3")] <- params$cola_tier_3_active_
  
  return(out)
}




# Discount rate -----------------------------------------------------------

get_discount_rate <- function(tier, params) {
  if (str_detect(tier, "tier_3")) {
    return(params$dr_new_)
  } else {
    return(params$dr_current_)
  }
}


# final average salary period ---------------------------------------------
get_fas_period <- function(tier) {
  if (str_detect(tier, "tier_1")) return(5)
  return(8)
}
