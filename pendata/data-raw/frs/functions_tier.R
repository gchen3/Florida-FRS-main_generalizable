

# functions to get the tier and type, given:
#   class, entry_year, age, yos, new_year

tier1_type <- function(class, age, yos){

  norm_specad <- expression(class %in% c("special", "admin") &
                              (yos >= 25 |
                                 (age >= 55 & yos >= 6) |
                                 (age >= 52 & yos >= 25)))

  norm_other <- expression(!class %in% c("special", "admin") &
                             (yos >= 30 |
                                (age >= 62 & yos >= 6)))

  early_specad <- expression(class %in% c("special", "admin") &
                               (yos >= 6 & age >= 53))

  early_other <- expression(!class %in% c("special", "admin") &
                              (yos >= 6 & age >= 58))

  type <- case_when(eval(norm_specad) |
                      eval(norm_other) ~ "norm",
                    eval(early_specad) |
                      eval(early_other) ~ "early",
                    yos >= 6 ~ "vested",
                    .default = "non_vested")
  paste0("tier_1_", type)
}

tier2_type <- function(class, age, yos){

  norm_specad <- expression(class %in% c("special", "admin") &
                              (yos >= 30 |
                                 (age >= 60 & yos >= 8)))

  norm_other <- expression(!class %in% c("special", "admin") &
                             yos >= 33 |
                             (age >= 65 & yos >= 8))

  early_specad <- expression(class %in% c("special", "admin") &
                               (yos >= 8 & age >= 56))

  early_other <- expression(!class %in% c("special", "admin") &
                              (yos >= 8 & age >= 61))

  type <- case_when(eval(norm_specad) |
                      eval(norm_other) ~ "norm",
                    eval(early_specad) |
                      eval(early_other) ~ "early",
                    yos >= 8 ~ "vested",
                    .default = "non_vested")
  paste0("tier_2_", type)
}

tier3_type <- function(class, age, yos){

  norm_specad <- expression(class %in% c("special", "admin") &
                              (yos >= 30 |
                                 (age >= 60 & yos >= 8)))

  norm_other <- expression(!class %in% c("special", "admin") &
                             yos >= 33 |
                             (age >= 65 & yos >= 8))

  early_specad <- expression(class %in% c("special", "admin") &
                               (yos >= 8 & age >= 56))

  early_other <- expression(!class %in% c("special", "admin") &
                              (yos >= 8 & age >= 61))

  type <- case_when(eval(norm_specad) |
                      eval(norm_other) ~ "norm",
                    eval(early_specad) |
                      eval(early_other) ~ "early",
                    yos >= 8 ~ "vested",
                    .default = "non_vested")
  paste0("tier_3_", type)
}


get_tier <- function(class, entry_year, age, yos, new_year){
  tier_type <- case_when(
    entry_year < 2011 ~ tier1_type(class, age, yos),
    entry_year < new_year ~ tier2_type(class, age, yos),
    entry_year >- new_year ~ tier3_type(class, age, yos),
    .default = "ERROR"
  )
  tier_type
}


# check <- tribble(
#   ~class, ~age, ~yos,
#   "special", 30, 5,
#   "special", 20, 25,
#   "special", 60, 8
# )
#
# new_year_ <- 2024
# check |>
#   # filter(row_number()==2) |>
#   mutate(t1=tier1_type(class, age, yos),
#          tier=get_tier(class, rep(100, 3), age, yos))


