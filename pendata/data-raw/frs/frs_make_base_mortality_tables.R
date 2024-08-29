
dir_pd <- r"(E:\R_projects\projects\Florida-FRS-main_generalizable\pendata)"
dir_draw <- fs::path(dir_pd, "data-raw")
dir_r <- fs::path(dir_pd, "R")
dir_reason <- fs::path(dir_draw, "reason_results")
dir_soa <- fs::path(dir_draw, "external", "soa")


# libraries ---------------------------------------------------------------

source(fs::path(dir_draw, "libraries.r"))

devtools::load_all()
data("pub2010hc_mortality_rates", package = "pendata")

# fill in missing employee and healthy retiree values and drop safety ----
mort_fillin <- pub2010hc_mortality_rates |>
  mutate(employee_type = ifelse(employee_type == "teachers", "teacher", employee_type)) |> # Reason used teacher, SOA uses teachers
  filter(employee_type %in% c("general", "teacher")) |>
  pivot_wider(names_from = beneficiary_type, values_from = rate) |>
  mutate(employee = ifelse(is.na(employee), healthy_retiree, employee), # higher ages are missing
         healthy_retiree = ifelse(is.na(healthy_retiree), employee, healthy_retiree)) |>
  pivot_longer(cols = -c(employee_type, gender, age),
               names_to = "beneficiary_type",
               values_to = "rate")

# construct mortality rates for regular employees and add to ----
# the frs mortality rates for regular are assumed by Reason to be the average of general and teacher
# Here is their code, which is executed AFTER they fill in missing employee and healthy retiree values
# base_regular_mort_table <- (base_general_mort_table + base_teacher_mort_table) / 2

mort_regular <- mort_fillin |>
  mutate(employee_type = "regular") |>
  summarise(rate = mean(rate, na.rm = TRUE),
            .by=c(employee_type, beneficiary_type, gender, age))

frs_mort_table <- bind_rows(mort_fillin, mort_regular) |>
  arrange(employee_type, beneficiary_type, gender, age, age)

usethis::use_data(frs_mort_table, overwrite = TRUE)



# test the mortality tables against Reason tables -------------------------

reformat_base_mortality <- function(etype, frs_mort_table, dfold) {
  dfnew <- frs_mort_table |>
    filter(employee_type == etype) |>
    pivot_wider(names_from = c(beneficiary_type, gender), values_from = rate) |>
    select(all_of(names(dfold)))

  class_dfold <- class(dfold)

  # Force the class of dfnew to be the same as dfold
  if (any(class_dfold == "tbl_df")) {
    # Convert dfnew to a tibble (tbl_df)
    dfnew <- as_tibble(dfnew)
  } else if (any(class_dfold == "data.frame")) {
    # Convert dfnew to a data.frame (if it's not already)
    dfnew <- as.data.frame(dfnew)
  } else {
    # Handle other classes if necessary
    stop("Unsupported class for dfold")
  }
  return(dfnew)
}

# the following test passes
for (etype in c("general", "regular", "teacher")) {
  print(etype)
  dfold <- reason[[paste0("base_", etype, "_mort_table")]]
  dfnew <- reformat_base_mortality(etype, frs_mort_table, dfold)
  expect_equal(dfnew, dfold)
}

class(dfold); class(dfnew)
str(dfold); str(dfnew)

# TODO: fix reformatting function to hard code the class of dfnew
class(reason$base_general_mort_table) # "tbl_df"     "tbl"        "data.frame"
class(reason$base_regular_mort_table) # "data.frame"
class(reason$base_teacher_mort_table) # "tbl_df"     "tbl"        "data.frame"


