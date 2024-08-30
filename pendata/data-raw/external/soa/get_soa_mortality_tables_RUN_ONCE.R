# SOA mortality tables ----------------------------------------------------


# folders -----------------------------------------------------------------

dir_pd <- r"(E:\R_projects\projects\Florida-FRS-main_generalizable\pendata)"
dir_draw <- fs::path(dir_pd, "data-raw")
dir_r <- fs::path(dir_pd, "R")
dir_reason <- fs::path(dir_draw, "reason_results")
dir_soa <- fs::path(dir_draw, "external", "soa")


# libraries ---------------------------------------------------------------

source(fs::path(dir_draw, "libraries.r"))


# download and save SOA mortality rate tables --------------------------------------

url <- "https://www.soa.org/49347a/globalassets/assets/files/resources/research-report/2019/pub-2010-headcount-mort-rates.xlsx"
# path_file(url)

download.file(url, fs::path(dir_soa, path_file(url)), mode = "wb")


# Function to get a mortality table from a single sheet ----

get_mort <- function(sheet, fullpath) {
  # read a single frs mortality table from a given sheet
  # create a long mortality table

  # each sheet has the employee type (e.g., teacher, safety, etc.) in A2
  employee_type <- read_excel(fullpath,
    sheet = sheet, range = "A2",
    col_names = "employee_type"
  ) |>
    pull(employee_type) |>
    str_to_lower()
  print(paste0("Getting employee type: ", employee_type))

  # get the raw mortality table, which we will clean
  mort1 <- suppressMessages(
    read_excel(fullpath,
      sheet = sheet,
      skip = 3,
      col_names = FALSE,
      col_types = "text"
    )
  )

  # identify columns to drop - those where value in row 2 (variable names)
  # is missing
  cols_to_drop <- mort1[2, ] |>
    unlist(use.names = FALSE) |>
    is.na()

  mort2 <- mort1[, !cols_to_drop]

  # create gender vector, telling which columns are male or female
  # pull gender values from first row
  gender <- mort2[1, 2:ncol(mort2)] |> # first row has gender
    unlist(use.names = FALSE) |> # convert to vector
    # carry forward (to the right) the nonmissing value, "male" or "female"
    # locf stands for "last observation carry forward"
    zoo::na.locf0() |>
    str_remove("^.*?; ") |> # remove everything before first "; "
    str_to_lower() |>
    str_sub(1, -2) # remove s on the end
  # we now have a vector such as c("male", "male", "male", "female", ...)

  # pull variable names from 2nd row
  colnames_base <- mort2[2, ] |>
    unlist(use.names = FALSE) |>
    str_replace_all(" ", "_") |>
    str_remove_all("\\*") |>
    str_to_lower()

  # column names have values such as healthy_retiree__male ...
  colnames <- c(
    colnames_base[1],
    # use double underscore to make separation easy later
    paste0(colnames_base[-1], "__", gender)
  )

  mort3 <- mort2 |>
    setNames(colnames) |>
    filter(row_number() > 2) |>
    mutate(age = as.integer(age)) |>
    pivot_longer(-age, values_to = "rate") |>
    # here's where the double underscore is helpful
    separate(name, into = c("beneficiary_type", "gender"), sep = "__") |>
    mutate(employee_type = employee_type, rate = as.numeric(rate)) |>
    select(employee_type, beneficiary_type, gender, age, rate) |>
    arrange(employee_type, beneficiary_type, gender, age)

  mort3 # return a long mortality table for a single sheet
}


# Read and stack the SOA mortality tables ----

# .. general, teacher, and safety all involve the same cleaning ----

fname <- "pub-2010-headcount-mort-rates.xlsx"
fullpath <- fs::path(dir_soa, fname)

sheets <- c("PubT.H-2010", "PubS.H-2010", "PubG.H-2010") # teacher, safety general

mort <- sheets |>
  purrr::map(\(x) get_mort(x, fullpath)) |>
  list_rbind() |>
  filter(!is.na(rate))


## Save the SOA mortality rates ----

# Save the mortality rates in the package data folder as `pub2010hc_mortality_rates`, making them available to users of `pendata`.

pub2010hc_mortality_rates <- mort

usethis::use_data(pub2010hc_mortality_rates, overwrite = TRUE)
# usethis::use_data(pub2010hc_mortality_rates, "external/data_file.rda", overwrite = TRUE)


# test against Reason data after frs revisions ------------------------------------------------

# dir_reason <- r"(E:\R_projects\projects\Florida-FRS-main_generalizable\refactor\reason_results)"
# fpath <- fs::path(dir_reason, "reason_workspace.RData")


## Show the SOA mortality rates ----
# avoid the need to install
# devtools::load_all()
#
# DT::datatable(pub2010hc_mortality_rates,
#           options = list(scrollX = TRUE, scrollY = TRUE, paging = TRUE, pageLength = 20),
#           filter="top") |>
#   formatPercentage(columns = c("rate"), digits=3)
#
# ## Inspect the mortality tables
#
# # names(mort) |> kable(caption="Names")
#
# head(mort) |>
#   gt() |>
#   tab_header("head of the mortality table")
#
# count(mort, employee_type) |>
#   gt() |>
#   tab_header("counts by employee type")
#
# count(mort, beneficiary_type) |>
#   gt() |>
#   tab_header("counts by beneficiary type")
#
# count(mort, gender) |>
#   gt() |>
#   tab_header("counts by gender")
