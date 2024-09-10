#' FRS (Florida Retirement System) Environment
#'
#' An environment containing data and functions specific to FRS.
#'
#' @name frs
#' @export
frs <- new.env(parent = emptyenv())

#' Load FRS Data
#'
#' @keywords internal
load_frs_data <- function() {
  frs_data_path <- system.file("data", "frs_data.rda", package = "pendata")
  if (frs_data_path == "") {
    warning("frs_data.rda not found in the package. Returning empty list.")
    return(list())
  }
  load(frs_data_path, envir = environment())
  if (!exists("frs_data")) {
    warning("frs_data object not found in the data file. Returning empty list.")
    return(list())
  }
  return(frs_data)
}

#' Calculate FRS Benefits
#'
#' @param years_of_service Number of years of service
#' @param final_average_salary Final average salary
#' @return Calculated benefit amount
#' @name frs_calculate_benefits
NULL

#' @rdname frs
#' @export
frs$calculate_benefits <- function(years_of_service, final_average_salary) {
  benefit <- years_of_service * 0.016 * final_average_salary
  return(benefit)
}

# Initialize the data
frs$data <- NULL  # This will be properly initialized when accessed
