#' Florida Retirement System Environment
#'
#' An environment containing data elements and functions related to the Florida Retirement System.
#'
#' @export
frs <- new.env(parent = emptyenv())

#' Initialize the FRS environment
#'
#' @keywords internal
.init_frs_env <- function() {
  # Load the data into the frs environment
  utils::data("base_mort_table", envir = frs, package = "pendata")

  # Assign the function to the frs environment
  frs$reformat_base_mortality <- function(etype) {
    reformat_base_mortality(etype, frs$base_mort_table)
  }
}
