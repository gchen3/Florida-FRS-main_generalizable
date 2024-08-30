#' Florida Retirement System Data
#'
#' @docType data
#'
#' @format A saved environment containing data elements and functions related to the Florida Retirement System.
#'
#' @source Data collected from various sources, including official reports and databases.
#'
#' @section Data Elements:
#'   \describe{
#'   \item{base_mort_table}{A tibble containing base (unimproved) mortality
#'   rates for employees and beneficiaries of the Florida Retirement System,
#'   based on SOA's Pub.H-2010 Headcount-Weighted Mortality Rates, with
#'   modifications to adapt it to FRS.} }
#'
#  #' @section Functions:
#  #'   \describe{
#  #'     \item{calculate_benefits}{A function to calculate benefits based on the provided data.}
#  #'   }
#  #'
#' @examples
#' data(frs, package = "pendata")
#' # Accessing a specific data element within the environment
#' frs$base_mort_table
# #' # Using a function within the environment
# #' frs$calculate_benefits()
"frs"
