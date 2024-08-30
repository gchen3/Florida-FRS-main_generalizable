#' Pub.H-2010 Headcount-Weighted Mortality Rates
#'
#' The Pub.H-2010 Headcount-Weighted Mortality Rates provided by the Society
#'   of Actuaries in the Excel file pub-2010-headcount-mort-rates.xlsx. It has
#'   mortality rates for general, safety, and teacher workers as of 2010.
#'
#' @format A tibble with 1,878 rows and 5 variables:
#' \describe{
#'   \item{employee_type}{general, safety, or teacher}
#'   \item{beneficiary_type}{e.g., employee, healthy_retiree}
#'   \item{gender}{male or female}
#'   \item{age}{Age of person in years}
#'   \item{rate}{Mortality rate}
#' }
#' @source Where you got the data from (e.g., \url{http://example.com})
"pub2010hc_mortality_rates"
