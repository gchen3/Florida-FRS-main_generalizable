#' Reformat FRS base mortality table
#'
#' @param etype String in c("general", "regular", "teacher")
#' @param base_mort_table The base mortality table to reformat
#' @return A reformatted data frame for use in Reason model
#' @export
reformat_base_mortality <- function(etype, base_mort_table) {
  dfnew <- base_mort_table |>
    dplyr::filter(employee_type == etype) |>
    tidyr::pivot_wider(names_from = c(beneficiary_type, gender), values_from = rate)

  if(etype == "regular") dfnew <- as.data.frame(dfnew)

  return(dfnew)
}
