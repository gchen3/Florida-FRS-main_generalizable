
#' Present value of a future payment
#'
#' @param rate Annual discount rate (scalar double).
#' @param g Growth rate (details needed).
#' @param nper Number of periods per year (scalar).
#' @param pmt Future payment, per period per year (scalar double).
#' @param t Number of years in the future (scalar).
#'
#' @return Numeric value.
#' @export
#'
#' @examples
#' pv(.1, g=0, nper=1, pmt=100, t=1) # 90.90909
#' pv(.1, g=0, nper=1, pmt=100, t=2) # 82.644628
#' pv(.1, g=0, nper=2, pmt=100, t=2) # 157.7761
#' pv(.1, g=.05, nper=2, pmt=100, t=2) # 161.5327
pv <- function(rate, g = 0, nper, pmt, t = 1) {
  # I don't quite understand this function
  r <- (1 + rate)/(1 + g) - 1 # djb rate adjusted for growth - real rate?
  PV <- pmt / r * (1 - (1 / (1 + r)^nper)) / (1 + g) * (1 + rate)^(1 - t)
  return(PV)
}
