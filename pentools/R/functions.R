
#' Present value of annuity (same amount or same growth in N periods)
#'
#' @param rate Annual discount rate (scalar double).
#' @param g Annual growth rate of the annuity amount (scalar double).
#' @param nper Number of periods/years (scalar).
#' @param pmt Annuity amount in the beginning year (scalar double).
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
  r <- (1 + rate)/(1 + g) - 1
  PV <- pmt / r * (1 - (1 / (1 + r)^nper)) / (1 + g) * (1 + rate)^(1 - t)
  return(PV)
}

# Reference: financial mathematics for actuaries 3rd
# Link: https://actuarialscience.yolasite.com/resources/Ruckman.pdf

# ann_fac calculate the annuity factor with rate and t
# pv = fv * ann_fac
# fv = pv * (1 + rate) ^ t
# Reference: p.4
ann_fac <- function (rate, t) {
  vt <- (1 + rate) ^ (-t)
  return(vt)
}

# level_ann_fac calculates the annuity (same payment at equal intervals of time) for 1 unit (such as $1) with rate and t
# Reference: p. 21
level_ann_fac <- function (rate, t) {
  vt <- (1 + rate) ^ (-t)
  a_nt <- (1 - vt) / rate
  return(a_ni)
  }

level_ann_fac(.08, 18)

# growth_ann_fac calculates the present value of a growth annuity for 1 unit (such as $1) with interest rate, growth rate and t
# Reference: p. 71 "Compound Increasing Annuity-Immediate Present Value Factor" with some adjustments
growth_ann_fac <- function (rate, growth, t) {
#    j <- ((1 + rate) / (1 + growth)) - 1
#    ia_nt <- (1 / (1 + rate)) * level_ann_fac(j, t)
#    ia_nt = (1 - ((1 + growth)/(1 + rate))^t) / (rate - growth)
#    delta <- log(1 + j)
#    vt <- (1 + j) ^ (-t)
#    ia_nt <- (level_ann_fac(j, t) - t * vt) / delta
  return(ia_nt)
}

pv(0.08, g=0.05, nper=18, pmt=100, t=1)
100 * growth_ann_fac (0.08, 0.05, 18)
pv(.08, g=0.05, nper=2, pmt=100, t=1)
100 / growth_ann_fac (r = 0.1, g = 0.05, t = 2)

