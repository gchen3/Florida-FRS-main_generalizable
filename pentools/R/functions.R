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
#' pv(0.08, g=0.05, nper=18, pmt=100, t=1) # 90.90909
#' pv(.1, g=0, nper=1, pmt=100, t=2) # 82.644628
#' pv(.1, g=0, nper=2, pmt=100, t=2) # 157.7761
#' pv(.1, g=.05, nper=2, pmt=100, t=2) # 161.5327
pv <- function(rate, g = 0, nper, pmt, t = 1) {
  r <- (1 + rate)/(1 + g) - 1
  PV <- pmt / r * (1 - (1 / (1 + r)^nper)) / (1 + g) * (1 + rate)^(1 - t)
  return(PV)
}

#' get_pv calculate the discount factor with rate and t
#'
#' @param rate Annual discount rate (scalar double).
#' @param t Number of years in the future (scalar).
#'
#' @return Numeric value.
#' @export
#'
#' @examples
#' get_pv(0.05,5) # 0.7835
get_pv <- function (rate, t) {
  vt <- (1 + rate) ^ (-t)
  return(vt)
}


#' get_pv_pmt calculates the present value for an annuity (same payment at equal intervals of time) for 1 unit (such as $1) with rate and t
#' Reference:financial mathematics for actuaries 3rd (p.21)
#' Link: https://actuarialscience.yolasite.com/resources/Ruckman.pdf
#'
#' @param rate Annual discount rate (scalar double).
#' @param t Number of years in the future (scalar).
#'
#' @return Numeric value.
#' @export
#'
#' @examples
#' get_pv_pmt(.05, 5) # 4.3295
get_pv_pmt <- function (rate, t) {
  if (rate == 0) {
    pv_pmt <- t
  } else {
    vt <- (1 + rate) ^ (-t)
    pv_pmt <- (1 - vt) / rate
    }
  return(pv_pmt)
  }


#' get_pv_gpmt calculates the present value of a growth annuity for 1 unit (such as $1) with interest rate, growth rate and t
#' Reference: https://financeformulas.net/Present_Value_of_Growing_Annuity.html
#'
#' @param rate Annual discount rate (scalar double).
#' @param growth Annual growth rate (scalar double).
#' @param t Number of years in the future (scalar).
#'
#' @return Numeric value.
#' @export
#'
#' @examples
#' get_pv_gpmt(0.08, 0.05, 18) # 13.2582
get_pv_gpmt <- function (rate, growth, t) {
  if (rate == growth) {
    pv_gpmt = t
  } else {
  pv_gpmt = (1 - ((1 + growth)/(1 + rate))^t) / (rate - growth)
  return(pv_gpmt)
  }
}


# Compare with the pv function
# pv(0.08, g=0.05, nper=18, pmt=100, t=1)
# 100 * get_pv_gpmt (0.08, 0.05, 18) * vt(0.08, 0)
# pv(.08, g=0.05, nper=2, pmt=100, t= 2)
# 100 * get_pv_gpmt (r = 0.08, g = 0.05, t = 2) * vt(0.08, 1)

#' get_pv_cf calculates the present value of future cashflows (cf) 
#'
#' @param rate Annual discount rate (scalar double).
#' @param cf A list of cash flows in n=length(cf) number of years
#'
#' @return Numeric value.
#' @export
#'
#' @examples
#' cf <- c(100, 200, 300, 400, 500, 600)
#' get_pv_cf(0.05, cf)  # 1704.37
get_pv_cf <- function(rate, cf) {
  pv <- 0
  for(i in 1:length(cf)) {
    pv <- pv + cf[i] * (get_pv(rate, i))
  }
  return(pv)
}


#' get_pv_cf_roll returns the remaining present value of future cash flows (cf) in every year forward
#' 
#' @param rate Annual discount rate (scalar double).
#' @param cf A list of cash flows in n=length(cf) number of years
#'
#' @return A list of numeric values
#' @export
#'
#' @examples
#' cf <- c(100, 200, 300, 400, 500, 600)
#' get_pv_cf_roll (0.05, cf) #$1,704.37 $1,609.13 $1,427.72 $1,168.57 $839.49) $447.73
get_pv_cf_roll <- function(rate, cf) {
  pv <- numeric(length(cf))
  for(i in 1:length(cf)) {
  pv[i] <- get_pv_cf(rate, cf[i:length(cf)])
  }
  return(pv)
}


#' get_pmt_due calculates the first payment of an annuity due with a present value pv, interest rate (rate), and remaining period (t)
#' payments are made in advance (beginning of each time period)
#' Reference: Annuity Due Payment - PV, https://financeformulas.net/Annuity-Due-Payment-from-Present-Value.html
#' Title
#'
#' @param rate Annual discount rate (scalar double).
#' @param t Number of years in the future (scalar).
#'
#' @return numeric value.
#' @export
#'
#' @examples
#' get_pmt_due(0.05, 5) # 0.219976
get_pmt_due <- function(rate, t) {
  if (rate == 0) {
    pmt = 1/t
  } else {
  pmt = (rate / (1 -(1 + rate) ^ (-t))) * (1 / (1 + rate))
  }
  return(pmt)
}


#' get_pmt_growth calculates the first payment for an growth annuity due with a present value pv, 
#' interet rate (rate), # remaining period (t), and growth rate (g)
#' payments are made in advance (beginning of each time period)
#'
#' @param rate Annual discount rate (scalar double).
#' @param growth Annual growth rate (scalar double).
#' @param t Number of years in the future (scalar).
#'
#' @return numeric value.
#' @export
#'
#' @examples
#' get_pmt_growth(0.05, 0.02, 5) #0.2117597
get_pmt_growth <- function(rate, growth, t) {
  if (rate == growth) {
  pmt_growth = 1/t
  } else {
  pmt_growth = ((rate - growth) / (1 - ((1 + growth) / (1 + rate)) ^ t)) * (1 / (1 + rate))
  }
  return(pmt_growth)
}
