#' Calculate Present Value Factor
#'
#' This function computes the present value factor for a given interest rate and time period.
#'
#' @param rate Numeric. The interest rate per period as a decimal (e.g., 0.05 for 5%).
#' @param t Numeric. The time period for which the present value factor is calculated.
#'
#' @return Numeric. The present value factor, which is the discount factor (1 + rate)^(-t)
#'
#' @details
#' The present value factor is used to discount future cash flows to their present value.
#'
#' @examples
#' # Calculate the present value factor for a 5% interest rate over 3 periods
#' get_pv(rate = 0.05, t = 3)
#'
#' # Calculate the present value factor for a 10% interest rate over 1 period
#' get_pv(rate = 0.10, t = 1)
#'
#' @export
get_pv <- function (rate, t) {
  vt <- (1 + rate) ^ (-t)
  return(vt)
}


#' Calculate Present Value of an Annuity Payment
#'
#' This function computes the present value of an annuity payment over a given time period and interest rate.
#'
#' @param rate Numeric. The interest rate per period as a decimal (e.g., 0.05 for 5%). If the rate is 0, the present value is equal to the total number of periods.
#' @param t Numeric. The total number of periods for the annuity payments.
#'
#' @return Numeric. The present value of the annuity payments.
#'
#' @examples
#' # Present value of an annuity with 5% interest over 10 periods
#' get_pv_pmt(rate = 0.05, t = 10)
#'
#' # Present value of an annuity with 0% interest over 10 periods
#' get_pv_pmt(rate = 0, t = 10)
#'
#' @export
get_pv_pmt <- function (rate, t) {
  if (rate == 0) {
    pv_pmt <- t
  } else {
    vt <- (1 + rate) ^ (-t)
    pv_pmt <- (1 - vt) / rate
    }
  return(pv_pmt)
  }


#' Calculate Present Value of a Growing Annuity Payment
#'
#' This function computes the present value of a series of growing annuity payments over a specified time period, given an interest rate and growth rate.
#'
#' @param rate Numeric. The interest rate per period as a decimal (e.g., 0.05 for 5%).
#' @param growth Numeric. The growth rate of the payments per period as a decimal (e.g., 0.02 for 2%).
#' @param t Numeric. The total number of periods for the growing annuity payments.
#'
#' @return Numeric. The present value of the growing annuity payments.
#'
#' @examples
#' # Present value of a growing annuity with 5% interest, 2% growth, over 10 periods
#' get_pv_gpmt(rate = 0.05, growth = 0.02, t = 10)
#'
#' # Present value of a growing annuity with 5% interest, 5% growth, over 10 periods
#' get_pv_gpmt(rate = 0.05, growth = 0.05, t = 10)
#'
#' @export
get_pv_gpmt <- function (rate, growth, t) {
  if (rate == growth) {
    pv_gpmt = t
  } else {
  pv_gpmt = (1 - ((1 + growth)/(1 + rate))^t) / (rate - growth)
  return(pv_gpmt)
  }
}

#' npv calculates the present value of future cashflows (cf)
#'
#' @param rate Annual discount rate (scalar double).
#' @param cf A list of cash flows in n=length(cf) number of years
#'
#' @return Numeric value.
#' @export
#'
#' @examples
#' cf <- c(100, 200, 300, 400, 500, 600)
#' npv(0.05, cf)  # 1704.37
npv <- function(rate, cf) {
  df <- (1+rate)^(-(1:(length(cf))))    # Discount factor in each year based on rate
  pv <- sum(cf * df)                    # The sum of the product of cash flow and discount factor in each year is PV
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
  pv[i] <- npv(rate, cf[i:length(cf)])
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

#' Calculate the Present Value of Future Benefits (PVFB)
#'
#' Given a vector of separation rates, a vector of corresponding interest rates, and a vector of
#' future values (benefits), this function computes the present value of these benefits. At each
#' period `i`, it calculates the probability of separation and then discounts the subsequent
#' future values back to the present using the provided interest rates.
#'
#' @param sep_rate_vec Numeric vector. The annual separation rates for each future period.
#' @param interest_vec Numeric vector. The annual interest (discount) rates use in that period.
#' @param value_vec Numeric vector. The future benefits payable at each period.
#'
#' @return A numeric vector of the same length as `value_vec`, where each element represents the
#'         present value of future benefits starting from that period.
#' @export
#'
#' @examples
#' sep_rate_vec <- c(0.01, 0.02, 0.03, 0.04)
#' interest_vec <- c(0.05, 0.05, 0.05, 0.05)
#' value_vec <- c(100, 200, 300, 400)
#' get_pvfb(sep_rate_vec, interest_vec, value_vec)
get_pvfb <- function(sep_rate_vec, interest_vec, value_vec) {
    N <- length(value_vec)
    PVFB <- double(length = N)
    for (i in 1:N) {
      sep_rate <- sep_rate_vec[i:N]
      sep_prob <- cumprod(1 - sep_rate) * sep_rate       # Probability of separating in each subsequent period
      interest <- interest_vec[i]
      if (i < N) {
        value_sub <- value_vec[(i+1):N]                  # Payment in t+1 until the end of periods
        sep_prob_sub <- sep_prob[-1]                     # Probability of remaining in the plan until the period t
        df_sub <- (1 + interest)^(-(1:length(value_sub))) # Discount factors in each year based on the interest rate used in t
        PVFB[i] <- sum(value_sub * sep_prob_sub * df_sub) # The product of probability, discount factor, future values (benefits) is PVFB
      } else {
        PVFB[i = N] <- 0                                 # At the last period, there are no future periods, so PVFB is 0
      }
    }
    return(PVFB)
  }


#' Calculate Annuity Factors with Survival and Cost-of-Living Adjustments
#'
#' This function computes annuity factors for a series of survival discount rates, incorporating cost-of-living adjustments (COLA). It optionally supports a one-time COLA adjustment.
#'
#' @param surv_DR_vec Numeric vector. A vector of survival discount rates, representing the probabilities of survival for each period.
#' @param cola_vec Numeric vector. A vector of cost-of-living adjustment (COLA) rates for each period.
#' @param one_time_cola Logical, optional. If TRUE, a one-time COLA is applied (default is FALSE), this is not coded as no COLAs.
#'
#' @return Numeric vector. A vector of annuity factors, considering COLAs where each element corresponds to a period's annuity factor.
#'
#' @examples
#' # Example with survival discount rates and COLA rates
#' surv_DR_vec <- c(1.0, 0.9, 0.8, 0.7)
#' cola_vec <- c(0.02, 0.02, 0.02, 0.02)
#' annfactor(surv_DR_vec, cola_vec)
#'
#' # Example with a one-time COLA
#' annfactor(surv_DR_vec, cola_vec, one_time_cola = FALSE)
#'
#' @export
annfactor <- function(surv_DR_vec, cola_vec, one_time_cola = FALSE) {
  N <- length(surv_DR_vec)                                     # Define the length of the input vector
  annfactor_vec <- numeric(N)                                  # Create the output vector with the same length

  for (i in 1:N) {
    cola <- ifelse(one_time_cola, 0, cola_vec[i])              # If one-time COLA, the cola is 0 (Question: This actually means no COLAs)
    cola_project <- c(0, rep(cola, max(0, N - i)))             # Project COLA for future periods with the same COLA rate

    cumprod_cola <- cumprod(1 + cola_project)                  # Calculate the cumulative product of previous COLA rates
    surv_ratio <- surv_DR_vec[i:N] / surv_DR_vec[i]            # Set the base year survival as 1, calculate probability of survival in future years

    annfactor_vec[i] <- sum(surv_ratio * cumprod_cola)         # The sum of product of cumulative COLA increase and survival rates to a future year is the annuity factor considering COLA
  }

  return(annfactor_vec)
}

