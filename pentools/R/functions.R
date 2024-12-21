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

#' Present Value of a Growing Annuity
#'
#' This function calculates the present value (PV) of a growing annuity, taking into account
#' the interest rate, growth rate, number of periods, payment amount, and timing of payments.
#'
#' @param rate Numeric. The interest rate per period (e.g., 0.05 for 5%).
#' @param g Numeric. The growth rate of payments per period (default is 0).
#' @param nper Integer. The total number of periods (e.g., 10 for 10 periods).
#' @param pmt Numeric. The payment amount per period.
#' @param t Integer. Timing of payments:
#'   - \code{t = 1}: Payments occur at the end of the period (default).
#'   - \code{t = 0}: Payments occur at the beginning of the period.
#'
#' @return Numeric. The present value of the growing annuity.
#'
#' @examples
#' # Example: Calculate PV for a growing annuity
#' pv(rate = 0.05, g = 0.02, nper = 10, pmt = 1000, t = 1)
#'
#' @export
pv <- function(rate, g = 0, nper, pmt, t = 1) {
  r <- (1 + rate)/(1 + g) - 1
  PV <- pmt/r * (1 - (1 / (1 + r)^nper)) / (1 + g) * (1 + rate)^(1 - t)
  return(PV)
}

#' Rolling Present Value Calculation
#'
#' This function computes the rolling present value of a series of payments (cash flows) over time.
#' The first value in the \code{pmt_vec} vector must be zero, as it is a placeholder for calculations.
#'
#' @param rate Numeric. The interest rate per period (e.g., 0.05 for 5%).
#' @param g Numeric. The growth rate of payments per period (default is 0).
#' @param nper Integer. The total number of periods (e.g., 10 for 10 periods).
#' @param pmt_vec Numeric vector. A vector of payment amounts over the periods.
#'   The first value in this vector must be zero.
#' @param t Integer. Timing of payments:
#'   - \code{t = 1}: Payments occur at the end of the period (default).
#'   - \code{t = 0}: Payments occur at the beginning of the period.
#'
#' @return Numeric vector. A vector containing the rolling present values for each period.
#'
#' @details
#' The rolling present value is calculated iteratively:
#' \itemize{
#'   \item For the first period (\code{i = 1}), the present value is calculated using the \code{pv} function for the second payment.
#'   \item For subsequent periods (\code{i > 1}), the present value is updated by rolling forward the previous value with interest
#'   and subtracting the adjusted payment.
#' }
#' The formula for the rolling present value is:
#' \deqn{PV[i] = PV[i-1] \cdot (1 + rate) - pmt_vec[i] \cdot (1 + rate)^{1 - t}}
#'
#' @examples
#' # Example: Calculate rolling PV for a series of payments
#' pmt_vec <- c(0, 1000, 1200, 1300, 1100, 1400)
#' roll_pv(rate = 0.05, g = 0.02, nper = 10, pmt_vec = pmt_vec, t = 1)
#'
#' @seealso
#' \code{\link{pv}} for the present value calculation used within this function.
#'
#' @export
roll_pv <- function(rate, g = 0, nper, pmt_vec, t = 1) {
  pv_vec <- double(length(pmt_vec))
  for (i in 1:length(pv_vec)) {
    if (i == 1) {
      pv_vec[i] <- pv(rate, g, nper, pmt_vec[2], t)
    } else {
      pv_vec[i] <- pv_vec[i-1] * (1 + rate) - pmt_vec[i] * (1 + rate)^(1 - t)
    }
  }

  return(pv_vec)
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

#' get_pmt0 calculates the total first payment of an annuity due with a present value (pv),
#' interest rate (r), and the number of periods (nper).
#' This function multiplies the payment factor (calculated using get_pmt_due) by the present value.
#'
#' @param r Annual discount rate (scalar double).
#' @param nper Number of periods (scalar integer).
#' @param pv Present value of the annuity (scalar numeric).
#'
#' @return numeric value representing the total first payment.
#' @export
#'
#' @examples
#' get_pmt0(0.05, 5, 1000) # Example: calculates the total first payment
get_pmt0 <- function(r, nper, pv) {
  get_pmt_due(r, nper)*pv
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
        PVFB[i = N] <- NA                                 # At the last period, there are no future periods, so PVFB is 0
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


#' Calculate Present Value of Future Salaries (PVFS)
#'
#' This function computes the present value of future salaries (PVFS) for a given set of probabilities, interest rates, and salary values over a time period.
#'
#' @param remaining_prob_vec Numeric vector. The remaining survival probabilities for each period.
#' @param interest_vec Numeric vector. The interest rates for each period.
#' @param sal_vec Numeric vector. The projected salaries for each period.
#'
#' @return Numeric vector. A vector of PVFS values, where each element represents the present value of future salaries starting from that period.
#'
#' @details
#' The present value of future salaries is calculated by:
#' - Adjusting projected salaries by the survival probability at each period.
#' - Discounting future salaries to their present value using the interest rates provided.
#'
#' @examples
#' # Example inputs
#' remaining_prob_vec <- c(1, 0.95, 0.90, 0.85)
#' interest_vec <- c(0.03, 0.03, 0.03, 0.03)
#' sal_vec <- c(50000, 52000, 54000, 56000)
#'
#' # Calculate PVFS
#' get_pvfs(remaining_prob_vec, interest_vec, sal_vec)
#'
#' @export
get_pvfs <- function(remaining_prob_vec, interest_vec, sal_vec) {
  N <- length(sal_vec)
  PVFS <- double(length = N)
  for (i in 1:N) {
    remaining_prob_sub <- remaining_prob_vec[i:N] / remaining_prob_vec[i] # Calculate survival probabilities for future periods, using i year survival rate as the base
    interest <- interest_vec[i]                                           # Get the interest rate for the current period
    sal_sub <- sal_vec[i:N]                                               # Subset salaries for future periods from i period
    df_sub  <- (1 + interest)^(-(1:length(sal_sub)))                      # Discount factors in each year based on the interest rate used in t
    PVFS[i] <- sum(sal_sub * remaining_prob_sub * df_sub)                 # The sum of product of the future salaries, survival probability, and discount factor is present value of future salaries
  }
  return(PVFS)
}

#' Recursive Growing Function with Lag
#'
#' This function calculates a series of values that grow recursively based on an initial value and a vector of growth rates, incorporating a lag effect.
#'
#' @param x Numeric vector. A vector where the first element represents the initial value, and the rest are placeholders that will be replaced with recursively calculated values.
#' @param g Numeric vector. A vector of growth rates for each period, expressed as decimals (e.g., 0.05 for 5% growth).
#'
#' @return Numeric vector. A vector of the same length as \code{x}, where the first value is unchanged, and subsequent values are recursively grown based on \code{g}.
#'
#' @details
#' - The cumulative growth is calculated using \code{cumprod(1 + g)}, which computes the cumulative product of \code{1 + g}.
#' - Values in \code{x[2:length(x)]} are calculated as \code{x[1] * g_cul}, where \code{g_cul} is the cumulative growth factor.
#' - The lag effect ensures that the growth is applied recursively from the initial value.
#'
#' @examples
#' # Example with an initial value and growth rates
#' x <- numeric(5)
#' x[1] <- 100  # Initial value
#' g <- c(0.05, 0.03, 0.02, 0.04)
#' recur_grow(x, g)
#'
#' @export
recur_grow <- function(x, g) {
  g_cul <- cumprod(1 + g)
  x[2:length(x)] <- x[1] * g_cul[1:(length(g) - 1)]
  return(x)
}

#' Recursive Growing Function (No Lag)
#'
#' This function calculates a series of values that grow recursively based on an initial value and a vector of growth rates, without incorporating a lag effect.
#'
#' @param x Numeric vector. A vector where the first element represents the initial value, and the rest are placeholders that will be replaced with recursively calculated values.
#' @param g Numeric vector. A vector of growth rates for each period, expressed as decimals (e.g., 0.05 for 5% growth).
#'
#' @return Numeric vector. A vector of the same length as \code{x}, where the first value is unchanged, and subsequent values are recursively grown based on \code{g} without a lag.
#'
#' @details
#' - The growth vector \code{g} is shifted to remove the lag, so \code{g[1]} represents the growth for the first step.
#' - The function then calls \code{\link{recur_grow}} to perform the recursive growth calculation.
#'
#' @examples
#' # Example with an initial value and growth rates
#' x <- numeric(5)
#' x[1] <- 100  # Initial value
#' g <- c(0.05, 0.03, 0.02, 0.04)
#' recur_grow2(x, g)
#'
#' @seealso \code{\link{recur_grow}} for the version with lag.
#'
#' @export
recur_grow2 <- function(x, g) {
  g[1:length(g)-1] <- g[2:length(g)]
  recur_grow(x, g)
}

#' Recursive Growing Function with a Single Base and Fixed Growth Rate
#'
#' This function calculates a series of values that grow recursively starting from a single base value, using a fixed growth rate over a specified number of periods.
#'
#' @param x Numeric. The base value from which the growth calculation begins.
#' @param g Numeric. The fixed growth rate, expressed as a decimal (e.g., 0.05 for 5% growth per period).
#' @param nper Integer. The total number of periods for the growth calculation, including the initial value.
#'
#' @return Numeric vector. A vector of length \code{nper}, where the first element is \code{x}, and subsequent elements are calculated by applying the fixed growth rate recursively.
#'
#' @details
#' - The growth rate \code{g} is applied recursively to generate a series of growth factors.
#' - The initial value \code{x} is included as the first element of the resulting vector, followed by the recursively grown values.
#'
#' @examples
#' # Example with a base value and a fixed growth rate
#' x <- 100  # Initial value
#' g <- 0.05  # Growth rate (5%)
#' nper <- 5  # Number of periods
#' recur_grow3(x, g, nper)
#'
#' @export
recur_grow3 <- function(x, g, nper) {
  growth_factors <- cumprod(rep(1 + g, nper - 1))
  x_vec <- c(x, x * growth_factors)
  return(x_vec)
}



# not yet developed -------------------------------------------------------


#' @export
get_pmt <- function(r, g = 0, nper, pv, t = 1) {
  a <- get_pmt0((1+r)/(1+g) - 1, nper, pv*(1+r)^t)
  return(a)
}

#Cumulative future values function (with interest being a single value)
#' @export
get_cum_fv <- function(interest, cashflow, first_value = 0){
  cumvalue <- double(length = length(cashflow))
  cumvalue[1] <- first_value
  for (i in 2:length(cumvalue)) {
    cumvalue[i] <- cumvalue[i - 1]*(1 + interest) + cashflow[i - 1]
  }
  return(cumvalue)
}

#Adding new entrants function
#' @export
add_new_entrants <- function(g, ne_dist, wf1, wf2, ea, age, position_matrix){
  #g is the assumed population growth of the plan
  #ne_dist is a vector representing the distribution of new entrants for each entry age
  #wf1 is the population in period 1. wf2 is the wf1 population after decremented
  #ea and age are two vectors representing entry age and age for active members
  #position_matrix is the matrix that rearranges the new entrant numbers in the right positions to be added to the active workforce array
  ne <- sum(wf1)*(1 + g) - sum(wf2)
  ne_vec <- ne * ne_dist
  ne_matrix <- matrix(ne_vec, nrow = length(ea), ncol = length(age))
  ne_matrix_trans <- ne_matrix * position_matrix

  return(ne_matrix_trans)
}
