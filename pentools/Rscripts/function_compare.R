npv_reason <- function(rate, cashflows) {
  for(i in 1:length(cashflows)){
    if(i == 1){
      NPV <- cashflows[i]/((1+rate)^(i))
    } else {
      NPV <- NPV + cashflows[i]/((1+rate)^(i))
    }
  }

  return(NPV)
}

npv <- function(rate, cashflows) {
  df <- (1+rate)^(-(1:(length(cashflows))))
  pv <- sum(cashflows * df)
  return(pv)
}

rate <- 0.05
cashflows <- c(100, 240, 300, 410, 520)

npv_reason(rate, cashflows)
npv(rate, cashflows)

microbenchmark(npv_reason(rate, cashflows), npv(rate, cashflows), times = 1000)

get_pvfb_reason <- function(sep_rate_vec, interest_vec, value_vec) {
  PVFB <- double(length = length(value_vec))
  for (i in 1:length(value_vec)) {
    sep_rate <- sep_rate_vec[i:length(sep_rate_vec)]
    #sep_prob in a given year is the probability that the member will survive all the previous years and get terminated exactly in the given year
    sep_prob <- cumprod(1 - lag(sep_rate, n = 2, default = 0)) * lag(sep_rate, default = 0)
    interest <- interest_vec[i]
    value <- value_vec[i:length(value_vec)]
    value_adjusted <- value * sep_prob
    PVFB[i] <- npv(interest, value_adjusted[2:length(value_adjusted)])
  }
  return(PVFB)
}

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

sep_rate_vec <- c(0.05, 0.06, 0.03, 0.04, 0.02, 0.05, 0.06, 0.08, 0.08, 0.09)
interest_vec <- c(0.05, 0.03, 0.04, 0.05, 0.04, 0.05, 0.04, 0.03, 0.03, 0.04)
value_vec <- c(100, 120, 130, 140, 150, 120, 150, 160, 200, 220)


get_pvfb_reason(sep_rate_vec, interest_vec, value_vec)
get_pvfb(sep_rate_vec, interest_vec, value_vec)

microbenchmark(get_pvfb_reason(sep_rate_vec, interest_vec, value_vec), get_pvfb(sep_rate_vec, interest_vec, value_vec), times = 1000)


annfactor_reason <- function(surv_DR_vec, cola_vec, one_time_cola = F){
  annfactor_vec <- double(length(surv_DR_vec))
  for (i in 1:length(annfactor_vec)) {
    cola <- ifelse(one_time_cola == F, cola_vec[i], 0)

    if (i == length(annfactor_vec)) {
      cola_project <- 0
    } else {
      cola_project <- c(0, rep(cola, length((i+1):length(cola_vec))))
    }

    cumprod_cola <- cumprod(1 + cola_project)
    annfactor_vec[i] <- sum((surv_DR_vec[i:length(surv_DR_vec)] / surv_DR_vec[i]) * cumprod_cola)
  }
  return(annfactor_vec)
}

annfactor <- function(surv_DR_vec, cola_vec, one_time_cola = FALSE) {
  N <- length(surv_DR_vec)
  annfactor_vec <- numeric(N)

  for (i in 1:N) {
    cola <- ifelse(one_time_cola, 0, cola_vec[i])
    cola_project <- c(0, rep(cola, max(0, N - i)))

    cumprod_cola <- cumprod(1 + cola_project)
    surv_ratio <- surv_DR_vec[i:N] / surv_DR_vec[i]

    annfactor_vec[i] <- sum(surv_ratio * cumprod_cola)
  }

  return(annfactor_vec)
}

# annfactor_2_map <- function(surv_DR_vec, cola_vec, one_time_cola = FALSE) {
#   N <- length(surv_DR_vec)
#
#   map_dbl(seq_len(N), function(i) {
#     cola <- if (one_time_cola) 0 else cola_vec[i]
#     cola_project <- c(0, rep(cola, max(0, N - i)))
#
#     cumprod_cola <- cumprod(1 + cola_project)
#     surv_ratio <- surv_DR_vec[i:N] / surv_DR_vec[i]
#
#     sum(surv_ratio * cumprod_cola)
#   })
# }

surv_DR_vec <- c(0.95, 0.90, 0.85, 0.80, 0.95)
cola_vec <- c(0.02, 0.02, 0.02, 0.02, 0.02)

annfactor(surv_DR_vec, cola_vec, one_time_cola = FALSE)
annfactor_2(surv_DR_vec, cola_vec, one_time_cola = FALSE)
annfactor(surv_DR_vec, cola_vec, one_time_cola = TRUE)
annfactor_2(surv_DR_vec, cola_vec, one_time_cola = TRUE)

# cola_vec_2 <- c(0.03, 0.03, 0.03, 0.03, 0.03)
# annfactor(surv_DR_vec, cola_vec_2, one_time_cola = TRUE)
# annfactor_2(surv_DR_vec, cola_vec_2, one_time_cola = TRUE)

microbenchmark (annfactor(surv_DR_vec, cola_vec, one_time_cola = FALSE),
                annfactor_2(surv_DR_vec, cola_vec, one_time_cola = FALSE),
                times = 1000)

