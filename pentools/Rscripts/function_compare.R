
# npv ---------------------------------------------------------------------

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

# get_pvfb ----------------------------------------------------------------

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
      PVFB[i = N] <- NA                                 # At the last period, there are no future periods, so PVFB is 0
    }
  }
  return(PVFB)
}

sep_rate_vec <- c(0.05, 0.06, 0.03, 0.04, 0.02, 0.05, 0.06, 0.08, 0.08, 0.09, 0.08)
interest_vec <- c(0.05, 0.03, 0.04, 0.05, 0.04, 0.05, 0.04, 0.03, 0.03, 0.04, 0.05)
value_vec <- c(100, 120, 130, 140, 150, 120, 150, 160, 200, 220, 300)


get_pvfb_reason(sep_rate_vec, interest_vec, value_vec)
get_pvfb(sep_rate_vec, interest_vec, value_vec)

microbenchmark(get_pvfb_reason(sep_rate_vec, interest_vec, value_vec), get_pvfb(sep_rate_vec, interest_vec, value_vec), times = 1000)


# annfactor ---------------------------------------------------------------

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

npv = function(rate, cashflows) {
  for(i in 1:length(cashflows)){
    if(i == 1){
      NPV <- cashflows[i]/((1+rate)^(i))
    } else {
      NPV <- NPV + cashflows[i]/((1+rate)^(i))
    }
  }

  return(NPV)
}


# get_pvfs ----------------------------------------------------------------

get_pvfs_reason <- function(remaining_prob_vec, interest_vec, sal_vec) {
  PVFS <- double(length = length(sal_vec))
  for (i in 1:length(sal_vec)) {
    remaining_prob_og <- remaining_prob_vec[i:length(remaining_prob_vec)]
    remaining_prob <- remaining_prob_og / remaining_prob_og[1]
    interest <- interest_vec[i]
    sal <- sal_vec[i:length(sal_vec)]
    sal_adjusted <- sal * remaining_prob
    PVFS[i] <- npv(interest, sal_adjusted)
  }
  return(PVFS)
}

get_pvfs <- function(remaining_prob_vec, interest_vec, sal_vec) {
  N <- length(sal_vec)
  PVFS <- double(length = N)
  for (i in 1:N) {
    remaining_prob_sub <- remaining_prob_vec[i:N] / remaining_prob_vec[i]
    interest <- interest_vec[i]
    sal_sub <- sal_vec[i:N]
    df_sub  <- (1 + interest)^(-(1:length(sal_sub)))  # Discount factors in each year based on the interest rate used in t
    PVFS[i] <- sum(sal_sub * remaining_prob_sub * df_sub)
  }
  return(PVFS)
}

remaining_prob_vec <- c(0.95, 0.90, 0.85, 0.80, 0.90, 0.95, 0.90, 0.85, 0.80, 0.90)
interest_vec <- c(0.05, 0.04, 0.03, 0.04, 0.05, 0.05, 0.04, 0.03, 0.04, 0.05)
sal_vec <- c(100, 200, 320, 420, 540,100, 200, 320, 420, 540)

get_pvfs_reason(remaining_prob_vec, interest_vec, sal_vec)
get_pvfs(remaining_prob_vec, interest_vec, sal_vec)


microbenchmark(get_pvfs_reason(remaining_prob_vec, interest_vec, sal_vec),
               get_pvfs(remaining_prob_vec, interest_vec, sal_vec),
               times = 1000)

# get_pmt -----------------------------------------------------------------

get_pmt0_reason <- function(r, nper, pv) {
  if (r == 0) {
    a <- pv/nper
  } else {
    a <- ifelse(nper == 0, 0, pv*r*(1+r)^(nper-1)/((1+r)^nper-1))
  }

  return(a)
}

get_pmt_due <- function(rate, t) {
  if (rate == 0) {
    pmt = 1/t
  } else {
    pmt = (rate / (1 -(1 + rate) ^ (-t))) * (1 / (1 + rate))
  }
  return(pmt)
}

get_pmt0 <- function(r, nper, pv) {
  get_pmt_due(r, nper)*pv
}

r <- 0.02
nper <- 50
pv <- 500

get_pmt0(r, nper, pv)
get_pmt0_reason(r, nper, pv)

microbenchmark(get_pmt0_reason(r, nper, pv),
               get_pmt0(r, nper, pv),
               times = 1000)


# growth ------------------------------------------------------------------

recur_grow_reason <- function(x, g) {
  if (length(x) > 1) {
    for (i in 2:length(x)) {
      x[i] <- x[i-1] * (1 + g[i - 1])
    }
  }
  return(x)
}

recur_grow <- function(x, g) {
 g_cul <- cumprod(1 + g)
 x[2:length(x)] <- x[1] * g_cul[1:(length(g) - 1)]
 return(x)
}

x1 <- c(100, 0, 0, 0)  # Initial vector
g1 <- c(0.05, 0.03, 0.04, 0.05)  # Growth rates
recur_grow(x1, g1)
recur_grow_reason(x1, g1)

microbenchmark(recur_grow(x1, g1),
               recur_grow_reason(x1, g1),
               times=1000)

recur_grow2_reason <- function(x, g) {
  if (length(x) > 1) {
    for (i in 2:length(x)) {
      x[i] <- x[i-1] * (1 + g[i])
    }
  }
  return(x)
}

recur_grow2 <- function(x, g) {
  g[1:length(g)-1] <- g[2:length(g)]
  recur_grow(x, g)
}

x1 <- c(100, 0, 0, 0)  # Initial vector
g1 <- c(0.05, 0.03, 0.04, 0.05)  # Growth rates
recur_grow2(x1, g1)
recur_grow2_reason(x1, g1)

microbenchmark(recur_grow2(x1, g1),
               recur_grow2_reason(x1, g1),
               times=1000)


recur_grow3_reason <- function(x, g, nper) {
  x_vec <- double(length = nper)
  x_vec[1] <- x

  for (i in 2:length(x_vec)) {
    x_vec[i] <- x_vec[i-1] * (1 + g)
  }

  return(x_vec)
}

recur_grow3 <- function(x, g, nper) {
  growth_factors <- cumprod(rep(1 + g, nper - 1))
  x_vec <- c(x, x * growth_factors)
  return(x_vec)
}

recur_grow3_reason(100, 0.05, 10)
recur_grow3(100, 0.05, 10)

microbenchmark(recur_grow3_reason(100, 0.05, 3),
               recur_grow3(100, 0.05, 3),
               times=1000)


# pv ----------------------------------------------------------------------

pv_reason <- function(rate, g = 0, nper, pmt, t = 1) {
  r <- (1 + rate)/(1 + g) - 1
  PV <- pmt/r * (1 - (1 / (1 + r)^nper)) / (1 + g) * (1 + rate)^(1 - t)
  return(PV)
}

pv <- function(rate, g = 0, nper, pmt, t = 1) {
  get_pv_gpmt (rate, growth = g, t = nper) * pmt * (1 + rate)^(1 - t)
}

rate <- 0.05
g <- 0.03
nper <- 5
pmt <- 100

pv_reason(rate, g, nper, pmt)
pv(rate, g, nper, pmt)
pv_reason(rate, g, nper, pmt, 0)
pv(rate, g, nper, pmt, 0)

microbenchmark(pv_reason(rate, g, nper, pmt),
               pv(rate, g, nper, pmt),
               pv_reason(rate, g, nper, pmt, 0),
               pv(rate, g, nper, pmt, 0),
               times=1000)


# roll_pv -----------------------------------------------------------------

roll_pv_reason <- function(rate, g = 0, nper, pmt_vec, t = 1) {
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


rate <-0.05
g <- 0.03
nper <- 5
pmt_vec <- c(100, 102, 103, 105, 109)
roll_pv(rate, g, nper, pmt_vec)

roll_pv_reason(rate, g, nper, pmt_vec)
