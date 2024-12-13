# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

test_that("get_pv works as expected", {
  expect_equal(get_pv(rate = 0, t = 1), 1)
  expect_equal(get_pv(rate = 0.05, t = 10),  0.613913, tolerance = 1e-6)
  expect_equal(get_pv(rate = 0.05, t = 5), 0.783526, tolerance = 1e-6)
  expect_equal(get_pv(rate = 0.02, t = 10), 0.820348, tolerance = 1e-6)
})

test_that("get_pv_pmt works as expected", {
  expect_equal(get_pv_pmt(rate = 0, t = 1), 1)
  expect_equal(get_pv_pmt(rate = 0.05, t = 10),   7.721735, tolerance = 1e-6)
  expect_equal(get_pv_pmt(rate = 0.05, t = 5),  4.329477, tolerance = 1e-6)
  expect_equal(get_pv_pmt(rate = 0.02, t = 10),   8.982585, tolerance = 1e-6)
})

test_that("get_pv_gpmt works as expected", {
  expect_equal(get_pv_gpmt(rate = 0, growth = 0, t = 1), 1)
  expect_equal(get_pv_gpmt(rate = 0.08, growth = 0.05, t = 18), 13.2582, tolerance = 1e-6)
  expect_equal(get_pv_gpmt(rate = 0.03, growth = 0.02, t = 10), 9.295367, tolerance = 1e-6)
})

test_that("npv works as expected", {
  expect_equal(npv(0.05, c(100, 200, 300, 400, 500, 600)), 1704.368582, tolerance = 1e-6)
  expect_equal(npv(0.02, c(100, 200, 300, 400, 500, 600)), 1928.156077, tolerance = 1e-6)
})

test_that("get_pv_cf_roll works as expected", {
  expect_equal(get_pv_cf_roll(0.05, c(100, 200, 300, 400, 500, 600)), c(1704.368582, 1689.587011, 1574.066361, 1352.769679, 1020.408163, 571.4285714), tolerance = 1e-6)
  expect_equal(get_pv_cf_roll(0.02, c(100, 200, 300, 400, 500, 600)), c(1928.156077, 1866.719198, 1704.053582, 1438.134654, 1066.897347, 588.2352941), tolerance = 1e-6)
})

test_that("get_pmt_due works as expected", {
  expect_equal(get_pmt_due(rate = 0, t = 1), 1, tolerance = 1e-6)
  expect_equal(get_pmt_due(rate = 0.05, t = 5), 0.219976, tolerance = 1e-5)
  expect_equal(get_pmt_due(rate = 0.02, t = 5), 0.207998, tolerance = 1e-5)
  expect_equal(get_pmt_due(rate = 0.05, t = 10), 0.123338, tolerance = 1e-5)
})

test_that("get_pmt_growth works as expected", {
  expect_equal(get_pmt_growth(rate = 0.05, growth = 0.02, t = 5), 0.2117597, tolerance = 1e-6)
})


test_that("get_pvfb works as expected", {
  sep_rate_vec <- c(0.01, 0.02, 0.03, 0.04)
  interest_vec <- c(0.05, 0.05, 0.05, 0.05)
  value_vec <- c(100, 200, 300, 400)

  pvfb_result <- get_pvfb(sep_rate_vec, interest_vec, value_vec)
  pvfb_result_reason <- c(23.86535, 21.39173, 14.18971, 0)
  expect_equal(pvfb_result, pvfb_result_reason, tolerance = 1e-5)
})

test_that("annfactor works as expected", {
  surv_DR_vec <- c(0.95, 0.90, 0.85, 0.80)
  cola_vec <- c(0.02, 0.02, 0.02, 0.02)

  annfactor_result <- annfactor(surv_DR_vec, cola_vec, one_time_cola = FALSE)
  expect_result_reason <- c(3.790849, 2.888133, 1.960000, 1)

  expect_equal(annfactor_result, expect_result_reason, tolerance = 1e-5)

  annfactor_result <- annfactor(surv_DR_vec, cola_vec, one_time_cola = TRUE)
  expect_result_reason <- c(3.684211, 2.833333, 1.941176, 1.000000)

  expect_equal(annfactor_result, expect_result_reason, tolerance = 1e-5)

})
