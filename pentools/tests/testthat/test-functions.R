# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

test_that("pv works as expected", {
  expect_equal(pv(.1, g=0, nper=1, pmt=100, t=1), 90.909090)
  expect_equal(pv(.1, g=0, nper=1, pmt=100, t=2), 82.644628)
  expect_equal(pv(.1, g=0, nper=2, pmt=100, t=2), 157.776108)
  expect_equal(pv(.1, g=.05, nper=2, pmt=100, t=2), 161.53268)
})
