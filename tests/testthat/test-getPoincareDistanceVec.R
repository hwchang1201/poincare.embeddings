test_that("to see if poincare distance function works", {
  expect_equal(getPoincareDistanceVec(c(0, 0), c(0.1, 0.5)), 1.125126)
  expect_equal(getPoincareDistanceVec(c(0.4, 0), c(0.1, 0.5)), 1.369482)
})
