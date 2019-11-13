test_that("To see if average precision works", {
  expect_equal(averagePrecision(2, c(1,0), c(0.5, 0.3)), 1)
  expect_equal(averagePrecision(2, c(0,1), c(0.5, 0.3)), 0.5)
})
