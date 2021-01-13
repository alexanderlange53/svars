test_that("cf works with 3-dims", {
  v1 <- vars::VAR(USA, p = 3, ic = "AIC" )
  x1 <- id.dc(v1)
  x2 <- cf(x1, series = 1)

  expect_equal(round(sum(x2$counter[,1]), 2),  -3.51, tolerance = 0.3)
})

test_that("cf works with 2-dims", {
  v1 <- vars::VAR(USA[,-3], p = 3, ic = "AIC" )
  x1 <- id.dc(v1)
  x2 <- cf(x1, series = 1)

  expect_equal(round(sum(x2$counter[,1]), 2), -0.86, tolerance = 0.3)
})
