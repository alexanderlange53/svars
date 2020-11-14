test_that("irf works with 3-dims and p = 3", {
  v1 <- vars::VAR(USA, p = 3, ic = "AIC" )
  x1 <- id.dc(v1)
  x2 <- irf(x1, n.ahead = 20)

  B_irf <- matrix(unlist(x2$irf[1,-1]), 3, 3, byrow = T)

  expect_equivalent(x1$B,  B_irf)
  expect_equal(dim(x2$irf), c(20, 10))
  expect_equal(round(mean(unlist(x2$irf)), 2), 1.2)
})

test_that("irf works with 3-dims and p = 1", {
  v1 <- vars::VAR(USA, p = 1, ic = "AIC" )
  x1 <- id.dc(v1)
  x2 <- irf(x1, n.ahead = 20)

  B_irf <- matrix(unlist(x2$irf[1,-1]), 3, 3, byrow = T)

  expect_equivalent(x1$B,  B_irf)
  expect_equal(dim(x2$irf), c(20, 10))
  expect_equal(round(mean(unlist(x2$irf)), 3), 1.209)
})

test_that("irf works with 2-dims and p = 3", {
  v1 <- vars::VAR(USA[,1:2], p = 3, ic = "AIC" )
  x1 <- id.dc(v1)
  x2 <- irf(x1, n.ahead = 20)

  B_irf <- matrix(unlist(x2$irf[1,-1]), 2, 2, byrow = T)

  expect_equivalent(x1$B,  B_irf)
  expect_equal(dim(x2$irf), c(20, 5))
  expect_equal(round(mean(unlist(x2$irf)), 5), 2.22229)
})
