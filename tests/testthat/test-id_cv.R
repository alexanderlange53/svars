context("test-id_cv.R")

test_that("unrestricted id.cv estimation with 3-dim works", {
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6, ic = "AIC" )
  x1 <- id.cv(v1, SB = 59)

  expect_equal(round(x1$Lik, 4), -98.3976)
  expect_equal(round(sum(diag(x1$Lambda)),4), 1.8272)
  expect_equal(round(sum(x1$B),4), 3.2471)

  expect_equal(round(sum(x1$Lambda_SE), 4), 0.4312)

  expect_equal(x1$K, 3)
  expect_equal(x1$n, 169)
  expect_equal(x1$restrictions, 0)
  expect_equal(x1$SB, 59)
  expect_equal(x1$p, 6)
  expect_equal(x1$iteration, 6)

  expect_match(x1$method, "Changes in Volatility")
})

test_that("unrestricted id.cv estimation with 2-dim works", {
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, ic = "AIC" )
  x1 <- id.cv(v1, SB = 59)

  expect_equal(round(x1$Lik, 4), -105.4419)
  expect_equal(round(sum(diag(x1$Lambda)),4), 0.7017)
  expect_equal(round(sum(x1$B),4), 2.4101)

  expect_equal(round(sum(x1$Lambda_SE), 4), 0.1625)

  expect_equal(x1$K, 2)
  expect_equal(x1$n, 172)
  expect_equal(x1$restrictions, 0)
  expect_equal(x1$SB, 59)
  expect_equal(x1$p, 3)
  expect_equal(x1$iteration, 4)

  expect_match(x1$method, "Changes in Volatility")
})
