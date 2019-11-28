context("test-id_chol.R")

test_that("id.chol 3-dims works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.chol(v1)

  expect_equal(sum(round(x1$B, 4)), 2.8932)

  expect_equal(x1$K, 3)
  expect_equal(x1$n, 169)
  expect_equal(x1$p, 6)

  expect_match(x1$method, "Cholesky")
})

