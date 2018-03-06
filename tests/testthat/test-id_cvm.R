context("test-id_cvm.R")

test_that("id.cvm 3-dims works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6, ic = "AIC" )
  cob <- copula::indepTestSim(v1$obs, v1$K, verbose=FALSE)
  x1 <- id.cvm(v1, dd = cob)

  expect_equal(sum(round(x1$B, 1)), 2.5)

  expect_equal(x1$K, 3)
  expect_equal(x1$n, 169)
  expect_equal(x1$p, 6)

  expect_lt(x1$test.stats, 0.014)

  expect_match(x1$method, "Cramer-von Mises distance")
})

test_that("id.cvm 3-dims works with internal test stat calculation", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6, ic = "AIC" )
  x1 <- id.cvm(v1, dd = NULL)

  expect_equal(sum(round(x1$B, 1)), 2.5)

  expect_equal(x1$K, 3)
  expect_equal(x1$n, 169)
  expect_equal(x1$p, 6)

  expect_lt(x1$test.stats, 0.014)

  expect_match(x1$method, "Cramer-von Mises distance")
})

test_that("id.cvm 2-dims works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, ic = "AIC" )
  cob <- copula::indepTestSim(v1$obs, v1$K, verbose=FALSE)
  x1 <- id.cvm(v1, dd = cob)

  expect_equal(sum(round(x1$B, 1)), 0.3)

  expect_equal(x1$K, 2)
  expect_equal(x1$n, 172)
  expect_equal(x1$p, 3)

  expect_lt(x1$test.stats, 0.014)

  expect_match(x1$method, "Cramer-von Mises distance")
})

test_that("id.cvm 2-dims works with internal test stat calculation", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, ic = "AIC" )
  x1 <- id.cvm(v1, dd = NULL)

  expect_equal(x1$K, 2)
  expect_equal(x1$n, 172)
  expect_equal(x1$p, 3)

  expect_lt(x1$test.stats, 0.014)

  expect_match(x1$method, "Cramer-von Mises distance")
})
