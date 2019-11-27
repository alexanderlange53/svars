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


test_that("id.dc 3-dims PIT = FALSE works with constant", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(Canada, p = 2, type = "const")
  x1 <- id.dc(v1)

  expect_equal(sum(round(x1$B, 4)), 1.6337)

  expect_equal(x1$K, 4)
  expect_equal(x1$n, 82)
  expect_equal(x1$PIT, FALSE)
  expect_equal(x1$p, 2)

  expect_match(x1$method, "Distance covariances")
})

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


test_that("restricted id.cvm 4-dims works with restricted var and constant", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(Canada, p = 2, type = "const")
  cob <- copula::indepTestSim(v1$obs, v1$K, verbose=FALSE)

  # from vars example
  restrict <- matrix(c(1, 1, 1, 1, 1, 1, 0, 0, 0,
                       1, 0, 1, 0, 0, 1, 0, 1, 1,
                       0, 0, 1, 1, 0, 1, 0, 0, 1,
                       1, 1, 1, 0, 1, 1, 0, 1, 0),
                     nrow=4, ncol=9, byrow=TRUE)
  vRes = restrict(var.2c, method = "man", resmat = restrict)
  x1 <- id.cvm(vRes, dd = cob)

  expect_equal(sum(round(x1$B, 1)), -0.7)

  expect_equal(x1$K, 4)
  expect_equal(x1$n, 82)
  expect_equal(x1$p, 2)

  #expect_lt(x1$test.stats, 0.009451468)

  expect_match(x1$method, "Cramer-von Mises distance")
})

test_that("restricted id.cvm 4-dims works with restricted var and constant + trend", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(Canada, p = 2, type = "both")
  cob <- copula::indepTestSim(v1$obs, v1$K, verbose=FALSE)

  # from vars example
  restrict <- matrix(c(1, 1, 1, 1, 1, 1, 0, 0, 0,
                       1, 0, 1, 0, 0, 1, 0, 1, 1,
                       0, 0, 1, 1, 0, 1, 0, 0, 1,
                       1, 1, 1, 0, 1, 1, 0, 1, 0),
                     nrow=4, ncol=9, byrow=TRUE)
  vRes = restrict(var.2c, method = "man", resmat = restrict)
  x1 <- id.cvm(vRes, dd = cob)

  expect_equal(sum(round(x1$B, 1)), -0.8)

  expect_equal(x1$K, 4)
  expect_equal(x1$n, 82)
  expect_equal(x1$p, 2)

  #expect_lt(x1$test.stats, 0.009451468)

  expect_match(x1$method, "Cramer-von Mises distance")
})

