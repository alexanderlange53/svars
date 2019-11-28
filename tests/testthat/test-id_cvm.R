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


test_that("restricted id.cvm 3-dims works with restricted var and constant", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 3, type = "const")
  # Form vars example
  restrict <- matrix(c(1, 1, 1, 1, 1, 1, 0, 0, 0,1,
                       1, 0, 1, 0, 0, 1, 0, 1, 1,1,
                       0, 0, 1, 1, 0, 1, 0, 0, 1,1),
                     nrow=3, ncol=10, byrow=TRUE)
  vRes = restrict(v1, method = "man", resmat = restrict)
  cob <- copula::indepTestSim(v1$obs, v1$K, verbose=FALSE)
  x1 <- id.cvm(vRes, dd = cob)

  expect_equal(sum(round(x1$B, 1)), -3)

  expect_equal(x1$K, 3)
  expect_equal(x1$n, 172)
  expect_equal(x1$p, 3)

  #expect_lt(x1$test.stats, 0.009451468)

  expect_match(x1$method, "Cramer-von Mises distance")
})

test_that("restricted id.cvm 3-dims works with restricted var and constant + trend", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 3, type = "both")
  # Form vars example
  restrict <- matrix(c(1, 1, 1, 1, 1, 1, 0, 0, 0,1,1,
                       1, 0, 1, 0, 0, 1, 0, 1, 1,1,1,
                       0, 0, 1, 1, 0, 1, 0, 0, 1,1,1),
                     nrow=3, ncol=11, byrow=TRUE)
  vRes = restrict(v1, method = "man", resmat = restrict)

  cob <- copula::indepTestSim(v1$obs, v1$K, verbose=FALSE)
  x1 <- id.cvm(vRes, dd = cob)

  expect_equal(sum(round(x1$B, 1)), 2.7)

  expect_equal(x1$K, 3)
  expect_equal(x1$n, 172)
  expect_equal(x1$p, 3)

  #expect_lt(x1$test.stats, 0.009451468)

  expect_match(x1$method, "Cramer-von Mises distance")
})

