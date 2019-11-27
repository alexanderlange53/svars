context("test-id_dc.R")

test_that("id.dc 3-dims PIT = FALSE works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.dc(v1)

  expect_equal(sum(round(x1$B, 4)), 2.8498)

  expect_equal(x1$K, 3)
  expect_equal(x1$n, 169)
  expect_equal(x1$PIT, FALSE)
  expect_equal(x1$p, 6)

  expect_match(x1$method, "Distance covariances")
})

test_that("id.dc 3-dims PIT = TRUE works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.dc(v1, PIT = TRUE)

  expect_equal(sum(round(x1$B, 4)), 2.5394)

  expect_equal(x1$K, 3)
  expect_equal(x1$n, 169)
  expect_equal(x1$PIT, TRUE)
  expect_equal(x1$p, 6)

  expect_match(x1$method, "Distance covariances")
})

test_that("id.dc 3-dims PIT = TRUE works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.dc(v1, PIT = TRUE)

  expect_equal(sum(round(x1$B, 4)), 2.5394)

  expect_equal(x1$K, 3)
  expect_equal(x1$n, 169)
  expect_equal(x1$PIT, TRUE)
  expect_equal(x1$p, 6)

  expect_match(x1$method, "Distance covariances")
})

test_that("id.dc 2-dims PIT = FALSE works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, ic = "AIC" )
  x1 <- id.dc(v1)

  expect_equal(sum(round(x1$B, 4)), 1.8296)

  expect_equal(x1$K, 2)
  expect_equal(x1$n, 172)
  expect_equal(x1$PIT, FALSE)
  expect_equal(x1$p, 3)

  expect_match(x1$method, "Distance covariances")
})

test_that("id.dc 2-dims PIT = FALSE works with trend + constant", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, type = 'both' )
  x1 <- id.dc(v1)

  expect_equal(sum(round(x1$B, 4)), 1.8102)

  expect_equal(x1$K, 2)
  expect_equal(x1$n, 172)
  expect_equal(x1$PIT, FALSE)
  expect_equal(x1$p, 3)

  expect_match(x1$method, "Distance covariances")
})

test_that("id.dc 2-dims PIT = FALSE works without deterministic term", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, type = 'none' )
  x1 <- id.dc(v1)

  expect_equal(sum(round(x1$B, 4)), 1.8441)

  expect_equal(x1$K, 2)
  expect_equal(x1$n, 172)
  expect_equal(x1$PIT, FALSE)
  expect_equal(x1$p, 3)

  expect_match(x1$method, "Distance covariances")
})

test_that("id.dc 2-dims PIT = TRUE works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, ic = "AIC" )
  x1 <- id.dc(v1, PIT = TRUE)

  expect_equal(sum(round(x1$B, 4)), 1.8734)

  expect_equal(x1$K, 2)
  expect_equal(x1$n, 172)
  expect_equal(x1$PIT, TRUE)
  expect_equal(x1$p, 3)

  expect_match(x1$method, "Distance covariances")
})

test_that("restricted id.dc 4-dims PIT = FALSE works with restricted var and constant", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(Canada, p = 2, type = "const")
  # Form vars example
  restrict <- matrix(c(1, 1, 1, 1, 1, 1, 0, 0, 0,
                       1, 0, 1, 0, 0, 1, 0, 1, 1,
                       0, 0, 1, 1, 0, 1, 0, 0, 1,
                       1, 1, 1, 0, 1, 1, 0, 1, 0),
                     nrow=4, ncol=9, byrow=TRUE)
  vRes = restrict(var.2c, method = "man", resmat = restrict)

  x1 <- id.dc(vRes)

  expect_equal(sum(round(x1$B, 4)), 1.6517)

  expect_equal(x1$K, 4)
  expect_equal(x1$n, 82)
  expect_equal(x1$PIT, FALSE)
  expect_equal(x1$p, 2)

  expect_match(x1$method, "Distance covariances")
})

test_that("id.dc 4-dims PIT = FALSE works with restricted var and trend", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(Canada, p = 2, type = "trend")
  # Form vars example
  restrict <- matrix(c(1, 1, 1, 1, 1, 1, 0, 0, 0,
                       1, 0, 1, 0, 0, 1, 0, 1, 1,
                       0, 0, 1, 1, 0, 1, 0, 0, 1,
                       1, 1, 1, 0, 1, 1, 0, 1, 0),
                     nrow=4, ncol=9, byrow=TRUE)
  vRes = restrict(var.2c, method = "man", resmat = restrict)

  x1 <- id.dc(vRes)

  expect_equal(sum(round(x1$B, 4)), 1.6517)

  expect_equal(x1$K, 4)
  expect_equal(x1$n, 82)
  expect_equal(x1$PIT, FALSE)
  expect_equal(x1$p, 2)

  expect_match(x1$method, "Distance covariances")
})


test_that("id.dc 4-dims PIT = FALSE works with restricted var and constant + trend", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(Canada, p = 2, type = "both")
  # Form vars example
  restrict <- matrix(c(1, 1, 1, 1, 1, 1, 0, 0, 0,
                       1, 0, 1, 0, 0, 1, 0, 1, 1,
                       0, 0, 1, 1, 0, 1, 0, 0, 1,
                       1, 1, 1, 0, 1, 1, 0, 1, 0),
                     nrow=4, ncol=9, byrow=TRUE)
  vRes = restrict(var.2c, method = "man", resmat = restrict)

  x1 <- id.dc(vRes)

  expect_equal(sum(round(x1$B, 4)), 1.6517)

  expect_equal(x1$K, 4)
  expect_equal(x1$n, 82)
  expect_equal(x1$PIT, FALSE)
  expect_equal(x1$p, 2)

  expect_match(x1$method, "Distance covariances")
})

