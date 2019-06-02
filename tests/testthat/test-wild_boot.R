context("test-wild_boot.R")

test_that("wild.boot returns valid object for id.dc", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.dc(v1)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbdc <- wild.boot(x1, rademacher = TRUE, nboot = 50, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbdc, 12)
  expect_equal(bbdc$nboot, 50)

  bbdc1 <- wild.boot(x1, rademacher = TRUE, nboot = 50, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbdc1, 12)
  expect_equal(bbdc1$nboot, 50)

  bbdc2 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 50, n.ahead = 30, nc = 1, signrest = signrest)
  expect_length(bbdc2, 12)
  expect_equal(bbdc2$nboot, 50)

  bbdc3 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 50, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbdc3, 12)
  expect_equal(bbdc3$nboot, 50)

  x1 <- id.dc(v1, PIT = FALSE)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbdc4 <- wild.boot(x1, rademacher = TRUE, nboot = 50, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbdc4, 12)
  expect_equal(bbdc4$nboot, 50)

  bbdc5 <- wild.boot(x1, rademacher = TRUE, nboot = 50, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbdc5, 12)
  expect_equal(bbdc5$nboot, 50)

  bbdc6 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 50, n.ahead = 30, nc = 1, signrest = signrest)
  expect_length(bbdc6, 12)
  expect_equal(bbdc6$nboot, 50)

  bbdc7 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 50, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbdc7, 12)
  expect_equal(bbdc7$nboot, 50)
})

test_that("wild.boot works with parallelization", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.dc(v1)

  cores <- 2
  bb <- wild.boot(x1, rademacher = TRUE, nboot = 50, n.ahead = 30, nc = cores, signrest = NULL)
  expect_length(bb, 12)
  expect_equal(bb$nboot, 50)
})

test_that("wild.boot returns valid object for id.cv", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.cv(v1, SB = 59)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbcv <- wild.boot(x1, rademacher = TRUE, nboot = 20, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbcv, 12)
  expect_equal(bbcv$nboot, 20)

  bbcv1 <- wild.boot(x1, rademacher = TRUE, nboot = 20, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbcv1, 12)
  expect_equal(bbcv1$nboot, 20)

  bbcv2 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 20, n.ahead = 30, nc = 1, signrest = signrest)
  expect_length(bbcv2, 12)
  expect_equal(bbcv2$nboot, 20)

  bbcv3 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 20, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbcv3, 12)
  expect_equal(bbcv3$nboot, 20)

  restmat <- matrix(NA, 3,3)
  restmat[1,c(2,3)] <- 0
  restmat[2,3] <- 0
  x1 <- id.cv(v1, SB = 59, restriction_matrix = restmat)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbcv4 <- wild.boot(x1, rademacher = TRUE, nboot = 20, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbcv4, 12)
  expect_equal(bbcv4$nboot, 20)

  bbcv5 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 20, n.ahead = 30, nc = 1, signrest = signrest)
  expect_length(bbcv5, 12)
  expect_equal(bbcv5$nboot, 20)

  # bbcv6 <- wild.boot(x1, rademacher = TRUE, nboot = 50, n.ahead = 30, nc = 1, signrest = NULL)
  # expect_length(bbcv6, 12)
  # expect_equal(bbcv6$nboot, 50)

})

test_that("wild.boot returns valid object for id.ngml", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.ngml(v1)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbngml <- wild.boot(x1, rademacher = TRUE, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)

  expect_length(bbngml, 12)
  expect_equal(bbngml$nboot, 10)

  bbngml1 <- wild.boot(x1, rademacher = TRUE, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml1, 12)
  expect_equal(bbngml1$nboot, 10)

  bbngml2 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
  expect_length(bbngml2, 12)
  expect_equal(bbngml2$nboot, 10)

  bbngml3 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml3, 12)
  expect_equal(bbngml3$nboot, 10)

  x1 <- id.ngml(v1, stage3 = TRUE)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbngml4 <- wild.boot(x1, rademacher = TRUE, nboot = 5, n.ahead = 30, nc = 2, signrest = signrest)

  expect_length(bbngml4, 12)
  expect_equal(bbngml4$nboot, 5)

  bbngml5 <- wild.boot(x1, rademacher = TRUE, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml5, 12)
  expect_equal(bbngml5$nboot, 5)

  bbngml6 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 5, n.ahead = 30, nc = 2, signrest = signrest)
  expect_length(bbngml6, 12)
  expect_equal(bbngml6$nboot, 5)

  bbngml7 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml7, 12)
  expect_equal(bbngml7$nboot, 5)

  restmat <- matrix(NA, 3,3)
  restmat[1,c(2,3)] <- 0
  restmat[2,3] <- 0

  x1 <- id.ngml(v1, stage3 = F, restriction_matrix = restmat)

  bbngml8 <- wild.boot(x1, rademacher = TRUE, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml8, 12)
  expect_equal(bbngml8$nboot, 5)

  bbngml9 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml9, 12)
  expect_equal(bbngml9$nboot, 5)

  x1 <- id.ngml(v1, stage3 = T, restriction_matrix = restmat)

  bbngml10 <- wild.boot(x1, rademacher = TRUE, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml10, 12)
  expect_equal(bbngml10$nboot, 5)

  bbngml11 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml11, 12)
  expect_equal(bbngml11$nboot, 5)
})
