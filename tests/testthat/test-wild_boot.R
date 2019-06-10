context("test-wild_boot.R")

test_that("wild.boot returns valid object for id.dc", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.dc(v1)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbdc <- wild.boot(x1, rademacher = TRUE, nboot = 50, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbdc, 15)
  expect_equal(bbdc$nboot, 50)

  bbdc1 <- wild.boot(x1, rademacher = TRUE, nboot = 50, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbdc1, 15)
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

  expect_length(bbdc3, 15)
  expect_equal(bbdc3$nboot, 50)

  bbdc4 <- wild.boot(x1, rademacher = TRUE, nboot = 50, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbdc4, 15)
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
  expect_length(bb, 15)
  expect_equal(bb$nboot, 50)
})

test_that("wild.boot returns valid object for id.cv", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.cv(v1, SB = 59)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbcv <- wild.boot(x1, rademacher = TRUE, nboot = 50, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbcv, 15)
  expect_equal(bbcv$nboot, 50)

  bbcv1 <- wild.boot(x1, rademacher = TRUE, nboot = 50, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbcv1, 15)
  expect_equal(bbcv1$nboot, 50)

  bbcv2 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 50, n.ahead = 30, nc = 1, signrest = signrest)
  expect_length(bbcv2, 12)
  expect_equal(bbcv2$nboot, 50)

  bbcv3 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 50, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbcv3, 12)
  expect_equal(bbcv3$nboot, 50)

  restmat <- matrix(NA, 3,3)
  restmat[1,c(2,3)] <- 0
  restmat[2,3] <- 0
  x1 <- id.cv(v1, SB = 59, restriction_matrix = restmat)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbcv4 <- wild.boot(x1, rademacher = TRUE, nboot = 50, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbcv2, 15)
  expect_equal(bbcv2$nboot, 50)
  expect_length(bbcv4, 12)
  expect_equal(bbcv4$nboot, 50)

  bbcv5 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 50, n.ahead = 30, nc = 1, signrest = signrest)
  expect_length(bbcv5, 12)
  expect_equal(bbcv5$nboot, 50)

})

test_that("wild.boot returns valid object for id.st", {
  skip_on_cran()
  set.seed(231)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.st(v1, c_fix = 80, gamma_fix = -1)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbst <- wild.boot(x1, rademacher = TRUE, nboot = 20, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbst, 15)
  expect_equal(bbst$nboot, 20)

  bbst1 <- wild.boot(x1, rademacher = TRUE, nboot = 20, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbst1, 12)
  expect_equal(bbst1$nboot, 20)

  bbst2 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 20, n.ahead = 30, nc = 1, signrest = signrest)
  expect_length(bbst2, 12)
  expect_equal(bbst2$nboot, 20)

  bbst3 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 20, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbst3, 12)
  expect_equal(bbst3$nboot, 20)

  restmat <- matrix(NA, 3,3)
  restmat[1, c(2, 3)] <- 0
  restmat[2, 3] <- 0
  x1 <- id.st(v1, c_fix = 80, gamma_fix = -1, restriction_matrix = restmat)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbst4 <- wild.boot(x1, rademacher = TRUE, nboot = 20, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbst2, 15)
  expect_equal(bbst2$nboot, 20)
  expect_length(bbst4, 12)
  expect_equal(bbst4$nboot, 20)

  bbst5 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 20, n.ahead = 30, nc = 1, signrest = signrest)
  expect_length(bbst5, 12)
  expect_equal(bbst5$nboot, 20)
})

test_that("wild.boot returns valid object for id.ngml", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.ngml(v1)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbngml <- wild.boot(x1, rademacher = TRUE, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)

  expect_length(bbngml, 15)
  expect_equal(bbngml$nboot, 10)

  bbngml1 <- wild.boot(x1, rademacher = TRUE, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml1, 15)
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

  expect_length(bbngml3, 15)
  expect_equal(bbngml3$nboot, 5)

  bbngml4 <- wild.boot(x1, rademacher = TRUE, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml4, 15)
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
  restmat[1, c(2, 3)] <- 0
  restmat[2, 3] <- 0

  x1 <- id.ngml(v1, stage3 = F, restriction_matrix = restmat)

  bbngml5 <- wild.boot(x1, rademacher = TRUE, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml5, 15)
  expect_equal(bbngml5$nboot, 5)
  bbngml8 <- wild.boot(x1, rademacher = TRUE, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml8, 12)
  expect_equal(bbngml8$nboot, 5)

  bbngml9 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml9, 12)
  expect_equal(bbngml9$nboot, 5)

  x1 <- id.ngml(v1, stage3 = T, restriction_matrix = restmat)

  bbngml6 <- wild.boot(x1, rademacher = TRUE, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml6, 15)
  expect_equal(bbngml6$nboot, 5)
  bbngml10 <- wild.boot(x1, rademacher = TRUE, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml10, 12)
  expect_equal(bbngml10$nboot, 5)

  bbngml11 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml11, 12)
  expect_equal(bbngml11$nboot, 5)
})

test_that("wild.boot returns valid object for id.garch", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.garch(v1)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbgarch <- wild.boot(x1, rademacher = TRUE, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)

  expect_length(bbgarch, 15)
  expect_equal(bbgarch$nboot, 10)

  bbgarch1 <- wild.boot(x1, rademacher = TRUE, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbgarch1, 15)
  expect_equal(bbgarch1$nboot, 10)

  bbgarch2 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
  expect_length(bbgarch2, 12)
  expect_equal(bbgarch2$nboot, 10)

  bbgarch3 <- wild.boot(x1, recursive = TRUE, rademacher = TRUE, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbgarch3, 12)
  expect_equal(bbgarch3$nboot, 10)

  restmat <- matrix(NA, 3,3)
  restmat[1, c(2, 3)] <- 0
  restmat[2, 3] <- 0
  x1 <- id.garch(v1, restriction_matrix = restmat)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbgarch4 <- wild.boot(x1, rademacher = TRUE, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)

  expect_length(bbgarch2, 15)
  expect_equal(bbgarch2$nboot, 10)
  expect_length(bbgarch4, 12)
  expect_equal(bbgarch4$nboot, 10)
})
