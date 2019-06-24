context("test-mb_boot.R")

test_that("mb.boot returns valid object for id.dc", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC")
  x1 <- id.dc(v1)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbdc <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbdc, 16)
  expect_equal(bbdc$nboot, 10)

  bbdc1 <- mb.boot(x1, b.length = 20, nboot = 10, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbdc1, 16)
  expect_equal(bbdc1$nboot, 10)

  bbdc2 <- mb.boot(x1, design = "fixed", b.length = 20, nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)
  expect_length(bbdc2, 16)
  expect_equal(bbdc2$nboot, 10)

  bbdc3 <- mb.boot(x1, design = "fixed", b.length = 20, nboot = 10, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbdc3, 16)
  expect_equal(bbdc3$nboot, 10)

  x1 <- id.dc(v1, PIT = FALSE)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbdc4 <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbdc3, 16)
  expect_equal(bbdc3$nboot, 10)

  bbdc4 <- mb.boot(x1, b.length = 20, nboot = 10, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbdc4, 16)
  expect_equal(bbdc4$nboot, 10)

  bbdc5 <- mb.boot(x1, b.length = 20, nboot = 10, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbdc5, 16)
  expect_equal(bbdc5$nboot, 10)

  bbdc6 <- mb.boot(x1, design = "fixed", b.length = 20, nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)
  expect_length(bbdc6, 16)
  expect_equal(bbdc6$nboot, 10)

  bbdc7 <- mb.boot(x1, design = "fixed", b.length = 20, nboot = 10, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbdc7, 16)
  expect_equal(bbdc7$nboot, 10)
})

test_that("mb.boot works with parallelization", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.dc(v1)

  cores <- 2
  bb <- mb.boot(x1, b.length = 20, nboot = 10, n.ahead = 30, nc = cores, signrest = NULL)

  expect_length(bb, 16)
  expect_equal(bb$nboot, 10)
})

test_that("mb.boot returns valid object for id.cv", {
  skip_on_cran()
  set.seed(231)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.cv(v1, SB = 59)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbcv <- mb.boot(x1, b.length = 20, nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbcv, 16)
  expect_equal(bbcv$nboot, 10)

  bbcv1 <- mb.boot(x1, b.length = 10, nboot = 10, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbcv1, 16)
  expect_equal(bbcv1$nboot, 10)

  bbcv2 <- mb.boot(x1, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)
  expect_length(bbcv2, 16)
  expect_equal(bbcv2$nboot, 10)

  bbcv3 <- mb.boot(x1, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbcv3, 16)
  expect_equal(bbcv3$nboot, 10)

  restmat <- matrix(NA, 3,3)
  restmat[1,c(2,3)] <- 0
  restmat[2,3] <- 0
  x1 <- id.cv(v1, SB = 59, restriction_matrix = restmat)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbcv4 <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbcv4, 16)
  expect_equal(bbcv4$nboot, 10)

  bbcv5 <- mb.boot(x1, design ="fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)
  expect_length(bbcv5, 16)
  expect_equal(bbcv5$nboot, 10)

  # With vector as input
  SB <- rep(0, v1$obs)
  SB[50:80] <- 1
  SB[100:110] <- 1

  x1 <- id.cv(v1, SB = SB)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbcv4 <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbcv4, 16)
  expect_equal(bbcv4$nboot, 10)

  bbcv5 <- mb.boot(x1, design = "fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)
  expect_length(bbcv5, 16)
  expect_equal(bbcv5$nboot, 10)
})

test_that("mb.boot returns valid object for id.st", {
  skip_on_cran()
  set.seed(231)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.st(v1, c_fix = 80, gamma_fix = -1)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbst <- mb.boot(x1, b.length = 20, nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbst, 16)
  expect_equal(bbst$nboot, 10)

  bbst1 <- mb.boot(x1, b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbst1, 16)
  expect_equal(bbst1$nboot, 10)

  bbst2 <- mb.boot(x1, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
  expect_length(bbst2, 16)
  expect_equal(bbst2$nboot, 10)

  bbst3 <- mb.boot(x1, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbst3, 16)
  expect_equal(bbst3$nboot, 10)

  restmat <- matrix(NA, 3,3)
  restmat[1,c(2,3)] <- 0
  restmat[2,3] <- 0
  x1 <- id.st(v1, c_fix = 80, gamma_fix = -1, restriction_matrix = restmat)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbst4 <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)

  expect_length(bbst4, 16)
  expect_equal(bbst4$nboot, 10)

  bbst5 <- mb.boot(x1, design = "fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
  expect_length(bbst5, 16)
  expect_equal(bbst5$nboot, 10)
})

test_that("mb.boot returns valid object for id.ngml", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.ngml(v1)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbngml <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbngml, 16)
  expect_equal(bbngml$nboot, 10)

  bbngml1 <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbngml1, 16)
  expect_equal(bbngml1$nboot, 10)

  bbngml2 <- mb.boot(x1, design = "fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)
  expect_length(bbngml2, 16)
  expect_equal(bbngml2$nboot, 10)

  bbngml3 <- mb.boot(x1, design = "fixed", b.length = 16, nboot = 10, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbngml3, 16)
  expect_equal(bbngml3$nboot, 10)

  x1 <- id.ngml(v1, stage3 = TRUE)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbngml4 <- mb.boot(x1, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = signrest)

  expect_length(bbngml4, 16)
  expect_equal(bbngml4$nboot, 5)

  bbngml4 <- mb.boot(x1, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml4, 16)
  expect_equal(bbngml4$nboot, 5)

  bbngml5 <- mb.boot(x1, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml5, 16)
  expect_equal(bbngml5$nboot, 5)

  bbngml6 <- mb.boot(x1, design = "fixed", b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = signrest)
  expect_length(bbngml6, 16)
  expect_equal(bbngml6$nboot, 5)

  bbngml7 <- mb.boot(x1, design = "fixed", b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml7, 16)
  expect_equal(bbngml7$nboot, 5)

  restmat <- matrix(NA, 3,3)
  restmat[1,c(2,3)] <- 0
  restmat[2,3] <- 0

  x1 <- id.ngml(v1, stage3 = F, restriction_matrix = restmat)

  bbngml5 <- mb.boot(x1, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml5, 16)
  expect_equal(bbngml5$nboot, 5)

  bbngml8 <- mb.boot(x1, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml8, 16)
  expect_equal(bbngml8$nboot, 5)

  bbngml9 <- mb.boot(x1, design = "fixed", b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml9, 16)
  expect_equal(bbngml9$nboot, 5)

  restmat <- matrix(NA, 3,3)
  restmat[1,c(2,3)] <- 0
  restmat[2,3] <- 0

  x1 <- id.ngml(v1, stage3 = T, restriction_matrix = restmat)

  bbngml6 <- mb.boot(x1, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml6, 16)
  expect_equal(bbngml6$nboot, 5)

  bbngml10 <- mb.boot(x1, b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml10, 16)
  expect_equal(bbngml10$nboot, 5)

  bbngml11 <- mb.boot(x1, design = "fixed", b.length = 16, nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml11, 16)
  expect_equal(bbngml11$nboot, 5)
})

test_that("mb.boot returns valid object for id.garch", {
  skip_on_cran()
  set.seed(231)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.garch(v1)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbgarch <- mb.boot(x1, b.length = 15, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)

  expect_length(bbgarch, 16)
  expect_equal(bbgarch$nboot, 10)

  bbgarch1 <- mb.boot(x1, b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbgarch1, 16)
  expect_equal(bbgarch1$nboot, 10)

  bbgarch2 <- mb.boot(x1, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
  expect_length(bbgarch2, 16)
  expect_equal(bbgarch2$nboot, 10)

  bbgarch3 <- mb.boot(x1, design = "fixed", b.length = 10, nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbgarch3, 16)
  expect_equal(bbgarch3$nboot, 10)

  restmat <- matrix(NA, 3,3)
  restmat[1,c(2,3)] <- 0
  restmat[2,3] <- 0
  x1 <- id.garch(v1, restriction_matrix = restmat)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbgarch4 <- mb.boot(x1, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)

  expect_length(bbgarch2, 16)
  expect_equal(bbgarch2$nboot, 10)
  expect_length(bbgarch4, 16)
  expect_equal(bbgarch4$nboot, 10)

  bbgarch5 <- mb.boot(x1, design = fixeed, b.length = 16, nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
  expect_length(bbgarch5, 16)
  expect_equal(bbgarch5$nboot, 10)
})
