context("test-wild_boot.R")

test_that("wild.boot returns valid object for id.dc", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC")
  x1 <- id.dc(v1)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbdc <- wild.boot(x1, distr = "rademacher", nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbdc, 17)
  expect_equal(bbdc$nboot, 10)

  bbdc1 <- wild.boot(x1, distr = "rademacher", nboot = 10, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbdc1, 17)
  expect_equal(bbdc1$nboot, 10)

  bbdc2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)
  expect_length(bbdc2, 17)
  expect_equal(bbdc2$nboot, 10)

  bbdc3 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbdc3, 17)
  expect_equal(bbdc3$nboot, 10)

  x1 <- id.dc(v1, PIT = FALSE)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbdc4 <- wild.boot(x1, distr = "rademacher", nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbdc3, 17)
  expect_equal(bbdc3$nboot, 10)

  bbdc4 <- wild.boot(x1, distr = "rademacher", nboot = 10, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbdc4, 17)
  expect_equal(bbdc4$nboot, 10)

  bbdc5 <- wild.boot(x1, distr = "rademacher", nboot = 10, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbdc5, 17)
  expect_equal(bbdc5$nboot, 10)

  bbdc6 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)
  expect_length(bbdc6, 17)
  expect_equal(bbdc6$nboot, 10)

  bbdc7 <- wild.boot(x1, design = "recursive", distr = "mammen", nboot = 10, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbdc7, 17)
  expect_equal(bbdc7$nboot, 10)
})

test_that("wild.boot works with parallelization", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.dc(v1)

  cores <- 2
  bb <- wild.boot(x1, distr = "rademacher", nboot = 10, n.ahead = 30, nc = cores, signrest = NULL)
  expect_length(bb, 17)
  expect_equal(bb$nboot, 10)
})

test_that("wild.boot returns valid object for id.cv", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.cv(v1, SB = 59)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbcv <- wild.boot(x1, distr = "rademacher", nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbcv, 17)
  expect_equal(bbcv$nboot, 10)

  bbcv1 <- wild.boot(x1, distr = "rademacher", nboot = 10, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbcv1, 17)
  expect_equal(bbcv1$nboot, 10)

  bbcv2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)
  expect_length(bbcv2, 17)
  expect_equal(bbcv2$nboot, 10)

  bbcv3 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbcv3, 17)
  expect_equal(bbcv3$nboot, 10)

  restmat <- matrix(NA, 3,3)
  restmat[1,c(2,3)] <- 0
  restmat[2,3] <- 0
  x1 <- id.cv(v1, SB = 59, restriction_matrix = restmat)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbcv4 <- wild.boot(x1, distr = "rademacher", nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbcv4, 17)
  expect_equal(bbcv4$nboot, 10)

  bbcv5 <- wild.boot(x1, design = "recursive", distr = "mammen", nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)
  expect_length(bbcv5, 17)
  expect_equal(bbcv5$nboot, 10)

  # With vector as input
  SB <- rep(0, v1$obs)
  SB[50:80] <- 1
  SB[100:110] <- 1

  x1 <- id.cv(v1, SB = SB)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbcv4 <- wild.boot(x1, distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)

  expect_length(bbcv4, 17)
  expect_equal(bbcv4$nboot, 10)

  bbcv5 <- wild.boot(x1, design = "recursive", distr = "gaussian", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
  expect_length(bbcv5, 17)
  expect_equal(bbcv5$nboot, 10)

})

test_that("wild.boot returns valid object for id.st", {
  skip_on_cran()
  set.seed(231)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.st(v1, c_fix = 80, gamma_fix = -1)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbst <- wild.boot(x1, distr = "rademacher", nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbst, 17)
  expect_equal(bbst$nboot, 10)

  bbst1 <- wild.boot(x1, distr = "rademacher", nboot = 10, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbst1, 17)
  expect_equal(bbst1$nboot, 10)

  bbst2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)
  expect_length(bbst2, 17)
  expect_equal(bbst2$nboot, 10)

  bbst3 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 1, signrest = NULL)
  expect_length(bbst3, 17)
  expect_equal(bbst3$nboot, 10)

  restmat <- matrix(NA, 3,3)
  restmat[1, c(2, 3)] <- 0
  restmat[2, 3] <- 0
  x1 <- id.st(v1, c_fix = 80, gamma_fix = -1, restriction_matrix = restmat)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbst4 <- wild.boot(x1, distr = "rademacher", nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)

  expect_length(bbst4, 17)
  expect_equal(bbst4$nboot, 10)

  bbst5 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 1, signrest = signrest)
  expect_length(bbst5, 17)
  expect_equal(bbst5$nboot, 10)
})

test_that("wild.boot returns valid object for id.ngml", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
  x1 <- id.ngml(v1)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbngml <- wild.boot(x1, distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)

  expect_length(bbngml, 17)
  expect_equal(bbngml$nboot, 10)

  bbngml1 <- wild.boot(x1, distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml1, 17)
  expect_equal(bbngml1$nboot, 10)

  bbngml2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
  expect_length(bbngml2, 17)
  expect_equal(bbngml2$nboot, 10)

  bbngml3 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml3, 17)
  expect_equal(bbngml3$nboot, 10)

  x1 <- id.ngml(v1, stage3 = TRUE)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbngml4 <- wild.boot(x1, distr = "rademacher", nboot = 5, n.ahead = 30, nc = 2, signrest = signrest)

  expect_length(bbngml4, 17)
  expect_equal(bbngml4$nboot, 5)

  bbngml4 <- wild.boot(x1, distr = "rademacher", nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml4, 17)
  expect_equal(bbngml4$nboot, 5)

  bbngml5 <- wild.boot(x1, distr = "rademacher", nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml5, 17)
  expect_equal(bbngml5$nboot, 5)

  bbngml6 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 5, n.ahead = 30, nc = 2, signrest = signrest)
  expect_length(bbngml6, 17)
  expect_equal(bbngml6$nboot, 5)

  bbngml7 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml7, 17)
  expect_equal(bbngml7$nboot, 5)

  restmat <- matrix(NA, 3,3)
  restmat[1, c(2, 3)] <- 0
  restmat[2, 3] <- 0

  x1 <- id.ngml(v1, stage3 = F, restriction_matrix = restmat)

  bbngml5 <- wild.boot(x1, distr = "rademacher", nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml5, 17)
  expect_equal(bbngml5$nboot, 5)

  bbngml8 <- wild.boot(x1, distr = "rademacher", nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml8, 17)
  expect_equal(bbngml8$nboot, 5)

  bbngml9 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml9, 17)
  expect_equal(bbngml9$nboot, 5)

  x1 <- id.ngml(v1, stage3 = T, restriction_matrix = restmat)

  bbngml6 <- wild.boot(x1, distr = "rademacher", nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml6, 17)
  expect_equal(bbngml6$nboot, 5)

  bbngml10 <- wild.boot(x1, distr = "rademacher", nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml10, 17)
  expect_equal(bbngml10$nboot, 5)

  bbngml11 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 5, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbngml11, 17)
  expect_equal(bbngml11$nboot, 5)
})

test_that("wild.boot returns valid object for id.garch", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 3)
  x1 <- id.garch(v1)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbgarch <- wild.boot(x1, distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)

  expect_length(bbgarch, 17)
  expect_equal(bbgarch$nboot, 10)

  bbgarch1 <- wild.boot(x1, distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbgarch1, 17)
  expect_equal(bbgarch1$nboot, 10)

  bbgarch2 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)
  expect_length(bbgarch2, 17)
  expect_equal(bbgarch2$nboot, 10)

  bbgarch3 <- wild.boot(x1, design = "recursive", distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = NULL)
  expect_length(bbgarch3, 17)
  expect_equal(bbgarch3$nboot, 10)

  restmat <- matrix(NA, 3,3)
  restmat[1, c(2, 3)] <- 0
  restmat[2, 3] <- 0
  x1 <- id.garch(v1, restriction_matrix = restmat)

  signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
  bbgarch4 <- wild.boot(x1, distr = "rademacher", nboot = 10, n.ahead = 30, nc = 2, signrest = signrest)

  expect_length(bbgarch4, 17)
  expect_equal(bbgarch4$nboot, 10)
})

test_that("wild.boot returns valid object with different deterministic terms", {
  skip_on_cran()
  set.seed(23211)

  # With constant + trend ---------
  # DC
  v1 <- vars::VAR(USA[, -3], p = 2, type = 'both')
  x1 <- id.dc(v1)

  bbdc <- wild.boot(x1, design = 'fixed', distr = "mammen", nboot = 10,
                    n.ahead = 30, nc = 2, signrest = NULL)

  expect_length(bbdc, 17)
  expect_equal(bbdc$nboot, 10)

  bbdc2 <- wild.boot(x1, design = 'recursive', distr = "mammen", nboot = 10,
                    n.ahead = 30, nc = 2, signrest = NULL)

  expect_length(bbdc2, 17)
  expect_equal(bbdc2$nboot, 10)

  # Without constant or trend ---------
  # DC
  v1 <- vars::VAR(USA[, -3], p = 2, type = 'none')
  x1 <- id.dc(v1)

  bbdc3 <- wild.boot(x1, design = 'fixed', distr = "mammen", nboot = 10,
                    n.ahead = 30, nc = 2, signrest = NULL)

  expect_length(bbdc3, 17)
  expect_equal(bbdc3$nboot, 10)

  bbdc4 <- wild.boot(x1, design = 'recursive', distr = "mammen", nboot = 10,
                     n.ahead = 30, nc = 2, signrest = NULL)

  expect_length(bbdc4, 17)
  expect_equal(bbdc4$nboot, 10)

  # CV
  # With constant + trend ---------
  v1 <- vars::VAR(USA[, -3], p = 2, type = 'both')
  x1 <- id.cv(v1, SB = 70)

  bbcv <- wild.boot(x1, design = 'fixed', distr = "mammen", nboot = 10,
                     n.ahead = 30, nc = 2, signrest = NULL)

  expect_length(bbcv, 17)
  expect_equal(bbcv$nboot, 10)

  bbcv2 <- wild.boot(x1, design = 'recursive', distr = "mammen", nboot = 10,
                     n.ahead = 30, nc = 2, signrest = NULL)

  expect_length(bbcv2, 17)
  expect_equal(bbcv2$nboot, 10)

  # CV
  # Without constant + trend ---------
  v1 <- vars::VAR(USA[, -3], p = 2, type = 'none')
  x1 <- id.cv(v1, SB = 70)

  bbcv3 <- wild.boot(x1, design = 'fixed', distr = "mammen", nboot = 10,
                    n.ahead = 30, nc = 2, signrest = NULL)

  expect_length(bbcv3, 17)
  expect_equal(bbcv3$nboot, 10)

  bbcv4 <- wild.boot(x1, design = 'recursive', distr = "mammen", nboot = 10,
                     n.ahead = 30, nc = 2, signrest = NULL)

  expect_length(bbcv4, 17)
  expect_equal(bbcv4$nboot, 10)
})
