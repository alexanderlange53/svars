context("test-id_cv.R")

test_that("unrestricted id.cv estimation with 3-dim works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6, ic = "AIC" )
  x1 <- id.cv(v1, SB = 59)
  x2 <- id.cv_boot(v1, SB = 59, SB2 = NULL)

  expect_equal(round(x1$Lik, 4), round(x2$Lik, 4), -564.2994)
  expect_equal(round(sum(diag(x1$Lambda)),4),  round(sum(diag(x2$Lambda)),4), 1.8286)
  expect_equal(round(sum(x1$B),4), round(sum(x2$B),4), 3.2471)

  expect_equal(round(sum(x1$Lambda_SE), 4), 0.4315, tolerance = 0.002)

  expect_equal(x1$K, x2$K, 3)
  expect_equal(x1$n, x2$n, 169)
  expect_equal(x1$restrictions,x2$restrictions, 0)
  expect_equal(x1$SB,x2$SB, 59)
  expect_equal(x1$p,x2$p, 6)
  expect_equal(x1$iteration,x2$iteration, 6)

  expect_match(x1$method, x2$method, "Changes in Volatility")
})

test_that("unrestricted id.cv estimation with 3-dim works with constant +  trend", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6, ic = "AIC", type = 'both' )
  x1 <- id.cv(v1, SB = 59)
  x2 <- id.cv_boot(v1, SB = 59, SB2 = NULL)

  expect_equal(round(x1$Lik, 4), round(x2$Lik, 4), -559.0479)
  expect_equal(round(sum(diag(x1$Lambda)),4),  round(sum(diag(x2$Lambda)),4), 1.8512)
  expect_equal(round(sum(x1$B),4), round(sum(x2$B),4), 3.216)

  expect_equal(round(sum(x1$Lambda_SE), 4), 0.4368, tolerance = 0.002)

  expect_equal(x1$K, x2$K, 3)
  expect_equal(x1$n, x2$n, 169)
  expect_equal(x1$restrictions,x2$restrictions, 0)
  expect_equal(x1$SB,x2$SB, 59)
  expect_equal(x1$p,x2$p, 6)
  expect_equal(x1$iteration,x2$iteration, 6)

  expect_match(x1$method, x2$method, "Changes in Volatility")
})

test_that("unrestricted id.cv estimation with 3-dim works without any determininstic term", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6, ic = "AIC", type = 'none')
  x1 <- id.cv(v1, SB = 59)
  x2 <- id.cv_boot(v1, SB = 59, SB2 = NULL)

  expect_equal(round(x1$Lik, 4), round(x2$Lik, 4), -567.2314)
  expect_equal(round(sum(diag(x1$Lambda)),4),  round(sum(diag(x2$Lambda)),4), 1.8234)
  expect_equal(round(sum(x1$B),4), round(sum(x2$B),4), 3.3)

  expect_equal(round(sum(x1$Lambda_SE), 4), 0.4303, tolerance = 0.002)

  expect_equal(x1$K, x2$K, 3)
  expect_equal(x1$n, x2$n, 169)
  expect_equal(x1$restrictions,x2$restrictions, 0)
  expect_equal(x1$SB,x2$SB, 59)
  expect_equal(x1$p,x2$p, 6)
  expect_equal(x1$iteration,x2$iteration, 6)

  expect_match(x1$method, x2$method, "Changes in Volatility")
})

test_that("unrestricted id.cv and boot estimation with 2-dim works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, ic = "AIC" )
  x1 <- id.cv(v1, SB = 59)
  x2 <- id.cv_boot(v1, SB = 59, SB2 = NULL)
  expect_equal(round(x1$Lik, 4),round(x2$Lik, 4), -421.5567)
  expect_equal(round(sum(diag(x1$Lambda)),4),round(sum(diag(x2$Lambda)),4), 0.7016)
  expect_equal(round(sum(x1$B),4),round(sum(x2$B),4), 2.4104)

  expect_equal(round(sum(x1$Lambda_SE), 4), 0.1624, tolerance = 0.002)

  expect_equal(x1$K,x2$K, 2)
  expect_equal(x1$n,x2$n, 172)
  expect_equal(x1$restrictions,x2$restrictions, 0)
  expect_equal(x1$SB,x2$SB, 59)
  expect_equal(x1$p,x2$p, 3)
  expect_equal(x1$iteration,x2$iteration, 4)

  expect_match(x1$method, x2$method, "Changes in Volatility")
})

test_that("restricted id.cv and boot estimation with 3-dim works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6, ic = "AIC" )
  restmat <- matrix(NA,3,3)
  restmat[1,c(2,3)] <- 0
  restmat[2,3] <- 0
  x1 <- id.cv(v1, SB = 59, restriction_matrix = restmat)
  x2 <- id.cv_boot(v1, SB = 59, SB2 = NULL, restriction_matrix = restmat)
  expect_equal(round(x1$Lik, 4), round(x2$Lik, 4), -568.6664)
  expect_equal(round(sum(diag(x1$Lambda)),4), round(sum(diag(x2$Lambda)),4), 1.5269)
  expect_equal(round(sum(x1$B),4),round(sum(x2$B),4), 3.8056)

  expect_equal(round(sum(x1$Lambda_SE), 4), 0.366, tolerance = 0.002)

  expect_gt(x1$lRatioTest$`Test statistic`, 0)

  expect_equal(x1$K, x2$K, 3)
  expect_equal(x1$n,x2$n, 169)
  expect_equal(x1$restrictions,x2$restrictions, 3)
  expect_equal(x1$SB,x2$SB, 59)
  expect_equal(x1$p,x2$p, 6)
  expect_equal(x1$iteration,x2$iteration, 5)

  expect_match(x1$method, x2$method, "Changes in Volatility")
})

test_that("restricted id.cv and boot estimation with 2-dim works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, ic = "AIC" )
  restmat <- matrix(NA,2,2)
  restmat[1,2] <- 0
  x1 <- id.cv(v1, SB = 59, restriction_matrix = restmat)
  x2 <- id.cv_boot(v1, SB = 59, SB2 = NULL, restriction_matrix = restmat)

  expect_equal(round(x1$Lik, 4), round(x2$Lik, 4), -422.3157)
  expect_equal(round(sum(diag(x1$Lambda)),4), round(sum(diag(x2$Lambda)),4), 0.6749)
  expect_equal(round(sum(x1$B),4), round(sum(x2$B),4), 2.5519)

  expect_equal(round(sum(x1$Lambda_SE), 4), 0.1566)

  expect_gt(x1$lRatioTest$`Test statistic`, 0)

  expect_equal(x1$K, x2$K, 2)
  expect_equal(x1$n,x2$n, 172)
  expect_equal(x1$restrictions,x2$restrictions, 1)
  expect_equal(x1$SB, x2$SB, 59)
  expect_equal(x1$p,x2$p, 3)
  expect_equal(x1$iteration, x2$iteration, 4)

  expect_match(x1$method, x2$method, "Changes in Volatility")
})

test_that("unrestricted id.cv and boot estimation with 5-dim works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(LN, p = 3)
  x1 <- id.cv(v1, SB = 250)
  x2 <- id.cv_boot(v1, SB = 250, SB2 = NULL)

  expect_equal(round(x1$Lik, 4), round(x2$Lik, 4), -2933.152)
  expect_equal(round(sum(diag(x1$Lambda)),4),round(sum(diag(x2$Lambda)),4), 2.8296)
  expect_equal(round(sum(x1$B),4),round(sum(x2$B),4), 10.078)

  expect_equal(round(sum(x1$Lambda_SE), 4), 0.3807, tolerance = 0.002)

  expect_equal(x1$K,x2$K, 5)
  expect_equal(x1$n,x2$n, 447)
  expect_equal(x1$restrictions,x2$restrictions, 0)
  expect_equal(x1$SB,x2$SB, 250)
  expect_equal(x1$p,x2$p, 3)
  expect_equal(x1$iteration,x2$iteration, 8)

  expect_match(x1$method, x2$method, "Changes in Volatility")
})

test_that("restricted id.cv and boot estimation with 5-dim works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(LN, p = 3)

  restmat <- matrix(NA, 5, 5)
  restmat[1, 2:5] <- 0
  restmat[2, 3:5] <- 0
  restmat[3, 4:5] <- 0
  restmat[4, 5] <- 0

  x1 <- id.cv(v1, SB = 250, restriction_matrix = restmat)
  x2 <- id.cv_boot(v1, SB = 250, SB2 = NULL, restriction_matrix = restmat)

  expect_equal(round(x1$Lik, 4), round(x2$Lik, 4), -2944.729)
  expect_equal(round(sum(diag(x1$Lambda)),4),round(sum(diag(x2$Lambda)),4), 2.7272)
  expect_equal(round(sum(x1$B),4),round(sum(x2$B),4), 9.0169)

  expect_equal(round(sum(x1$Lambda_SE), 4), 0.3686, tolerance = 0.002)

  expect_gt(x1$lRatioTest$`Test statistic`, 0)

  expect_equal(x1$K,x2$K, 5)
  expect_equal(x1$n,x2$n, 447)
  expect_equal(x1$restrictions,x2$restrictions, 10)
  expect_equal(x1$SB,x2$SB, 250)
  expect_equal(x1$p,x2$p, 3)
  expect_equal(x1$iteration,x2$iteration, 12)

  expect_match(x1$method, x2$method, "Changes in Volatility")
})

test_that("unrestricted id.cv estimation with 3-dim works with vector as SB", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )

  SB <- rep(0, v1$obs)
  SB[50:80] <- 1
  SB[100:120] <- 1

  x1 <- id.cv(v1, SB = SB)
  x2 <- id.cv_boot(v1, SB = SB, SB2 = NULL)

  expect_equal(round(x1$Lik, 4), round(x2$Lik, 4), -558.2536)
  expect_equal(round(sum(diag(x1$Lambda)),4),  round(sum(diag(x2$Lambda)),4), 7.8275)
  expect_equal(round(sum(x1$B),4), round(sum(x2$B),4), 1.5372)

  expect_equal(round(sum(x1$Lambda_SE), 4), 1.8457, tolerance = 0.002)

  expect_equal(x1$K, x2$K, 3)
  expect_equal(x1$n, x2$n, 169)
  expect_equal(x1$restrictions,x2$restrictions, 0)
    expect_equal(x1$p,x2$p, 6)
  expect_equal(x1$iteration,x2$iteration, 14)

  expect_match(x1$method, x2$method, "Changes in Volatility")
})

test_that("restricted id.cv estimation with 3-dim works with vector as SB", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )

  SB <- rep(0, v1$obs)
  SB[50:80] <- 1
  SB[100:120] <- 1

  restmat <- matrix(NA, 3, 3)
  restmat[1,c(2,3)] <- 0
  restmat[2,3] <- 0
  x1 <- id.cv(v1, SB = SB, restriction_matrix = restmat)
  x2 <- id.cv_boot(v1, SB = SB, SB2 = NULL, restriction_matrix = restmat)

  expect_equal(round(x1$Lik, 4), round(x2$Lik, 4), -569.0054)
  expect_equal(round(sum(diag(x1$Lambda)),4),  round(sum(diag(x2$Lambda)),4), 7.4017)
  expect_equal(round(sum(x1$B),4), round(sum(x2$B),4), 2.5515)

  expect_equal(round(sum(x1$Lambda_SE), 4), 1.7649, tolerance = 0.002)

  expect_gt(x1$lRatioTest$`Test statistic`, 0)

  expect_equal(x1$K, x2$K, 3)
  expect_equal(x1$n, x2$n, 169)
  expect_equal(x1$restrictions,x2$restrictions, 3)
  expect_equal(x1$p,x2$p, 6)
  expect_equal(x1$iteration,x2$iteration, 14)

  expect_match(x1$method, x2$method, "Changes in Volatility")
})
