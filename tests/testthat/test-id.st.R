context("id.st")

test_that("unrestricted id.st estimation with pre specified c and gamma works 3-dim works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6, ic = "AIC")
  x1 <- id.st(v1, c_fix = 80, gamma_fix = -1)

  expect_equal(round(x1$Lik, 4),  -516.8098)
  expect_equal(round(sum(diag(x1$Lambda)),4), 0.633)
  expect_equal(round(sum(x1$B),4), 3.5697)

  expect_equal(round(sum(x1$Lambda_SE), 4), 0.1393)

  expect_equal(x1$K, 3)
  expect_equal(x1$n,169)
  expect_equal(x1$restrictions,0)
  expect_equal(x1$est_c, 80)
  expect_equal(x1$p, 6)
  expect_equal(x1$iteration, 5)

  expect_match(x1$method, "Smooth transition")

})

test_that("unrestricted id.st estimation with pre specified c and gamma works 3-dim works with constant + trend", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6, ic = "AIC", type = 'both')
  x1 <- id.st(v1, c_fix = 80, gamma_fix = -1)

  expect_equal(round(x1$Lik, 4),  -510.3289)
  expect_equal(round(sum(diag(x1$Lambda)),4),  0.6418)
  expect_equal(round(sum(x1$B),4), 3.5395)

  expect_equal(round(sum(x1$Lambda_SE), 4), 0.1409)

  expect_equal(x1$K, 3)
  expect_equal(x1$n,169)
  expect_equal(x1$restrictions,0)
  expect_equal(x1$est_c, 80)
  expect_equal(x1$p, 6)
  expect_equal(x1$iteration, 5)

  expect_match(x1$method, "Smooth transition")
})

test_that("unrestricted id.st estimation with pre specified c and gamma works 3-dim works without any deterministic term", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6, ic = "AIC", type = 'none')
  x1 <- id.st(v1, c_fix = 80, gamma_fix = -1)

  expect_equal(round(x1$Lik, 4),  -518.9424)
  expect_equal(round(sum(diag(x1$Lambda)),4),  0.6292)
  expect_equal(round(sum(x1$B),4), 3.5978)

  expect_equal(round(sum(x1$Lambda_SE), 4), 0.1384)

  expect_equal(x1$K, 3)
  expect_equal(x1$n,169)
  expect_equal(x1$restrictions,0)
  expect_equal(x1$est_c, 80)
  expect_equal(x1$p, 6)
  expect_equal(x1$iteration, 5)

  expect_match(x1$method, "Smooth transition")
})

test_that("unrestricted id.st estimation with searching algo 3-dim works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6, ic = "AIC" )
  cores <- 2
  x1 <- id.st(v1, nc = cores, c_lower = 0.4, c_upper = 0.6, c_step = 5, c_fix = NULL,
              transition_variable = NULL, gamma_lower = -3, gamma_upper = 2,
              gamma_step = 1, gamma_fix = NULL, max.iter = 5,
              crit = 0.01, restriction_matrix = NULL, lr_test = FALSE)

  expect_equal(round(x1$Lik, 4),  -510.1889)
  expect_equal(round(sum(diag(x1$Lambda)),4), 0.5685)
  expect_equal(round(sum(x1$B),4), 3.6847)

  expect_equal(round(sum(x1$Lambda_SE), 4), 0.1256)

  expect_equal(x1$K, 3)
  expect_equal(x1$n,169)
  expect_equal(x1$restrictions,0)
  expect_equal(x1$est_c, 73)
  expect_equal(x1$est_g, -1)
  expect_equal(x1$p, 6)
  expect_equal(x1$iteration, 5)

  expect_match(x1$method, "Smooth transition")

})

test_that("id.st estimation with restrictions 3-dim works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6, ic = "AIC" )
  restmat <- matrix(NA, 3, 3)
  restmat[1, 2:3] <- 0
  restmat[2, 3] <- 0

  ## Without lr test
  x1 <- id.st(v1, c_fix = 80, gamma_fix = -1, restriction_matrix = restmat)

  expect_equal(round(x1$Lik, 4),  -518.5316)
  expect_equal(round(sum(diag(x1$Lambda)), 4), 0.6118)
  expect_equal(round(sum(x1$B), 4), 3.5253)

  expect_equal(round(sum(x1$Lambda_SE), 4), 0.1352)

  expect_equal(x1$K, 3)
  expect_equal(x1$n,169)
  expect_equal(x1$restrictions, 3)
  expect_equal(x1$est_c, 80)
  expect_equal(x1$p, 6)
  expect_equal(x1$iteration, 5)

  expect_match(x1$method, "Smooth transition")

  ## With lr test
  x2 <- id.st(v1, c_fix = 80, gamma_fix = -1, restriction_matrix = restmat, lr_test = TRUE)

  expect_equal(round(x2$Lik, 4), round(x1$Lik, 4),  -518.5316)
  expect_equal(round(sum(diag(x1$Lambda)), 4), round(sum(diag(x2$Lambda)), 4), 0.6118)
  expect_equal(round(sum(x1$B), 4), round(sum(x2$B), 4), 3.5253)

  expect_equal(round(sum(x1$Lambda_SE), 4), round(sum(x2$Lambda_SE), 4), 0.1352)

  expect_equal(round(x2$lRatioTest[[2]], 3), 0.328)
})

test_that("Replication of Luetkepohl + Netsunajev 2017 5-dim works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(LN, p = 3, ic = "AIC" )

  x1 <- id.st(v1, c_fix = 167, gamma_fix = -2.77)

  expect_equal(round(x1$Lik, 2),  -2878.27)
  expect_equal(round(sum(diag(x1$Lambda)), 3), 2.677)
  expect_equal(round(sum(x1$B), 3), 3.263)

  expect_equal(round(sum(x1$Lambda_SE), 3), 0.424)

  expect_equal(x1$K, 5)
  expect_equal(x1$n, 447)
  expect_equal(x1$iteration, 5)

  expect_match(x1$method, "Smooth transition")
})
