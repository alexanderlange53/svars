context("test-id.garch.R")

test_that("unrestricted id.garch estimation with 3-dim works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6)
  x1 <- id.garch(v1)

  expect_equal(round(x1$Lik, 1),  -547.2)
  expect_equal(round(sum(x1$B), 4), 2.6492)


  expect_equal(x1$K, 3)
  expect_equal(x1$n, 169)
  #expect_equal(x1$restrictions,0)
  expect_equal(x1$p, 6)
  expect_equal(x1$iteration, 5)

  expect_match(x1$method,  "GARCH")
})

test_that("unrestricted id.garch with 2-dim works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, ic = "AIC" )
  x1 <- id.garch(v1)
  expect_equal(round(x1$Lik, 4), -411.4529)
  expect_equal(round(sum(x1$B), 4), -0.5087)

  expect_equal(x1$K, 2)
  expect_equal(x1$n, 172)
  #expect_equal(x1$restrictions, 0)
  expect_equal(x1$p, 3)
  expect_equal(x1$iteration, 5)

  expect_match(x1$method, "GARCH")
})

test_that("unrestricted id.garch Luetkepohl Netsunajev example works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(LN, p = 3)
  x1 <- id.garch(v1, max.iter = 10)

  expect_equal(round(x1$Lik), -2891)
  expect_equal(round(sum(x1$B), 2), -4.9)

  expect_equal(x1$K, 5)
  expect_equal(x1$n, 447)
  #expect_equal(x1$restrictions, 0)
  expect_equal(x1$p, 3)
  expect_equal(x1$iteration, 10)

  expect_equal(round(unname(x1$GARCH_parameter[1,1]), 2), 0.38)
  expect_equal(round(unname(x1$GARCH_parameter[2,1]), 2), 0.34)
  expect_equal(round(unname(x1$GARCH_parameter[3,1]), 2), 0.25)
  expect_equal(round(unname(x1$GARCH_parameter[4,1]), 2), 0.14)
  expect_equal(round(unname(x1$GARCH_parameter[5,1]), 2), 0.1)

  expect_equal(round(unname(x1$GARCH_parameter[1,2]), 2), 0.09)
  expect_equal(round(unname(x1$GARCH_parameter[2,2]), 2), 0.61)
  expect_equal(round(unname(x1$GARCH_parameter[3,2]), 2), 0.24)
  expect_equal(round(unname(x1$GARCH_parameter[4,2]), 2), 0.81)
  expect_equal(round(unname(x1$GARCH_parameter[5,2]), 2), 0.82)

  expect_match(x1$method, "GARCH")
})

test_that("restricted id.garch with 3-dim works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6, ic = "AIC" )

  restmat <- matrix(NA,3,3)
  restmat[1, c(2,3)] <- 0
  restmat[2, 3] <- 0

  x1 <- id.garch(v1, restriction_matrix = restmat)
  expect_equal(round(x1$Lik, 4),  -551.7518)
  expect_equal(round(sum(x1$B), 4), 1.463)

  expect_equal(x1$K, 3)
  expect_equal(x1$n, 169)
  expect_equal(x1$restrictions, 3)
  expect_equal(x1$p, 6)
  expect_equal(x1$iteration, 5)

  expect_match(x1$method, "GARCH")
})

test_that("Restricted id.garch Luetkepohl Netsunajev example works with R3 model", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(LN, p = 3)
  restmat <- matrix(NA, 5, 5)
  restmat[1, 2:5] <- 0
  restmat[2, 3:5] <- 0
  restmat[3, 4:5] <- 0

  x1 <- id.garch(v1, max.iter = 10, restriction_matrix = restmat)
  expect_equal(round(x1$Lik), -3004)
  expect_equal(round(sum(x1$B), 4), -2.6578)

  expect_equal(x1$K, 5)
  expect_equal(x1$n, 447)
  #expect_equal(x1$restrictions, 0)
  expect_equal(x1$p, 3)
  expect_equal(x1$iteration, 10)

  expect_match(x1$method, "GARCH")
})
#
# test_that("restricted id.cv and boot estimation with 2-dim works", {
#   skip_on_cran()
#   set.seed(23211)
#   v1 <- vars::VAR(USA[,-3], p = 3, ic = "AIC" )
#   restmat <- matrix(NA,2,2)
#   restmat[1,2] <- 0
#   x1 <- id.garch(v1, restriction_matrix = restmat)
#
#   expect_equal(round(x1$Lik, 4), -106.2008)
#   expect_equal(round(sum(x1$B),4), 2.5518)
#
#   expect_equal(x1$K, 2)
#   expect_equal(x1$n, 172)
#   expect_equal(x1$restrictions, 1)
#   expect_equal(x1$p,x2$p, 3)
#   expect_equal(x1$iteration, 4)
#
#   expect_match(x1$method, x2$method, "GARCH")
# })
