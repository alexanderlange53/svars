context("test-id.garch.R")

test_that("unrestricted id.garch estimation with 3-dim works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6, ic = "AIC" )
  x1 <- id.garch(v1)

  expect_equal(round(x1$Lik, 4), -551.5108)
  expect_equal(round(sum(x1$B),4), 2.3896)


  expect_equal(x1$K, 3)
  expect_equal(x1$n, 169)
  #expect_equal(x1$restrictions,0)
  expect_equal(x1$p, 6)
  expect_equal(x1$iteration,5)

  expect_match(x1$method,  "GARCH")
})

test_that("unrestricted id.cv and boot estimation with 2-dim works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, ic = "AIC" )
  x1 <- id.garch(v1)
  expect_equal(round(x1$Lik, 4), -411.4538)
  expect_equal(round(sum(x1$B),4), -0.5085)

  expect_equal(x1$K, 2)
  expect_equal(x1$n, 172)
  #expect_equal(x1$restrictions, 0)
  expect_equal(x1$p, 3)
  expect_equal(x1$iteration, 5)

  expect_match(x1$method, "GARCH")
})

# test_that("restricted id.garch with 3-dim works", {
#   skip_on_cran()
#   set.seed(23211)
#   v1 <- vars::VAR(USA, p = 6, ic = "AIC" )
#   restmat <- matrix(NA,3,3)
#   restmat[1,c(2,3)] <- 0
#   restmat[2,3] <- 0
#   x1 <- id.garch(v1, restriction_matrix = restmat)
#   expect_equal(round(x1$Lik, 4),  -102.7646)
#   expect_equal(round(sum(x1$B),4), 3.8043)
#
#   expect_equal(x1$K, )
#   expect_equal(x1$n, 169)
#   expect_equal(x1$restrictions, 3)
#   expect_equal(x1$p, 6)
#   expect_equal(x1$iteration, 5)
#
#   expect_match(x1$method, "GARCH")
# })
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
