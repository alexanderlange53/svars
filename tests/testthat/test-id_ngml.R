context("test-id_ngml.R")

test_that("id.ngml and id.ngml_boot 3-dim stage3 = FALSE works" , {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6, ic = "AIC" )
  x1 <- id.ngml(v1)
  x2 <- id.ngml_boot(v1)
  expect_equal(round(x1$Lik, 4),round(x2$Lik, 4), -548.1502)
  expect_equal(sum(round(x1$B, 4)), sum(round(x2$B, 4)), 2.7768)
  expect_equal(round(sum(x1$df),4), round(sum(x2$df),4), 12.9978)

  expect_equal(round(sum(x1$B_SE), 4), round(sum(x2$B_SE), 4), 1.0189)

  expect_equal(x1$K,x2$K, 3)
  expect_equal(x1$n,x2$n, 169)
  expect_equal(x1$stage3,x2$stage3, FALSE)
  expect_equal(x1$p,x2$p, 6)

  expect_match(x1$method,x2$method, "Non-Gaussian maximum likelihood")
})

test_that("id.ngml and boot 3-dim stage3 = TRUE works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6, ic = "AIC" )
  x1 <- id.ngml(v1, stage3 =  TRUE)
  x2 <- id.ngml_boot(v1, stage3 = TRUE)
  expect_equal(round(x1$Lik, 4),round(x2$Lik, 4), -548.1502)
  expect_equal(sum(round(x1$B, 4)),sum(round(x2$B, 4)), 2.7768)
  expect_equal(round(sum(x1$df),4),round(sum(x2$df),4), 12.9978)

  expect_equal(round(sum(x1$B_SE), 4), round(sum(x2$B_SE), 4), 1.0189)

  expect_equal(x1$K, x2$K, 3)
  expect_equal(x1$n, x2$n, 169)
  expect_equal(x1$stage3, x2$stage3, TRUE)
  expect_equal(x1$p, x2$p, 6)

  expect_match(x1$method, x2$method, "Non-Gaussian maximum likelihood")
})

test_that("id.ngml and boot 2-dim stage3 = FALSE works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, ic = "AIC" )
  x1 <- id.ngml(v1)
  x2 <- id.ngml_boot(v1)

  expect_equal(round(x1$Lik, 4),round(x2$Lik, 4), -425.5546)
  expect_equal(sum(round(x1$B, 4)), sum(round(x2$B, 4)), 1.8334)
  expect_equal(round(sum(x1$df),4),round(sum(x2$df),4), 7.8661)

  expect_equal(round(sum(x1$B_SE), 4), round(sum(x2$B_SE), 4), 0.4668)

  expect_equal(x1$K, x2$K, 2)
  expect_equal(x1$n, x2$n, 172)
  expect_equal(x1$stage3, x2$stage3, FALSE)
  expect_equal(x1$p,x2$p, 3)

  expect_match(x1$method, x2$method, "Non-Gaussian maximum likelihood")
})

test_that("id.ngml 2-dim stage3 = TRUE works", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, ic = "AIC" )
  x1 <- id.ngml(v1, stage3 = TRUE)
  x2 <- id.ngml(v1, stage3 = TRUE)

  expect_equal(round(x1$Lik, 4),round(x2$Lik, 4), -425.5546)
  expect_equal(sum(round(x1$B, 4)), sum(round(x2$B, 4)), 1.8334)
  expect_equal(round(sum(x1$df),4), round(sum(x2$df),4), 7.8661)

  expect_equal(round(sum(x1$B_SE), 4), round(sum(x2$B_SE), 4), 0.4668)

  expect_equal(x1$K, x2$K, 2)
  expect_equal(x1$n, x2$n, 172)
  expect_equal(x1$stage3, x2$stage3, TRUE)
  expect_equal(x1$p, x2$p,3)

  expect_match(x1$method, "Non-Gaussian maximum likelihood")
})
