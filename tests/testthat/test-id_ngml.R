context("test-id_ngml.R")

test_that("id.ngml and id.ngml_boot 3-dim stage3 = FALSE works, no restriction" , {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6, ic = "AIC" )
  x1 <- id.ngml(v1)
  x2 <- id.ngml_boot(v1)
  expect_equal(round(x1$Lik, 4),round(x2$Lik, 4), -548.1502)
  expect_equal(sum(round(x1$B, 4)), sum(round(x2$B, 4)), 2.7768)
  expect_equal(round(sum(x1$df),4), round(sum(x2$df),4), 12.9978)

  expect_equal(x1$K,x2$K, 3)
  expect_equal(x1$n,x2$n, 169)
  expect_equal(x1$stage3,x2$stage3, FALSE)
  expect_equal(x1$p,x2$p, 6)

  expect_match(x1$method,x2$method, "Non-Gaussian maximum likelihood")
})

test_that("id.ngml and boot 3-dim stage3 = TRUE works, no restriction", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6, ic = "AIC" )
  x1 <- id.ngml(v1, stage3 =  TRUE)
  x2 <- id.ngml_boot(v1, stage3 = TRUE)
  expect_equal(round(x1$Lik, 4),round(x2$Lik, 4), -548.1502)
  expect_equal(sum(round(x1$B, 4)),sum(round(x2$B, 4)), 2.7768)
  expect_equal(round(sum(x1$df),4),round(sum(x2$df),4), 12.9978)

  expect_equal(x1$K, x2$K, 3)
  expect_equal(x1$n, x2$n, 169)
  expect_equal(x1$stage3, x2$stage3, TRUE)
  expect_equal(x1$p, x2$p, 6)

  expect_match(x1$method, x2$method, "Non-Gaussian maximum likelihood")
})

test_that("id.ngml and boot 2-dim stage3 = FALSE works, no restriction", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, ic = "AIC" )
  x1 <- id.ngml(v1)
  x2 <- id.ngml_boot(v1)

  expect_equal(round(x1$Lik, 4),round(x2$Lik, 4), -425.5546)
  expect_equal(sum(round(x1$B, 4)), sum(round(x2$B, 4)), 1.8334)
  expect_equal(round(sum(x1$df),4),round(sum(x2$df),4), 7.8661)

  expect_equal(x1$K, x2$K, 2)
  expect_equal(x1$n, x2$n, 172)
  expect_equal(x1$stage3, x2$stage3, FALSE)
  expect_equal(x1$p,x2$p, 3)

  expect_match(x1$method, x2$method, "Non-Gaussian maximum likelihood")
})

test_that("id.ngml and boot 2-dim stage3 = FALSE works, no restriction constant + trend", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, type = 'both' )
  x1 <- id.ngml(v1)
  x2 <- id.ngml_boot(v1)

  expect_equal(round(x1$Lik, 4),round(x2$Lik, 4), -422.4783)
  expect_equal(sum(round(x1$B, 4)), sum(round(x2$B, 4)), 1.7762)
  expect_equal(round(sum(x1$df),4),round(sum(x2$df),4), 8.2167)

  expect_equal(x1$K, x2$K, 2)
  expect_equal(x1$n, x2$n, 172)
  expect_equal(x1$stage3, x2$stage3, FALSE)
  expect_equal(x1$p,x2$p, 3)

  expect_match(x1$method, x2$method, "Non-Gaussian maximum likelihood")
})

test_that("id.ngml and boot 2-dim stage3 = FALSE works, no restriction without deterministic term", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, type = 'none' )
  x1 <- id.ngml(v1)
  x2 <- id.ngml_boot(v1)

  expect_equal(round(x1$Lik, 4),round(x2$Lik, 4), -426.3248)
  expect_equal(sum(round(x1$B, 4)), sum(round(x2$B, 4)), 1.8878)
  expect_equal(round(sum(x1$df),4),round(sum(x2$df),4), 7.6745)

  expect_equal(x1$K, x2$K, 2)
  expect_equal(x1$n, x2$n, 172)
  expect_equal(x1$stage3, x2$stage3, FALSE)
  expect_equal(x1$p,x2$p, 3)

  expect_match(x1$method, x2$method, "Non-Gaussian maximum likelihood")
})

test_that("id.ngml and boot 2-dim stage3 = TRUE works, no restriction constant + trend", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, type = 'both')
  x1 <- id.ngml(v1, stage3 = TRUE)
  x2 <- id.ngml_boot(v1, stage3 = TRUE)

  expect_equal(round(x1$Lik, 4),round(x2$Lik, 4), -422.4783)
  expect_equal(sum(round(x1$B, 4)), sum(round(x2$B, 4)), 1.7762)
  expect_equal(round(sum(x1$df),4),round(sum(x2$df),4), 8.2167)

  expect_equal(x1$K, x2$K, 2)
  expect_equal(x1$n, x2$n, 172)
  expect_equal(x1$stage3, x2$stage3, FALSE)
  expect_equal(x1$p,x2$p, 3)

  expect_match(x1$method, x2$method, "Non-Gaussian maximum likelihood")
})

test_that("id.ngml and boot 2-dim stage3 = TRUE works, no restriction without deterministic term", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, type = 'none' )
  x1 <- id.ngml(v1, stage3 = TRUE)
  x2 <- id.ngml_boot(v1, stage3 = TRUE)

  expect_equal(round(x1$Lik, 4),round(x2$Lik, 4), -426.3248)
  expect_equal(sum(round(x1$B, 4)), sum(round(x2$B, 4)), 1.8878)
  expect_equal(round(sum(x1$df),4),round(sum(x2$df),4), 7.6745)

  expect_equal(x1$K, x2$K, 2)
  expect_equal(x1$n, x2$n, 172)
  expect_equal(x1$stage3, x2$stage3, FALSE)
  expect_equal(x1$p,x2$p, 3)

  expect_match(x1$method, x2$method, "Non-Gaussian maximum likelihood")
})

test_that("id.ngml 2-dim stage3 = TRUE works, no restriction", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, ic = "AIC" )
  x1 <- id.ngml(v1, stage3 = TRUE)
  x2 <- id.ngml_boot(v1, stage3 = TRUE)

  expect_equal(round(x1$Lik, 4), round(x2$Lik, 4), -425.5546)
  expect_equal(sum(round(x1$B, 4)), sum(round(x2$B, 4)), 1.8334)
  expect_equal(round(sum(x1$df),4), round(sum(x2$df),4), 7.8661)

  expect_equal(x1$K, x2$K, 2)
  expect_equal(x1$n, x2$n, 172)
  expect_equal(x1$stage3, x2$stage3, TRUE)
  expect_equal(x1$p, x2$p,3)

  expect_match(x1$method, "Non-Gaussian maximum likelihood")
})

test_that("id.ngml and id.ngml_boot 3-dim stage3 = FALSE works, with restriction" , {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6, ic = "AIC" )
  restriction_matrix <- matrix(NA, 3, 3)
  restriction_matrix[1, 2:3] <- 0
  restriction_matrix[2, 3] <- 0
  x1 <- id.ngml(v1, restriction_matrix = restriction_matrix)
  x2 <- id.ngml_boot(v1, restriction_matrix = restriction_matrix)
  expect_equal(round(x1$Lik, 4), round(x2$Lik, 4), 561.8295)
  expect_equal(sum(round(x1$B, 4)), sum(round(x2$B, 4)), 2.6431)
  expect_equal(round(sum(x1$df),4), round(sum(x2$df),4), 16.8249)

  expect_equal(x1$K,x2$K, 3)
  expect_equal(x1$n,x2$n, 169)
  expect_equal(x1$stage3,x2$stage3, FALSE)
  expect_equal(x1$p,x2$p, 6)

  expect_gt(x1$lRatioTest$`Test statistic`, 0)

  expect_match(x1$method,x2$method, "Non-Gaussian maximum likelihood")
})

test_that("id.ngml and boot 3-dim stage3 = TRUE works, with restriction", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA, p = 6, ic = "AIC" )
  restriction_matrix <- matrix(NA, 3, 3)
  restriction_matrix[1, 2:3] <- 0
  restriction_matrix[2, 3] <- 0
  x1 <- id.ngml(v1, stage3 =  TRUE, restriction_matrix = restriction_matrix)
  x2 <- id.ngml_boot(v1, stage3 = TRUE, restriction_matrix = restriction_matrix)
  expect_equal(round(x1$Lik, 4), round(x2$Lik, 4), -561.8295)
  expect_equal(sum(round(x1$B, 4)),sum(round(x2$B, 4)), 2.6431)
  expect_equal(round(sum(x1$df),4),round(sum(x2$df),4), 16.8249)

  expect_gt(x1$lRatioTest$`Test statistic`, 0)

  expect_equal(x1$K, x2$K, 3)
  expect_equal(x1$n, x2$n, 169)
  expect_equal(x1$stage3, x2$stage3, TRUE)
  expect_equal(x1$p, x2$p, 6)

  expect_match(x1$method, x2$method, "Non-Gaussian maximum likelihood")
})

test_that("id.ngml and boot 2-dim stage3 = FALSE works, with restriction", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, ic = "AIC" )
  restriction_matrix <- matrix(NA, 2, 2)
  restriction_matrix[1, 2] <- 0
  x1 <- id.ngml(v1, restriction_matrix = restriction_matrix)
  x2 <- id.ngml_boot(v1, restriction_matrix = restriction_matrix)

  expect_equal(round(x1$Lik, 4), round(x2$Lik, 4), -433.6669)
  expect_equal(sum(round(x1$B, 4)), sum(round(x2$B, 4)), 1.8721)
  expect_equal(round(sum(x1$df),4), round(sum(x2$df),4), 8.3969)

  expect_gt(x1$lRatioTest$`Test statistic`, 0)

  expect_equal(x1$K, x2$K, 2)
  expect_equal(x1$n, x2$n, 172)
  expect_equal(x1$stage3, x2$stage3, FALSE)
  expect_equal(x1$p,x2$p, 3)

  expect_match(x1$method, x2$method, "Non-Gaussian maximum likelihood")
})

test_that("id.ngml 2-dim stage3 = TRUE works, with restriction", {
  skip_on_cran()
  set.seed(23211)
  v1 <- vars::VAR(USA[,-3], p = 3, ic = "AIC" )
  restriction_matrix <- matrix(NA, 2, 2)
  restriction_matrix[1, 2] <- 0
  x1 <- id.ngml(v1, stage3 = TRUE, restriction_matrix = restriction_matrix)
  x2 <- id.ngml_boot(v1, stage3 = TRUE, restriction_matrix = restriction_matrix)

  expect_equal(round(x1$Lik, 4), round(x2$Lik, 4), -433.6669)
  expect_equal(sum(round(x1$B, 4)), sum(round(x2$B, 4)), 1.8721)
  expect_equal(round(sum(x1$df),4), round(sum(x2$df),4), 8.3969)

  expect_gt(x1$lRatioTest$`Test statistic`, 0)

  expect_equal(x1$K, x2$K, 2)
  expect_equal(x1$n, x2$n, 172)
  expect_equal(x1$stage3, x2$stage3, TRUE)
  expect_equal(x1$p, x2$p,3)

  expect_match(x1$method, "Non-Gaussian maximum likelihood")
})

