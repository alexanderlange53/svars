context("likelihood_ngml.R")

test_that("Likelihood function within ngml, stage2 works for 3x3 no restriction", {
  skip_on_cran()
  set.seed(23213)
  theta1 = c(rnorm(6, sd =.6), rep(1,3), rep(5,3))
  Tob1 = 169
  k1 = 3
  u1 = matrix(rnorm(Tob1*3), ncol = 3)
  # il1 <- matrix(0, k1*k1, k1*k1)
  # il1[1,1] <- 1
  # il1[5,5] <- 1
  # il1[9,9] <- 1
  # rows1 = c(1,5,9)
  # likelihood_ngml_stage2(theta = theta1,
  #                                   u = u1, k = k1,
  #                                   il = il1, restrictions = 0,
  #                                   rows = rows1, restriction_matrix = NULL)

  # First run
  likelihood_result <- LikelihoodNGMLStage2(theta = theta1,
                                           u = u1, k = k1, Tob = Tob1,
                                           restrictions = 0,
                                           RestrictionMatrix = matrix(NA, k1, k1))
  expect_equal(round(likelihood_result, 3), 1612.772)

  # Second run
  likelihood_result <- LikelihoodNGMLStage2(theta = theta1,
                                           u = u1, k = k1, Tob = Tob1,
                                           restrictions = 0,
                                           RestrictionMatrix = matrix(NA, k1, k1))
  expect_equal(round(likelihood_result, 3), 1612.772)
})

test_that("Likelihood function within ngml, stage2 works for 2x2 no restriction", {
  skip_on_cran()
  set.seed(23214)
  theta2 = c(rnorm(2, sd =.6), rep(1,2), rep(5,2))
  Tob2 = 169
  k2 = 2
  u2 = matrix(rnorm(Tob2*2), ncol = 2)
  # il2 <- matrix(0, k2*k2, k2*k2)
  # il2[1,1] <- 1
  # il2[4,4] <- 1
  # rows2 = c(1,4)
  # likelihood_result <- likelihood_ngml_stage2(theta = theta2,
  #                                            u = u2, k = k2,
  #                                            il = il2, restrictions = 0,
  #                                            rows = rows2, restriction_matrix = NULL)
  #
  # expect_equal(round(likelihood_result, 4), 508.5066)

  likelihood_result <- LikelihoodNGMLStage2(theta = theta2,
                                           u = u2, k = k2, Tob = Tob2,
                                           restrictions = 0,
                                           RestrictionMatrix = matrix(NA, k2, k2))
  expect_equal(round(likelihood_result, 1), 508.5)
})

test_that("Likelihood function within ngml, stage2 works for 5x5 no restriction", {
      skip_on_cran()
      set.seed(23215)
      theta3 = c(rnorm(20, sd =.6), rep(1,5), rep(5,5))
      Tob3 = 169
      k3 = 5
      u3 = matrix(rnorm(Tob3*5), ncol = 5)
      # il3 <- matrix(0, k3*k3, k3*k3)
      # il3[1,1] <- 1
      # il3[7,7] <- 1
      # il3[13,13] <- 1
      # il3[19,19] <- 1
      # il3[25,25] <- 1
      # rows3 = c(1, 7, 13, 19, 25)
      # likelihood_result <- likelihood_ngml_stage2(theta = theta3,
      #                                            u = u3, k = k3,
      #                                            il = il3, restrictions = 0,
      #                                            rows = rows3, restriction_matrix = NULL)

      likelihood_result <- LikelihoodNGMLStage2(theta = theta3,
                                               u = u3, k = k3, Tob = Tob3,
                                               restrictions = 0,
                                               RestrictionMatrix = matrix(NA, k3, k3))
      expect_equal(round(likelihood_result, 1), 1596.8)
})


test_that("Likelihood function within ngml, stage2 works for 3x3 with restriction", {
      skip_on_cran()
      set.seed(23213)
      theta1 = c(rnorm(3, sd =.6), rep(1,3), rep(5,3))
      Tob1 = 169
      k1 = 3
      u1 = matrix(rnorm(Tob1*3), ncol = 3)
      # il1 <- matrix(0, k1*k1, k1*k1)
      # il1[1,1] <- 1
      # il1[5,5] <- 1
      # il1[9,9] <- 1
      # rows1 = c(1,5,9)
      restriction_matrix <- matrix(NA, 3, 3)
      restriction_matrix[1, 2:3] <- 0
      restriction_matrix[2, 3] <- 0
      # likelihood_result <- likelihood_ngml_stage2(theta = theta1,
      #                                            u = u1, k = k1,
      #                                            il = il1, restrictions = 3,
      #                                            rows = rows1, restriction_matrix = restriction_matrix)

      likelihood_result <- LikelihoodNGMLStage2(theta = theta1,
                                                u = u1, k = k1, Tob = Tob1,
                                                restrictions = 3,
                                                RestrictionMatrix = restriction_matrix)
      expect_equal(round(likelihood_result, 3), 892.911)
})

test_that("Likelihood function within ngml, stage2 works for 2x2 with restriction", {
      skip_on_cran()
      set.seed(23214)
      theta2 = c(rnorm(1, sd =.6), rep(1,2), rep(5,2))
      Tob2 = 169
      k2 = 2
      u2 = matrix(rnorm(Tob2*2), ncol = 2)
      # il2 <- matrix(0, k2*k2, k2*k2)
      # il2[1,1] <- 1
      # il2[4,4] <- 1
      # rows2 = c(1,4)
      restriction_matrix <- matrix(NA, 2, 2)
      restriction_matrix[1, 2] <- 0
      # likelihood_result <- likelihood_ngml_stage2(theta = theta2,
      #                                            u = u2, k = k2,
      #                                            il = il2, restrictions = 1,
      #                                            rows = rows2, restriction_matrix = restriction_matrix)

      likelihood_result <- LikelihoodNGMLStage2(theta = theta2,
                                                u = u2, k = k2, Tob = Tob2,
                                                restrictions = 1,
                                                RestrictionMatrix = restriction_matrix)
      expect_equal(round(likelihood_result, 4), 529.6885)
})

test_that("Likelihood function within ngml, stage2 works for 5x5 with restriction", {
      skip_on_cran()
      set.seed(23215)
      theta3 = c(rnorm(10, sd =.6), rep(1,5), rep(5,5))
      Tob3 = 169
      k3 = 5
      u3 = matrix(rnorm(Tob3*5), ncol = 5)
      # il3 <- matrix(0, k3*k3, k3*k3)
      # il3[1,1] <- 1
      # il3[7,7] <- 1
      # il3[13,13] <- 1
      # il3[19,19] <- 1
      # il3[25,25] <- 1
      # rows3 = c(1, 7, 13, 19, 25)
      restriction_matrix <- matrix(NA, 5, 5)
      restriction_matrix[1, 2:5] <- 0
      restriction_matrix[2, 3:5] <- 0
      restriction_matrix[3, 4:5] <- 0
      restriction_matrix[4, 5] <- 0
      # likelihood_result <- likelihood_ngml_stage2(theta = theta3,
      #                                            u = u3, k = k3,
      #                                            il = il3, restrictions = 10,
      #                                            rows = rows3, restriction_matrix = restriction_matrix)

      likelihood_result <- LikelihoodNGMLStage2(theta = theta3,
                                                u = u3, k = k3, Tob = Tob3,
                                                restrictions = 10,
                                                RestrictionMatrix = restriction_matrix)

      expect_equal(round(likelihood_result, 1), 1484.5)
})
