context("likelihood_ngml.R")

test_that("Likelihood function within ngml, stage2 works for 3x3", {
  skip_on_cran()
  set.seed(23213)
  theta1 = c(rnorm(6, sd =.6), rep(1,3), rep(5,3))
  Tob1 = 169
  k1 = 3
  u1 = matrix(rnorm(Tob1*3), ncol = 3)
  il1 <- matrix(0, k1*k1, k1*k1)
  il1[1,1] <- 1
  il1[5,5] <- 1
  il1[9,9] <- 1
  rows1 = c(1,5,9)
  likelihood_result = likelihood_ngml_stage2(theta = theta1,
                                    u = u1, k = k1,
                                    il = il1, restrictions = 0,
                                    rows = rows1, restriction_matrix = NULL)
  expect_equal(round(likelihood_result, 3), 1612.772)
})

test_that("Likelihood function within ngml, stage2 works for 2x2", {
  skip_on_cran()
  set.seed(23214)
  theta2 = c(rnorm(2, sd =.6), rep(1,2), rep(5,2))
  Tob2 = 169
  k2 = 2
  u2 = matrix(rnorm(Tob2*2), ncol = 2)
  il2 <- matrix(0, k2*k2, k2*k2)
  il2[1,1] <- 1
  il2[4,4] <- 1
  rows2 = c(1,4)
  likelihood_result = likelihood_ngml_stage2(theta = theta2,
                                             u = u2, k = k2,
                                             il = il2, restrictions = 0,
                                             rows = rows2, restriction_matrix = NULL)
  expect_equal(round(likelihood_result, 4), 508.5066)
})

    test_that("Likelihood function within ngml, stage2 works for 5x5", {
      skip_on_cran()
      set.seed(23215)
      theta3 = c(rnorm(20, sd =.6), rep(1,5), rep(5,5))
      Tob3 = 169
      k3 = 5
      u3 = matrix(rnorm(Tob3*5), ncol = 5)
      il3 <- matrix(0, k3*k3, k3*k3)
      il3[1,1] <- 1
      il3[7,7] <- 1
      il3[13,13] <- 1
      il3[19,19] <- 1
      il3[25,25] <- 1
      rows3 = c(1, 7, 13, 19, 25)
      likelihood_result = likelihood_ngml_stage2(theta = theta3,
                                                 u = u3, k = k3,
                                                 il = il3, restrictions = 0,
                                                 rows = rows3, restriction_matrix = NULL)
      expect_equal(round(likelihood_result, 1), 1596.8)
    })

    # test_that("Likelihood function within ngml, stage3 works for 5x5", {
    #   skip_on_cran()
    #   set.seed(23213)
    #   A5 = rnorm(25)
    #   Tob5 = 169
    #   Z_t5 = matrix(c(rep(1,Tob5), rnorm(4*Tob5)), ncol = Tob5, byrow = T)
    #   k5 = 5
    #   il5 <- matrix(0, k5*k5, k5*k5)
    #   il5[1,1] <- 1
    #   il5[7,7] <- 1
    #   il5[13,13] <- 1
    #   il5[19,19] <- 1
    #   il5[25,25] <- 1
    #   rows5 = c(1, 7, 13, 19, 25)
    #   y5 = matrix(rnorm(5 * Tob5), nrow = 5)
    #   B_stand_est5 = matrix(rnorm(25), ncol = 5)
    #   sigma_est5 = sample(seq(0.000001,1, by=0.000001), 5)
    #   diag(B_stand_est5) = 1
    #   d_freedom5 = c(16.006191, 12.039321, 12.742021,  8.795433,  7.131478)
    #
    #   likelihood_result = likelihood_ngml_stage3(A = A5,
    #                                              Z_t = Z_t5,
    #                                              y = y5,
    #                                              il = il5,
    #                                              B_stand_est = B_stand_est5,
    #                                              rows = rows5,
    #                                              sigma_est = sigma_est5,
    #                                              d_freedom = d_freedom5)
    #   expect_equal(round(likelihood_result, 4), 1612.772)
    # })
