context("likelihood_cv.R")

test_that("Likelihood function within cv works for 3x3, no restriction", {
  skip_on_cran()
  set.seed(23212)
  S1 = c(rnorm(9, mean = 0, sd = .6), rep(1,3))
  Tob1 = 169
  Sigma_hat1_1 = matrix(c(rnorm(9, mean = 0, sd = .6)), ncol = 3)
  Sigma_hat2_1 = matrix(c(rnorm(9, mean = 0, sd = .6)), ncol = 3)
  restriction_matrix_1 = matrix(rep(NA,9), 3, 3)
  likelihood_result = LikelihoodCV(S = S1,
               Tob = Tob1,
               TB = 54,
               SigmaHat1 = Sigma_hat1_1,
               k = 3,
               SigmaHat2 = Sigma_hat2_1,
               RestrictionMatrix = restriction_matrix_1,
               restrictions =0)
  expect_equal(round(likelihood_result, 4), -864.1661)
})

test_that("Likelihood function within cv works for 2x2, no restriction", {
  set.seed(23214)
  S2 = c(rnorm(4, mean = 0, sd = .6), rep(1,2))
  Tob2 = 172
  Sigma_hat1_2 = matrix(c(rnorm(4, mean = 0, sd = .6)), ncol = 2)
  Sigma_hat2_2 = matrix(c(rnorm(4, mean = 0, sd = .6)), ncol = 2)
  restriction_matrix_2 = matrix(rep(NA,4), 2, 2)
  likelihood_result = LikelihoodCV(S = S2,
                                   Tob = Tob2,
                                   TB = 54,
                                   SigmaHat1 = Sigma_hat1_2,
                                   k = 2,
                                   SigmaHat2 = Sigma_hat2_2,
                                   RestrictionMatrix = restriction_matrix_2,
                                   restrictions =0)

  expect_equal(round(likelihood_result, 4), -319.2551)
})

  test_that("Likelihood function within cv works for 5x5, no restriction", {
    skip_on_cran()
    set.seed(23215)
    S3 = c(rnorm(25, mean = 0, sd = .6), rep(1,5))
    Tob3 = 172
    Sigma_hat1_3 = matrix(c(rnorm(25, mean = 0, sd = .6)), ncol = 5)
    Sigma_hat2_3 = matrix(c(rnorm(25, mean = 0, sd = .6)), ncol = 5)
    restriction_matrix_3 = matrix(rep(NA,25), 5, 5)
    likelihood_result = LikelihoodCV(S = S3,
                                     Tob = Tob3,
                                     TB = 54,
                                     SigmaHat1 = Sigma_hat1_3,
                                     k = 5,
                                     SigmaHat2 = Sigma_hat2_3,
                                     RestrictionMatrix = restriction_matrix_3,
                                     restrictions = 0)
    expect_equal(round(likelihood_result, 3), -1262.304)

  })
  test_that("Likelihood function within cv works for 3x3, with restriction", {
    skip_on_cran()
    set.seed(23213)
    S4 = c(rnorm(8, mean = 0, sd = .6), rep(1,3))
    Tob4 = 169
    Sigma_hat1_4 = matrix(c(rnorm(9, mean = 0, sd = .6)), ncol = 3)
    Sigma_hat2_4 = matrix(c(rnorm(9, mean = 0, sd = .6)), ncol = 3)
    restMat <- matrix(rep(NA, 9), ncol = 3)
    restMat[1,3] <- 0
    restriction_matrix_4 = restMat
    restrictions_4 <- length(restriction_matrix_4[!is.na(restriction_matrix_4)])
    likelihood_result = LikelihoodCV(S = S4,
                                     Tob = Tob4,
                                     TB = 54,
                                     SigmaHat1 = Sigma_hat1_4,
                                     k = 3,
                                     SigmaHat2 = Sigma_hat2_4,
                                     RestrictionMatrix = restriction_matrix_4,
                                     restrictions =1)
    expect_equal(round(likelihood_result, 3), -2211.323)
  })


  test_that("Likelihood function within cv works for 2x2, with restriction", {
    set.seed(23214)
    S5 = c(rnorm(3, mean = 0, sd = .6), rep(1,2))
    Tob5 = 172
    Sigma_hat1_5 = matrix(c(rnorm(4, mean = 0, sd = .6)), ncol = 2)
    Sigma_hat2_5 = matrix(c(rnorm(4, mean = 0, sd = .6)), ncol = 2)
    restMat <- matrix(rep(NA, 4), ncol = 2)
    restMat[1,2] <- 0
    restriction_matrix_5 = restMat
    restrictions_5 <- length(restriction_matrix_5[!is.na(restriction_matrix_5)])
    likelihood_result = LikelihoodCV(S = S5,
                                     Tob = Tob5,
                                     TB = 54,
                                     SigmaHat1 = Sigma_hat1_5,
                                     k = 2,
                                     SigmaHat2 = Sigma_hat2_5,
                                     RestrictionMatrix = restriction_matrix_5,
                                     restrictions = restrictions_5)
    expect_equal(round(likelihood_result, 4), 15.2476)
  })
    test_that("Likelihood function within cv works for 5x5, no restriction", {
      skip_on_cran()
      set.seed(23215)
      S6 = c(rnorm(24, mean = 0, sd = .6), rep(1,5))
      Tob6 = 172
      Sigma_hat1_6 = matrix(c(rnorm(25, mean = 0, sd = .6)), ncol = 5)
      Sigma_hat2_6 = matrix(c(rnorm(25, mean = 0, sd = .6)), ncol = 5)
      restMat <- matrix(rep(NA, 25), ncol = 5)
      restMat[1,5] <- 0
      restriction_matrix_6 = restMat
      restrictions_6 <- length(restriction_matrix_6[!is.na(restriction_matrix_6)])
      likelihood_result = LikelihoodCV(S = S6,
                                       Tob = Tob6,
                                       TB = 54,
                                       SigmaHat1 = Sigma_hat1_6,
                                       k = 5,
                                       SigmaHat2 = Sigma_hat2_6,
                                       RestrictionMatrix = restriction_matrix_6,
                                       restrictions = restrictions_6)
      expect_equal(round(likelihood_result, 2), 265.72)

})

    # test_that("Likelihood function returns 1e25, 3x3 dim, no restrictions", {
    #   skip_on_cran()
    #   set.seed(23213)
    #   S1 = c(rnorm(9, mean = 0, sd = .6), rep(1,3))
    #   Tob1 = 169
    #   Sigma_hat1_1 = matrix(c(rnorm(9, mean = 0, sd = .6)), ncol = 3)
    #   Sigma_hat2_1 = matrix(c(rnorm(9, mean = 0, sd = .6)), ncol = 3)
    #   restriction_matrix_1 = matrix(rep(NA,9), 3, 3)
    #   likelihood_result = LikelihoodCV(S = S1,
    #                                     Tob = Tob1,
    #                                     TB = 54,
    #                                     SigmaHat1 = Sigma_hat1_1,
    #                                     k = 3,
    #                                     SigmaHat2 = Sigma_hat2_1,
    #                                     RestrictionMatrix = restriction_matrix_1,
    #                                    restrictions = 0)
    #   expect_equal(round(likelihood_result, 4), 1e25)
    # })
