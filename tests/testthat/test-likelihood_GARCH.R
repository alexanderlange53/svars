test_that("Univariate GARCH likelihood 3dims works", {
  set.seed(23211)
  Tob <- 200
  k <- 3

  init_gamma <- runif(k)
  init_g <- rep(NA, k)
  test_g <- NA
  for(i in 1:k){
    test_g <- runif(1)
    if(init_gamma[i] + test_g < 1){
      init_g[i] <- test_g
    }else{
      while(init_gamma[i] + test_g > 1){
        test_g <- runif(1)
      }
      init_g[i] <- test_g
    }
  }
  parameter_ini_univ <- cbind(init_gamma, init_g)
  Sigma_e_0 <-  matrix(runif(Tob*k),  Tob, k)
  ste <- matrix(rnorm(Tob*k), k, Tob)

  l <- LikelihoodGARCHu(parameter_ini_univ[1, ],  est = ste[1, ], Sigma1 = Sigma_e_0[1, 1], Tob = Tob)

  expect_equal(round(l,3), 147.243)
})

test_that("Univariate GARCH likelihood 5dims works", {
  set.seed(23211)
  Tob <- 200
  k <- 5

  init_gamma <- runif(k)
  init_g <- rep(NA, k)
  test_g <- NA
  for(i in 1:k){
    test_g <- runif(1)
    if(init_gamma[i] + test_g < 1){
      init_g[i] <- test_g
    }else{
      while(init_gamma[i] + test_g > 1){
        test_g <- runif(1)
      }
      init_g[i] <- test_g
    }
  }
  parameter_ini_univ <- cbind(init_gamma, init_g)
  Sigma_e_0 <-  matrix(runif(Tob*k),  Tob, k)
  ste <- matrix(rnorm(Tob*k), k, Tob)

  l <- LikelihoodGARCHu(parameter_ini_univ[1, ], Sigma1 = Sigma_e_0[1, 1], est = ste[1, ], Tob = Tob)

  expect_equal(round(l,3), 119.634)
})

test_that("Multivariate GARCH likelihood 3dims works unrestricted", {
  set.seed(123)

  # Generating paramater
  parameter <- runif(9)
  SigmaE <- matrix(runif(3*200), 200, 3)
  Tob <- 200
  k <- 3
  u <- matrix(rnorm(3*200), 200, 3)
  RestrictionMatrix <- matrix(NA, 3, 3)
  restrictions <- 0

  l <- LikelihoodGARCHm(parameter, SigmaE, Tob, k, u,
                   RestrictionMatrix, restrictions)

  expect_equal(round(l,1), 130620.3)
})

test_that("Multivariate GARCH likelihood 3dims works restricted", {
  set.seed(123)

  # Generating paramater
  parameter <- runif(9)
  SigmaE <- matrix(runif(3*200), 200, 3)
  Tob <- 200
  k <- 3
  u <- matrix(rnorm(3*200), 200, 3)
  RestrictionMatrix <- matrix(NA, 3, 3)
  RestrictionMatrix[1, 2:3] <- 0
  RestrictionMatrix[2, 3] <- 0
  restrictions <- 3

  l <- LikelihoodGARCHm(parameter, SigmaE, Tob, k, u,
                        RestrictionMatrix, restrictions)

  expect_equal(round(l), 1334864)
})

test_that("Multivariate GARCH likelihood 5dims works unrestricted", {
  set.seed(123)

  # Generating paramater
  parameter <- runif(25)
  SigmaE <- matrix(runif(5*200), 200, 5)
  Tob <- 200
  k <- 5
  u <- matrix(rnorm(5*200), 200, 5)
  RestrictionMatrix <- matrix(NA, 5, 5)
  restrictions <- 0

  l <- LikelihoodGARCHm(parameter, SigmaE, Tob, k, u,
                        RestrictionMatrix, restrictions)

  expect_equal(round(l,1), 313771.9)
})

test_that("Multivariate GARCH likelihood 5dims works unrestricted", {
  set.seed(123)

  # Generating paramater
  parameter <- runif(25)
  SigmaE <- matrix(runif(5*200), 200, 5)
  Tob <- 200
  k <- 5
  u <- matrix(rnorm(5*200), 200, 5)
  RestrictionMatrix <- matrix(NA, 5, 5)
  RestrictionMatrix[1, 2:5] <- 0
  RestrictionMatrix[2, 3:5] <- 0
  RestrictionMatrix[3, 4:5] <- 0
  RestrictionMatrix[4, 5] <- 0
  restrictions <- 10

  l <- LikelihoodGARCHm(parameter, SigmaE, Tob, k, u,
                        RestrictionMatrix, restrictions)

  expect_equal(round(l), 15329867)
})
