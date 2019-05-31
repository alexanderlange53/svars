context("test-likelihood_st.R")

test_that("Luetkepohl Netsunajev 5*5 example", {

  v1 <- vars::VAR(LN, p = 3, type = 'const')
  u <- residuals(v1)
  # Transition function
  transition_f <- function(gamma, cc, st){
    G <- (1 + exp(-exp(gamma)*(st - cc)))^(-1)
    return(G)
  }
  G_grid <- mapply(transition_f, -2.77, 167, MoreArgs = list(st = seq(1:447)))

  # Parameter from Luetkepohl Netsunajev
  B <- matrix(c(0.213883587513229,	0.772120510734492,	-0.132282532851005,	-0.0318059401299042,	0.183421072804760,
        -0.000909983103775138,	0.0611927897127684,	-0.133387139716194,	0.224482965869132,	-0.178566415153278,
        -0.489608073426655,	1.34883810601586,	3.16440093460292,	1.18136247975495,	-0.349727160207559,
        -0.241998772722667,	1.07756860248053,	0.547129661435694,	-2.40448854722913,	-2.27634107379356,
         0.885487887527691,	0.0288063531310017,	0.0196527566892526,	0.0206577929300702,	0.00150251343596967), nrow = 5, byrow = T)
  Lambda <- c(0.0199927489696526, 0.314911226555606, 0.548190884220239,	0.866994133953794, 0.926892018919112)*diag(5)

  expect_equal(round(LikelihoodST(parameter = c(B, diag(Lambda)), u = u, G = G_grid, k = 5, Tob = 447,
                                  RestrictionMatrix = matrix(NA, 5,5), restrictions = 0), 3),
                     2976.656)
})

test_that("Random 2*2 example works", {

  set.seed(12123)
  u <- matrix(rnorm(400, sd = 1.2), 200, 2)
  # Transition function
  transition_f <- function(gamma, cc, st){
    G <- (1 + exp(-exp(gamma)*(st - cc)))^(-1)
    return(G)
  }
  G_grid <- mapply(transition_f, -2, 100, MoreArgs = list(st = seq(1:200)))

  # Parameter from Luetkepohl Netsunajev
  B <- matrix(c(rnorm(mean = 3, 4)), nrow = 2)
  Lambda <- c(rnorm(mean = 2, 2))*diag(2)

  expect_equal(round(LikelihoodST(parameter = c(B, diag(Lambda)), u = u, G = G_grid, k = 2, Tob = 200,
                                  RestrictionMatrix = matrix(NA, 2, 2), restrictions = 0), 3),
               906.717)
})

test_that("2*2 example with negative variance", {

  set.seed(12123)
  u <- matrix(rnorm(400, sd = 1.2), 200, 2)
  # Transition function
  transition_f <- function(gamma, cc, st){
    G <- (1 + exp(-exp(gamma)*(st - cc)))^(-1)
    return(G)
  }
  G_grid <- mapply(transition_f, -2, 100, MoreArgs = list(st = seq(1:200)))

  # Parameter from Luetkepohl Netsunajev
  B <- matrix(c(rnorm(mean = 3, 4)), nrow = 2)
  Lambda <- c(-2, 2)*diag(2)

  expect_equal(round(LikelihoodST(parameter = c(B, diag(Lambda)), u = u, G = G_grid, k = 2, Tob = 200,
                                  RestrictionMatrix = matrix(NA, 2, 2), restrictions = 0), 3),
               1e25)
})
