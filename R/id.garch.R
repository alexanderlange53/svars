#' GARCH identification of SVAR models
#'
#' Given an estimated VAR model, this function applies changes in volatility to identify the structural impact matrix B of the corresponding SVAR model
#' \deqn{y_t=c_t+A_1 y_{t-1}+...+A_p y_{t-p}+u_t
#' =c_t+A_1 y_{t-1}+...+A_p y_{t-p}+B \epsilon_t.}
#' Matrix B corresponds to the decomposition of the pre-break covariance matrix \eqn{\Sigma_1=B B'}.
#' The post-break covariance corresponds to \eqn{\Sigma_2=B\Lambda B'} where \eqn{\Lambda} is the estimated unconditional heteroskedasticity matrix.
#'
#' @param x An object of class 'vars', 'vec2var', 'nlVar'. Estimated VAR object
#' @param restriction_matrix Matrix. A matrix containing presupposed entries for matrix B, NA if no restriction is imposed (entries to be estimated)
#' @param max.iter Integer. Number of maximum GLS iterations
#' @param crit Integer. Critical value for the precision of the GLS estimation
#' @return A list of class "svars" with elements
#' \item{Lambda}{Estimated unconditional heteroscedasticity matrix \eqn{\Lambda}}
#' \item{Lambda_SE}{Matrix of standard errors of Lambda}
#' \item{B}{Estimated structural impact matrix B, i.e. unique decomposition of the covariance matrix of reduced form residuals}
#' \item{B_SE}{Standard errors of matrix B}
#' \item{n}{Number of observations}
#' \item{Fish}{Observed Fisher information matrix}
#' \item{Lik}{Function value of likelihood}
#' \item{iteration}{Number of GLS estimations}
#' \item{method}{Method applied for identification}
#' \item{A_hat}{Estimated VAR paramter via GLS}
#' \item{type}{Type of the VAR model, e.g. 'const'}
#' \item{restrictions}{Number of specified restrictions}
#' \item{restriction_matrix}{Specified restriction matrix}
#' \item{y}{Data matrix}
#' \item{p}{Number of lags}
#' \item{K}{Dimension of the VAR}
#'
#' @references Rigobon, R., 2003. Identification through Heteroskedasticity. The Review of Economics and Statistics, 85, 777-792.\cr
#'  Herwartz, H. & Ploedt, M., 2016. Simulation Evidence on Theory-based and Statistical Identification under Volatility Breaks Oxford Bulletin of Economics and Statistics, 78, 94-112.
#'
#' @seealso For alternative identification approaches see \code{\link{id.st}}, \code{\link{id.cvm}}, \code{\link{id.dc}} or \code{\link{id.ngml}}
#'
#' @examples
#' \donttest{
#' # data contains quartlery observations from 1965Q1 to 2008Q2
#' # assumed structural break in 1979Q3
#' # x = output gap
#' # pi = inflation
#' # i = interest rates
#' set.seed(23211)
#' v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#' x1 <- id.garch(v1)
#' summary(x1)
#'
#' # Impulse response analysis
#' i1 <- irf(x1, n.ahead = 30)
#' plot(i1, scales = 'free_y')
#'
#' # Restrictions
#' # Assuming that the interest rate doesn't influence the output gap on impact
#' restMat <- matrix(rep(NA, 9), ncol = 3)
#' restMat[1,3] <- 0
#' x2 <- id.garch(v1, restriction_matrix = restMat)
#' summary(x2)
#'
#'
#' }
#' @export


#----------------------------#
## Identification via GARCH ##
#----------------------------#

id.garch <- function(x, max.iter = 5, crit = 0.001, restriction_matrix = NULL){

  u <- Tob <- p <- k <- residY <- coef_x <- yOut <- type <- y <-  NULL
  get_var_objects(x)

  sigg <- crossprod(u)/(Tob-1-k*p)

  # Polar decomposition as in Lanne Saikkonen
  eigVectors <- eigen(sigg)$vectors
  eigValues <- diag(eigen(sigg)$values)
  eigVectors <- eigVectors[,rev(1:ncol(eigVectors))]
  eigValues <- diag(rev(eigen(sigg)$values))

  CC <- eigVectors %*% sqrt(eigValues) %*% t(eigVectors)
  B0 <- solve(solve(sqrt(eigValues)) %*% t(eigVectors))

  # Initial structural erros
  ste <- solve(sqrt(eigValues)) %*% t(eigVectors) %*% t(u)

  ## Stage 1: Univariate optimization of GARCH(1, 1) parameter
  # Initial values as in Luetkepohl + Schlaak (2018)
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
        if(init_gamma[i] + test_g < 1){
          init_g[i] <- test_g
        }
      }
    }
  }
  parameter_ini_univ <- cbind(init_gamma, init_g)

  # first observstion of strucutral variance is the estimated sample variance
  Sigma_e_0 <-  matrix(diag(var(t(ste))),  Tob, k, byrow = T)

  # optimizing the univariate likelihood functions
  maxL <- list()
  gamma_univ <- rep(NA, k)
  g_univ <- rep(NA, k)
  param_univ <- matrix(NA, 3, k)
  Sigma_e_univ <- matrix(NA, Tob, k)

  for(i in 1:k){
    maxL <- nlm(p = parameter_ini_univ[i, ], f = likelihood_garch_uni, k = k, Tob = Tob,
                Sigma_1 = Sigma_e_0[, i] , est = ste[i, ])

    # Optimized GARCH parameter
    gamma_univ[i] <- maxL$estimate[1]
    g_univ[i] <- maxL$estimate[2]
    # Including a constant
    param_univ[, i] <- rbind((1- gamma_univ[i]- g_univ[i]), gamma_univ[i], g_univ[i])
    # estimated conditional heteroskedasticity
    Sigma_e_univ[,i] <- sigma_garch_univ(param_univ[,i], Tob, Sigma_e_0[,i], ste[i,])
  }

  # Store estimtated GARCH parameter as initial values for multivariate optimization
  parameter_ini_univ <- cbind(gamma_univ, g_univ)

  ## Stage 2: Multivariate optimization
  # normalize the inverse of B to have ones on main diagonal
  B_0_inv <- solve(B0)
  norm_inv <- solve(diag(diag(B_0_inv), k, k))
  B_0_inv_norm <-  norm_inv %*% B_0_inv
  B_0_norm <- solve(norm_inv %*% B_0_inv)

  B_param_ini <- B_0_inv_norm [col(B_0_inv_norm ) != row(B_0_inv_norm )]
  diag_elements <- 1/(diag(B_0_inv))

  ini <- c(B_param_ini, diag_elements)

  # create empty vectors and lists for results
  gamma <- rep(NA, k)
  g <-  rep(NA, k)
  param <-  rep(NA, k)
  results_B <- list()
  results_param <- list()
  round <-  1
  Exit <- 1

  B_inv <- diag(k)
  ll <- list()
  multi_ml <- list()
  uni_ml <- list()
  uni_single_ml <- list()


  while (Exit > crit & round < max.iter){
    max_ml <- nlm(ini, f = likelihood_garch_multi, k = k, Tob = Tob,
                  Sigma_e = Sigma_e_univ , u = u, iterlim = 150, hessian = T)

    multi_ml[[round]] <- max_ml
    # initials for next round of univariate estimation
    ini <- max_ml$estimate
    # get the B matrix from the estimated parameters
    B_inv[col(B_inv) != row(B_inv)] <- ini[1:(k^2-k)]
    B <- solve(B_inv)

    diagonal_mat <- diag(ini[(k^2-k+1):k^2])

    B_est <- B %*% diagonal_mat
    B_est_inv <- solve(B_est)
    # save individual B matrices for each round
    results_B[[round]] <- B_est
    # calculate new structural residuals for update of the GARCH parameters
    est_r <- B_est_inv %*% t(u)

    # Evaluating exit criterion
    ll[[round]] <- max_ml$minimum
    if(round > 1){
      Exit <- abs(ll[[round-1]] - max_ml$minimum)
    }

    # re-estimate GARCH part, based on update of estimate of B
    # optimizing the univariate likelihood functions
    maxL <- list()
    gamma_univ <- rep(NA, k)
    g_univ <- rep(NA, k)
    param_univ <- matrix(NA, 3, k)
    Sigma_e_univ <- matrix(NA, Tob, k)

    # first observstion of strucutral variance is the estimated sample variance
    Sigma_e_0 <-  matrix(diag(var(t(est_r))),  Tob, k, byrow = T)

    for(i in 1:k){
      maxL <- nlm(p = parameter_ini_univ[i, ], f = likelihood_garch_uni, k = k, Tob = Tob,
                  Sigma_1 = Sigma_e_0[, i] , est = est_r[i, ], iterlim = 150, hessian = T)
      uni_single_ml[[i]] <- maxL$hessian

      gamma_univ[i] <- maxL$estimate[1]
      g_univ[i] <- maxL$estimate[2]

      param_univ[, i] <- rbind((1- gamma_univ[i]- g_univ[i]), gamma_univ[i], g_univ[i])
      Sigma_e_univ[,i] <- sigma_garch_univ(param_univ[,i], Tob, Sigma_e_0[,i], est_r[i,])
    }

    uni_ml[[round]] <- uni_single_ml
    results_param[[round]] <- parameter_ini_univ <-  cbind(gamma_univ, g_univ)
    round <-  round + 1
  } # end of while loop

  # extracting the best estimates
  ll <- unlist(ll)
  llf <- ll[which.min(ll)]
  # Calculate log likelihood with normalizing constant
  llf <- log(sqrt(1/(2 * pi)^k)) * Tob - llf
  cc <- which.min(ll)
  B_hat <- results_B[[cc]]
  GARCH_param_hat <- results_param[[cc]]

  # Standard errors
  multi_ml <- multi_ml[[cc]]
  HESS <- solve(multi_ml$hessian)

  uni_ml <- uni_ml[[cc]]
  HESS_univ <- lapply(uni_ml, function(x) diag(solve(x)))
  GARCH_SE <- do.call('rbind', HESS_univ)

  for(i in 1:nrow(HESS)){
    if(HESS[i,i] < 0){
      HESS[,i] <- -HESS[,i]
    }
  }

  FishObs <- sqrt(diag(HESS))
  B_inv_SE <- matrix(NA, k, k)
  B_inv_SE[col(B_inv_SE) != row(B_inv_SE)] <- FishObs[1:(k * k - k)]
  B_inv_diag_SE <- diag(FishObs[(k * k - k + 1):(k * k)])

  rownames(B_hat) <- colnames(u)
  rownames(B_inv_SE) <- colnames(u)
  colnames(GARCH_SE) <- colnames(GARCH_param_hat) <- c('gamma', 'g')

  # obtaining VAR parameter
  if(inherits(x, "var.boot")){
    A_hat <- coef_x
  }else{
    A <- matrix(0, nrow = k, ncol = k*p)
    for(i in 1:k){
      A[i,] <- coef_x[[i]][1:(k*p),1]
    }

    A_hat <- A

    if(type == "const"){
      v <- rep(1, k)

      for(i in 1:k){
        v[i] <- coef_x[[i]][(k*p+1), 1]
      }

      A_hat <- cbind(v, A)
    }else if (type == "trend"){
      trend <- rep(1, k)

      for(i in 1:k){
        trend[i] <- coef_x[[i]][(k*p+1), 1]
      }

      A_hat <- cbind(trend, A)
    }else if(type == "both"){
      v <- rep(1, k)

      for(i in 1:k){
        v[i] <- coef_x[[i]][(k*p+1), 1]
      }

      trend <- rep(1, k)

      for(i in 1:k){
        trend[i] <- coef_x[[i]][(k*p+2), 1]
      }

      A_hat <- cbind(v, trend, A)
    }
  }

  result <- list(
    B = B_hat,              # estimated B matrix (unique decomposition of the covariance matrix)
    B_inv_SE = B_inv_SE,            # standard errors of B matrix
    GARCH_parameter = GARCH_param_hat,
    GARCH_SE  = GARCH_SE,
    n = Tob,                # number of observations
    Fish = HESS,            # observerd fisher information matrix
    Lik = llf,             # function value of likelihood
    iteration = round,     # number of gls estimations
    method = "GARCH",
    A_hat = A_hat,            # VAR parameter estimated with gls
    type = type,          # type of the VAR model e.g 'const'
    y = yOut,                # Data
    p = unname(p),                # number of lags
    K = k                 # number of time series
  )
  class(result) <- "svars"

  return(result)

}
