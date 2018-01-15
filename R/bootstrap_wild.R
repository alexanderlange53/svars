#'Wild bootstrap for IRFs of identified SVARs
#'
#'Calculating confidence bands for impulse response functions via wild bootstrap techniques (Goncalves and Kilian, 2004).
#'
#'@param x SVAR object of class "svars"
#'@param rademacher If rademacher="TRUE", the Rademacher distribution is used to generate the bootstrap samples
#'@param horizon Time horizon for impulse response functions
#'@param nboot Number of bootstrap iterations
#'@param nc Number of processor cores (Not available on windows machines)
#'@param dd Object of class 'indepTestDist'. A simulated independent sample of the same size as the data. If not supplied, it will be calculated by the function
#'@param itermax Maximum number of iterations for DEoptim
#'@param steptol Tolerance for steps without improvement for DEoptim
#'@param iter2 Number of iterations for the second optimization
#'
#'@references Goncalves, S., Kilian, L., 2004. Bootstrapping autoregressions with conditional heteroskedasticity of unknown form. Journal of Econometrics 123, 89-120.
#'
#'@seealso \code{\link{id.cvm}}, \code{\link{id.dc}}, \code{\link{id.ngml}} or \code{\link{id.cv}}
#'
#' @examples
#' \donttest{
#' # data contains quarterly observations from 1965Q1 to 2008Q3
#' # x = output gap
#' # pi = inflation
#' # i = interest rates
#' set.seed(23211)
#' v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#' x1 <- id.ngml(v1)
#' summary(x1)
#'
#' # switching columns according to sign patter
#' x1$B <- x1$B[,c(3,2,1)]
#' x1$B[,3] <- x1$B[,3]*(-1)
#'
#' # impulse response analysis with confidence bands
#' bb <- wild.boot(x1, rademacher = T, nboot = 100, horizon = 30)
#' plot(bb, lowerq = 0.16, upperq = 0.84)
#' }
#'
#'
#'@export



wild.boot <- function(x, rademacher = FALSE, horizon, nboot, nc = 1, dd = NULL, itermax = 300, steptol = 200, iter2 = 50){

  # x: vars object
  # B: estimated covariance matrix from true data set
  # rademacher: wether the bootstraop work with rademacher distance
  # horizon: Time horizon for Irf
  # nboot: number of bootstrap replications
  if(x$method == "Cramer-von Mises distance" & is.null(dd)){
    dd <- copula::indepTestSim(x$n, x$K, verbose=F)
  }


  # function to create Z matrix
  y_lag_cr <- function(y, lag_length){
    # create matrix that stores the lags
    y_lag <- matrix(NA, dim(y)[1],dim(y)[2]*lag_length)
    for (i in 1:lag_length) {
      y_lag[(1+i):dim(y)[1],((i*NCOL(y)-NCOL(y))+1):(i*NCOL(y))] <- y[1:(dim(y)[1]-i),(1:NCOL(y))]
    }
    # drop first observation
    y_lag <- as.matrix(y_lag[-(1:lag_length),])
    out <- list(lags = y_lag)
  }

  sqrt.f <- function(Pstar, Sigma_u_star){
    yy <- suppressMessages(sqrtm(Sigma_u_hat_old))%*%solve(suppressMessages(sqrtm(Sigma_u_star)))%*%Pstar
    return(yy)
  }


  # gathering informations from vars object
  y <- x$y
  p <- x$p
  obs <- x$n
  k <- x$K
  B <- x$B

  # calculating covariance from actual VAR
  A <- x$A_hat
  Z <- t(y_lag_cr(y, p)$lags)

  if(x$type == 'const'){
    Z <- rbind(rep(1, ncol(Z)), Z)
  }else if(x$type == 'trend'){
    Z <- rbind(seq(1, ncol(Z)), Z)
  }else if(x$type == 'both'){
    Z <- rbind(rep(1, ncol(Z)), seq(1, ncol(Z)), Z)
  }else{
    Z <- Z
  }

  u <- t(y[-c(1:p),]) - A %*% Z
  Sigma_u_hat_old <- tcrossprod(u)/(obs - 1 - k * p)

  ub <- u

  # creating new error terms
  errors <- list()
  for(i in 1:nboot){
    ub <- u
    my <- rnorm(n = ncol(u))
    #my <- rnorm(1)
    if (rademacher == TRUE) {
      my <- (my > 0) - (my < 0)
    }
    errors[[i]] <- ub* my
  }

  # Bootstrapfunction
  bootf <- function(Ustar1){

    Ystar <- t(A %*% Z + Ustar1)

    Bstar <- t(Ystar) %*% t(Z) %*% solve(Z %*% t(Z))

    Ustar <- t(y[-c(1:p),]) - Bstar %*% Z

    Sigma_u_star <- tcrossprod(Ustar)/(ncol(Ustar1) - 1 - k * p)

    varb <- list(y = y,
                 coef_x = Bstar,
                 residuals = t(Ustar),
                 p = p,
                 type = x$type)
    class(varb) <- 'var.boot'

    if(x$method == "Non-Gaussian maximum likelihood"){
      temp <- id.ngml(varb, stage3 = x$stage3)
    }else if(x$method == "Changes in Volatility"){
      temp <- id.cv(varb, SB = x$SB)
    }else if(x$method == "Cramer-von Mises distance"){
      temp <- id.cvm(varb, itermax = itermax, steptol = steptol, iter2 = iter2, dd)
    }else{
      temp <- id.dc(varb, PIT=x$PIT)
    }

    Pstar <- temp$B

     Pstar1 <- sqrt.f(Pstar, Sigma_u_star)
     diag_sigma_root <- diag(diag(suppressMessages(sqrtm(Sigma_u_hat_old))))

     frobP <- frobICA_mod(t(solve(diag_sigma_root)%*%Pstar1), t(solve(diag_sigma_root)%*%B), standardize=TRUE)
     Pstar <- Pstar1%*%frobP$perm

     temp$B <- Pstar

    ip <- imrf(temp, horizon = horizon)
    return(list(ip, Pstar))
  }

  bootstraps <- pblapply(errors, bootf, cl = nc)

  Bs <- array(0, c(k,k,nboot))
  ipb <- list()
  for(i in 1:nboot){
    Bs[,,i] <- bootstraps[[i]][[2]]
    ipb[[i]] <- bootstraps[[i]][[1]]
  }

  # Calculating Standard errors for LDI methods
  if(x$method == "Cramer-von Mises distance" | x$method == "Distance covariances"){
    SE <- matrix(0,k,k)
    for(i in 1:k){
      for(j in 1:k){
        SE[i,j] <-  sum((Bs[i,j,] - sum(Bs[i,j,])/nboot)^2)/nboot
      }
    }

    SE <- sqrt(SE)
  }else{
    SE <- NULL
  }
  ## Impulse response of actual model
  ip <- imrf(x, horizon = horizon)

  result <- list(true = ip,
                 bootstrap = ipb,
                 SE = SE)
  class(result) <- 'sboot'
  return(result)
}


