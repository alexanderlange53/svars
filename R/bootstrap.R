wild.boot <- function(x, radermacher = FALSE, horizon, nboot){
  # x: vars object
  # B: estimated covariance matrix from true data set
  # radermacher: wether the bootstraop work with radermacher distance
  # horizon: Time horizon for Irf
  # nboot: number of bootstrap replications


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
  obs <- x$obs
  k <- x$K
  B <- x$B

  # calculating covariance from actual VAR
  A <- x$A_hat
  Z <- t(y_lag_cr(y, p)$lags)
  Z <-rbind(rep(1, ncol(Z)), Z)
  u <- t(y[-c(1:p),]) - A %*% Z
  Sigma_u_hat_old <- tcrossprod(u)/(obs - 1 - k * p)

  # creating new error terms
  errors <- list()
  for(i in 1:nboot){
    my <- rnorm(n = ncol(y))
    if (radermacher == TRUE) {
      my <- (my > 0) - (my < 0)
    }
    errors[[i]] <- u * my
  }

  # Bootstrapfunction
  bootf <- function(Ustar1){

    Ystar <- t(A %*% Z + Ustar1)
    varb <- VAR(Ystar, p = p)

    Sigma_u_star <- crossprod(residuals(varb))/(obs - 1 - k * p)

    temp <- id.ngml(varb)
    Pstar <- temp$B

    Pstar1 <- sqrt.f(Pstar, Sigma_u_star)
    diag_sigma_root <- diag(diag(suppressMessages(expm(B))))

    frobP <- frobICA_mod(t(solve(diag_sigma_root)%*%Pstar1), t(solve(diag_sigma_root)%*%B), standardize=TRUE)
    Pstar <- Pstar1%*%frobP$perm

    temp$B <- Pstar

    ip <- imrf(temp, horizon = horizon)
    return(ip)
  }

  bootstraps <- pblapply(errors, bootf)

  ## Impulse response of actual model
  ip <- imrf(x, horizon = horizon)

  return(list(true = ip,
              bootstrap = bootstraps))
}


