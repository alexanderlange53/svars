wild.boot <- function(x, B, radermacher = FALSE, horizon, nboot){
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
    yy <- sqrtm(Sigma_u_hat_old)%*%solve(sqrtm(Sigma_u_star))%*%Pstar
    return(yy)
  }


  # gathering informations from vars object
  y <- x$y
  p <- x$p
  obs <- x$obs
  k <- x$K

  # calculating covariance from actual VAR
  Sigma_u_hat_old <- 1 / crossprod(residuals(x))/(obs - 1 - k * p)

  # creating new error terms
  errors <- list()
  for(i in 1:nboot){
    my <- rnorm(n = ncol(y))
    if (radermacher == TRUE) {
      my <- (my > 0) - (my < 0)
    }
    errors[[i]] <- residuals(x) * my
  }

  bootf <- function(Ustar1){
    Z <- t(y_lag_cr(y, p)$lags)
    Z <-rbind(rep(1, ncol(Z)), Z)
    A <- matrix(0, nrow = k, ncol = k*p)
    for(i in 1:k){
      A[i,] <- coef(x)[[i]][1:(k*p),1]
    }
    if(x$type == 'const'){
      v <- rep(1, k)
      for(i in 1:k){
        v[i] <- coef(x)[[i]][(k*p+1), 1]
      }
      A <- cbind(v, A)
    }

    Ystar <- A %*% Z + t(Ustar1)
    Bstar <- Ystar %*% t(Z) %*% solve(Z %*% t(Z))
    Ustar <- t(y[-c(1:p),]) - Bstar %*% Z
    Sigma_u_star <- 1 / (obs - k * p)  * tcrossprod(Ustar)

    temp <- LD4 <- LDIw(Ustar, p = p, iter = 30, t0 = 1500, nlimit = 150, t_min = 0.0001, r = 0.6, matrices = 60,
                        lower = TRUE, upper = F, symmetric = T, cores = 12, dd)
    Pstar <- temp$B


    Pstar1 <- lapply(Pstar, sqrt.f, Sigma_u_star = Sigma_u_star)
    diag_sigma_root <- diag(diag(expm(B)))

    frobP <- lapply(Pstar1, function(x) frobICA_mod(t(solve(diag_sigma_root)%*%x), t(solve(diag_sigma_root)%*%B), standardize=TRUE))
    frobs <- sapply(frobP, '[[', 'frob_dist')
    frobmin <- frobP[[which.min(frobs)]]
    Pstar <- Pstar1[[which.min(frobs)]]%*%frobmin$perm

    VARBoot <- VAR(t(Ystar), p = p)
    A_hat <- matrix(0, nrow = k, ncol = k*p)
    for(i in 1:k){
      A_hat[i,] <- coef(VARBoot)[[i]][1:(k*p),1]
    }
    #if(x$type == 'const'){
    v <- rep(1, k)
    for(i in 1:k){
      v[i] <- coef(VARBoot)[[i]][(k*p+1), 1]
    }
    A_hat <- cbind(v, A_hat)

    ngml <- list(B = Pstar,
                 A_hat = A_hat,
                 type = 'const')

    ip <- irf(ngml, horizon = horizon)
    # ip$irf[,2:5] <- apply(ip$irf[,2:5], 2,cumsum)
    # ip$irf[,14:17] <- apply(ip$irf[,14:17], 2,cumsum)
    return(ip)
  }

  bootstraps <- pblapply(errors, bootf)

  ## Impulse response
  A <- matrix(0, nrow = k, ncol = k*p)
  for(i in 1:k){
    A[i,] <- coef(x)[[i]][1:(k*p),1]
  }
  #if(x$type == 'const'){
  v <- rep(1, k)
  for(i in 1:k){
    v[i] <- coef(x)[[i]][(k*p+1), 1]
  }
  A <- cbind(v, A)

  ngml <- list(B = B,
               A_hat = A,
               type = 'const')

  ip <- irf(ngml, horizon = horizon)
  # ip$irf[,2:5] <- apply(ip$irf[,2:5], 2,cumsum)
  # ip$irf[,14:17] <- apply(ip$irf[,14:17], 2,cumsum)

  return(list(true = ip,
              bootstrap = bootstraps))
}


