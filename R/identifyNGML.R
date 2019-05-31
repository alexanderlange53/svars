identifyNGML <- function(x, coef_x, Sigma_hat, u, k, p, Tob, yOut, type, y,
                         restriction_matrix, stage3 = F, naElements = NULL){
  # choleski decomposition of sigma_u
  B_l <- t(chol(Sigma_hat))
  # standardized choleski decomp
  B_l_st <- B_l%*%solve(diag(diag(B_l)))

  # Creating matrix with off diagonal elemts
  B_hat <- function(beta, k){
    B_hat <- diag(k)
    if(!is.null(restriction_matrix)){
      B_hat[naElements] <- beta
    }else{
      B_hat[row(B_hat)!=col(B_hat)] <- beta
    }
    return(B_hat)
  }

  # starting values
  if(!is.null(restriction_matrix)){
    naElements <- is.na(restriction_matrix)
    beta0 <- B_l_st[row(B_l)!=col(B_l)]
    beta0 <- beta0[-(which(!is.na(restriction_matrix))-floor(which(!is.na(restriction_matrix))/k))]
    restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
    diag(naElements) <- FALSE

    restriction_matrix_optim <- restriction_matrix
  }else{
    beta0 <- B_l_st[row(B_l)!=col(B_l)]
    restrictions <- 0

    restriction_matrix_optim <- matrix(NA, k, k)
  }

  sigma0 <- rep(1,k)
  lambda0 <- rep(5,k)
  theta0 <- c(beta0,sigma0,lambda0)

  # optimizing the likelihood function 2. stage
  maxL <- nlm(p = theta0, f = LikelihoodNGMLStage2, u = u, k = k, RestrictionMatrix = restriction_matrix_optim, Tob = Tob,
              restrictions = restrictions, hessian = TRUE)
  beta_est <- maxL$estimate[1:(k*k-k-restrictions)]

  sigma_est <- maxL$estimate[(k*k-k+1-restrictions):(k*k-restrictions)]
  B_stand_est <- B_hat(beta_est, k)
  B_mle <- B_stand_est%*%diag(sigma_est)
  d_freedom <- maxL$estimate[(k*k+1-restrictions):(k*k+k-restrictions)]
  ll <- maxL$minimum

  # obating standard errors from observed fisher information
  HESS <- solve(maxL$hessian)
  for(i in 1:nrow(HESS)){
    if(HESS[i,i] < 0){
      HESS[,i] <- -HESS[,i]
    }
  }
  if(!is.null(restriction_matrix)){
    unRestrictions = k*k-k - restrictions
    FishObs <- sqrt(diag(HESS))
    B.SE <- restriction_matrix
    B.SE[naElements] <- FishObs[1:unRestrictions]
    diag(B.SE) <- 0
    sigma_SE <- FishObs[(k*k-k+1- restrictions):(k*k- restrictions)]
    d_SE <- FishObs[(k*k+1- restrictions):(k*k+k- restrictions)]
  }else{
    FishObs <- sqrt(diag(HESS))
    B.SE <- matrix(0, k, k)
    B.SE[row(B.SE)!=col(B.SE)] <- FishObs[1:(k*k-k)]
    sigma_SE <- FishObs[(k*k-k+1):(k*k)]
    d_SE <- FishObs[(k*k+1):(k*k+k)]
  }

  # getting variances and covariances for S.E. of non stand B
  # covariance <- HESS[1:(k*k-k),((k*k-k+1):(k*k))]
  # B.SE.2 <- diag(sigma_SE)
  #
  #
  # jj <- 0
  # for(i in 1:k){
  #   for(j in 1:k){
  #     if(i != j){
  #       jj <- jj + 1
  #       B.SE.2[j,i] <- sqrt(2*covariance[jj,i]^2 + (B.SE[j,i]^2 + B_stand_est[j,i]^2)*(sigma_SE[i]^2 + sigma_est[i]^2) -
  #                             (covariance[jj, i] + B_stand_est[j,i] * sigma_est[i])^2)
  #     }
  #   }
  # }

  # Estimating VAR parameter 3. stage
  if(stage3 == TRUE){
    #y <- t(x$y)
    yl <- t(y_lag_cr(t(y), p)$lags)
    y_return <- y

    y <- y[,-c(1:p)]

    A <- matrix(0, nrow = k, ncol = k*p)

    for(i in 1:k){
      A[i,] <- coef_x[[i]][1:(k*p),1]
    }

    if(type == "const"){
      v <- rep(1, k)

      for(i in 1:k){
        v[i] <- coef_x[[i]][(k*p+1), 1]
      }

      A <- cbind(v, A)
      Z_t <- rbind(rep(1, ncol(yl)), yl)
    }else if (type == "trend"){
      trend <- rep(1, k)

      for(i in 1:k){
        trend[i] <- coef_x[[i]][(k*p+1), 1]
      }

      A <- cbind(trend, A)
      Z_t <- rbind(seq(p +1, ncol(y_return)), yl)
    }else if(type == "both"){
      v <- rep(1, k)

      for(i in 1:k){
        v[i] <- coef_x[[i]][(k*p+1), 1]
      }

      trend <- rep(1, k)
      Z_t <- rbind(rep(1, ncol(yl)), seq(p + 1, ncol(y_return)), yl)
      for(i in 1:k){
        trend[i] <- coef_x[[i]][(k*p+2), 1]
      }

      A <- cbind(v, trend, A)
    }else{
      Z_t <- yl
    }

    if(inherits(x, "var.boot")){
      A <- coef_x
    }

    A <- c(A)
    maxL2 <- nlm(p = A, f = LikelihoodNGMLStage3, Z_t = Z_t, Y = y,
                 B_stand_est = B_stand_est, sigma_est = sigma_est,
                 d_freedom = d_freedom, Tob = Tob, k = k, hessian = TRUE)

    A_hat <- matrix(maxL2$estimate, nrow = k)
    y <- y_return
  }else{
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
  }
  rownames(B_mle) <- colnames(u)
  rownames(B_stand_est) <- colnames(u)
  rownames(B.SE) <- colnames(u)

  result <- list(B = B_mle,       # estimated B matrix (unique decomposition of the covariance matrix)
                 #B_SE = B.SE.2,          # standard errors
                 sigma = sigma_est,      # estimated scale of the standardized B
                 sigma_SE = sigma_SE,    # standard errors of the scale
                 df = d_freedom,         # estimated degrees of freedom of the distribution
                 df_SE = d_SE,           # standard errors of the degrees of freedom
                 Fish = HESS,            # observed fisher information matrix
                 A_hat = A_hat,          # estimated VAR parameter
                 B_stand = B_stand_est,  # estimated standardized B matrix
                 B_stand_SE = B.SE ,     # standard errors
                 Lik = -ll,              # value of maximum likelihood
                 method = "Non-Gaussian maximum likelihood",
                 n = Tob,              # number of observations
                 type = type,          # type of the VAR model e.g 'const'
                 y = yOut,             # Data
                 p = p,                # number of lags
                 K = k,                # number of time series
                 restrictions = restrictions,
                 restriction_matrix = restriction_matrix,
                 stage3 = stage3
  )
  return(result)
}
