id.ngml_boot <- function(x, stage3 = FALSE, Z = NULL, restriction_matrix = NULL){

  u <- Tob <- p <- k <- residY <- coef_x <- yOut <- type <- y <-  NULL
  get_var_objects(x)

  # check if varest object is restricted
  if(inherits(x,"varest")){
    if(!is.null(x$restrictions)){
      stop("id.ngml currently supports identification of unrestricted VARs only. Consider using id.dc, id.cvm or id.chol instead.")
    }
  }

  restriction_matrix = get_restriction_matrix(restriction_matrix, k)
  restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
  # calculating the covariance matrix
  Sigma_hat <- crossprod(residY)/(Tob-1-k*p)

  # choleski decomposition of sigma_u
  B_l <- t(chol(Sigma_hat))
  # standardized choleski decomp
  B_l_st <- B_l%*%solve(diag(diag(B_l)))

  # starting values
  if(restrictions > 0){
    naElements <- is.na(restriction_matrix)
    beta0 <- B_l_st[row(B_l)!=col(B_l)]
    beta0 <- beta0[-(which(!is.na(restriction_matrix))-floor(which(!is.na(restriction_matrix))/k))]
    #restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
    diag(naElements) <- FALSE

    restriction_matrix_optim <- restriction_matrix
  }else{
    beta0 <- B_l_st[row(B_l)!=col(B_l)]
    #restrictions <- 0

    restriction_matrix_optim <- restriction_matrix
  }

  sigma0 <- rep(1,k)
  lambda0 <- rep(5,k)
  theta0 <- c(beta0,sigma0,lambda0)

  # Creating matrix with off diagonal elemts
  B_hat <- function(beta, k){
    B_hat <- diag(k)
    if(restrictions > 0){
      B_hat[naElements] <- beta
    }else{
      B_hat[row(B_hat)!=col(B_hat)] <- beta
    }
    return(B_hat)
  }

  # optimizing the likelihood function 2. stage
  maxL <- suppressWarnings(nlm(p = theta0, f = LikelihoodNGMLStage2, u = u, k = k, RestrictionMatrix = restriction_matrix_optim, Tob = Tob,
              restrictions = restrictions, hessian = TRUE))
  beta_est <- maxL$estimate[1:(k*k-k-restrictions)]

  sigma_est <- maxL$estimate[(k*k-k+1-restrictions):(k*k-restrictions)]
  B_stand_est <- B_hat(beta_est, k)
  B_mle <- B_stand_est%*%diag(sigma_est)
  d_freedom <- maxL$estimate[(k*k+1-restrictions):(k*k+k-restrictions)]
  ll <- maxL$minimum

  # Estimating VAR parameter 3. stage
  if(stage3 == TRUE){

    if(!is.null(Z)){
      A <- coef_x
      Z_t <- Z
    }else{
      A <- matrix(0, nrow = k, ncol = k*p)

      for(i in 1:k){
        A[i,] <- coef_x[[i]][1:(k*p),1]
      }
      yl <- t(YLagCr(t(y), p))
      y <- y[,-c(1:p)]

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
        Z_t <- rbind(seq(p + 1, Tob), yl)
      }else if(type == "both"){
        v <- rep(1, k)

        for(i in 1:k){
          v[i] <- coef_x[[i]][(k*p+1), 1]
        }

        trend <- rep(1, k)
        Z_t <- rbind(rep(1, ncol(yl)), seq(p + 1, Tob + p), yl)
        for(i in 1:k){
          trend[i] <- coef_x[[i]][(k*p+2), 1]
        }

        A <- cbind(v, trend, A)
      }else{
        Z_t <- yl
      }
    }


    A <- c(A)
    maxL2 <- suppressWarnings(nlm(p = A, f = LikelihoodNGMLStage3, Z_t = Z_t, Y = y,
                                  B_stand_est = B_stand_est, sigma_est = sigma_est,
                                  d_freedom = d_freedom, Tob = Tob, k = k, hessian = TRUE))

    A_hat <- matrix(maxL2$estimate, nrow = k)
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

  result <- list(B = B_mle,       # estimated B matrix (unique decomposition of the covariance matrix)
                 sigma = sigma_est,      # estimated scale of the standardized B
                 df = d_freedom,         # estimated degrees of freedom of the distribution
                 A_hat = A_hat,          # estimated VAR parameter
                 B_stand = B_stand_est,  # estimated standardized B matrix
                 Lik = -ll,              # value of maximum likelihood
                 method = "Non-Gaussian maximum likelihood",
                 n = Tob,              # number of observations
                 type = type,            # type of the VAR model e.g 'const'
                 y = t(y),                # Data
                 p = p,                # number of lags
                 restrictions = restrictions, # number of restrictions
                 K = k,                # number of time series
                 stage3 = stage3,
                 VAR = x
  )
  class(result) <- "svars"
  return(result)
}
