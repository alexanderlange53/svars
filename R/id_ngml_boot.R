id.ngml_boot <- function(x, stage3 = FALSE, Z = NULL, restriction_matrix = NULL){

  if(inherits(x, "var.boot")){
    u <- x$residuals
    Tob <- nrow(u)
    k <- ncol(u)
    residY <- u
  }else{
    u <- residuals(x)
    Tob <- nrow(u)
    k <- ncol(u)
    residY <- u
  }

  if(inherits(x, "var.boot")){
    p <- x$p
    y <- t(x$y)
    type = x$type
    coef_x = x$coef_x
  }else if(inherits(x, "varest")){
    p <- x$p
    y <- t(x$y)
    type = x$type
    coef_x = coef(x)
  }else if(inherits(x, "nlVar")){
    p <- x$lag
    y <- t(x$model[, 1:k])
    coef_x <- t(coef(x))

    if(inherits(x, "VECM")){
      coef_x <- t(VARrep(x))
    }

    if(rownames(coef_x)[1] %in% c("Intercept", "constant")){
      coef_x <- coef_x[c(2:nrow(coef_x),1),]

    }else if(rownames(coef_x)[1] == "Trend"){
      coef_x <- coef_x[c(2:nrow(coef_x),1),]
    }
    if(rownames(coef_x)[1] %in% c("Intercept", "constant", "Trend")){
      coef_x <- coef_x[c(2:nrow(coef_x),1),]
    }
    type <- x$include
    coef_x <- split(coef_x, rep(1:ncol(coef_x), each = nrow(coef_x)))
    coef_x <- lapply(coef_x, as.matrix)
  }else if(inherits(x, "list")){
    p <- x$order
    y <- t(x$data)
    coef_x <- x$coef
    if(x$cnst == TRUE){
      coef_x <- coef_x[c(2:nrow(coef_x),1),]
      type = "const"
    }
    coef_x <- split(coef_x, rep(1:ncol(coef_x), each = nrow(coef_x)))
    coef_x <- lapply(coef_x, as.matrix)

  }else if(inherits(x, "vec2var")){
    coef_x <- vector("list", length = k)
    names(coef_x) <- colnames(x$y)
    p <- x$p
    y <- t(x$y)

    for (i in seq_len(k)) {
      for (j in seq_len(p)) coef_x[[i]] <- c(coef_x[[i]], x$A[[j]][i,])
      coef_x[[i]] <- c(coef_x[[i]], x$deterministic[i,])
    }
    coef_x <- lapply(coef_x, matrix)
    type <- "const"

  }else{
    stop("Object class is not supported")
  }

  # calculating the covariance matrix
  Sigma_hat <- crossprod(residY)/(Tob-1-k*p)

  # choleski decomposition of sigma_u
  B_l <- t(chol(Sigma_hat))
  # standardized choleski decomp
  B_l_st <- B_l%*%solve(diag(diag(B_l)))

  # starting values
  if(!is.null(restriction_matrix)){
    naElements <- is.na(restriction_matrix)
    beta0 <- B_l_st[row(B_l)!=col(B_l)]
    beta0 <- beta0[-(which(!is.na(restriction_matrix))-floor(which(!is.na(restriction_matrix))/k))]
    restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
    diag(naElements) <- FALSE
  }else{
    beta0 <- B_l_st[row(B_l)!=col(B_l)]
    restrictions <- 0
  }
  sigma0 <- rep(1,k)
  lambda0 <- rep(5,k)
  theta0 <- c(beta0,sigma0,lambda0)

  # Creating selection matrix for likelihood
  il <- matrix(0, k*k, k*k)
  rows <- rep(0, k)
  for(i in 1:k){
    if(i == 1){
      il[i,i] <- 1
      rows[i] <- 1
    }else{
      il[(i+(k*(i-1))),(i+(k*(i-1)))] <- 1
      rows[i] <- i+(k*(i-1))
    }
  }

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

  # optimizing the likelihood function 2. stage
  maxL <- nlm(p = theta0, f = likelihood_ngml_stage2, u = u, k = k, il = il, rows = rows, restriction_matrix = restriction_matrix,
              restrictions = restrictions,
              hessian = FALSE)
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
      yl <- t(y_lag_cr(t(y), p)$lags)
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
        Z_t <- rbind(seq(1, ncol(yl)), yl)
      }else if(type == "both"){
        v <- rep(1, k)

        for(i in 1:k){
          v[i] <- coef_x[[i]][(k*p+1), 1]
        }

        trend <- rep(1, k)
        Z_t <- rbind(rep(1, ncol(yl)), seq(1, ncol(yl)), yl)
        for(i in 1:k){
          trend[i] <- coef_x[[i]][(k*p+2), 1]
        }

        A <- cbind(v, trend, A)
      }else{
        Z_t <- yl
      }
    }


    A <- c(A)
    maxL2 <- suppressMessages(nlm(p = A, f = likelihood_ngml_stage3, Z_t = Z_t, y = y, il = il,
                 B_stand_est = B_stand_est, rows = rows, sigma_est = sigma_est,
                 d_freedom = d_freedom, k=k, hessian = FALSE))

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
                 stage3 = stage3
  )
  class(result) <- "svars"
  return(result)
}
