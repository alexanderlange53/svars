id.ngml_boot <- function(x, stage3 = FALSE, Z = NULL){


  # likelihood function to optimize
  loglik <-function(theta) {

    Tob <- nrow(u)
    K <- ncol(u)

    beta <- theta[1:(K*(K-1))]
    sigma <- theta[(K*(K-1)+1):K^2]
    lambda <- theta[(K^2+1):length(theta)]

    B_hat <- function(beta){
      B_hat <- diag(K)
      B_hat[row(B_hat)!=col(B_hat)] <- beta
      return(B_hat)
    }

    if(all(sigma > 0) & det(B_hat(beta)) > 0.1 & all(lambda > 2)){

      logl <- rep(0, Tob)

      mid <- il %*% kronecker(diag(K), solve(B_hat(beta)))
      midd <- mid[rows, ]
      mid1 <- matrix(0, nrow = K, ncol = K)
      for(i in 1:K){
        if(i == 1){
          mid1[1, 1:K] <- midd[1, 1:K]
        }else{
          mid1[i, 1:K] <- midd[i, ((K+1+K*(i-2)):(i*K))]
        }
      }

      l_t <- function(uu, sigma, lambda, mid1, beta){
        l <-  sum(log(dt( ((sigma)^(-1))*sqrt(lambda/(lambda - 2)) * (mid1 %*% uu), lambda ))) - log(det(B_hat(beta))) - sum(log(sigma*sqrt((lambda - 2)/lambda)))
        return(l)
      }

      logl <- sum(apply(X = u, MARGIN = 1, FUN = l_t, sigma = sigma, lambda = lambda, mid1 = mid1, beta = beta))

      return(-logl)

    } else {
      return(1e25)
    }
  }

  resid.ls <- function(Z_t, k, A){
    term1 <- kronecker(t(Z_t), diag(k))%*%A
    return(term1)
  }

  loglik2 <-function(A) {

    logl <- rep(0, Tob)

    term1 <- apply(Z_t, 2, resid.ls, k = k, A = A)
    uu <- t(y) - t(term1)

    mid <- il %*% kronecker(diag(k), solve(B_stand_est))
    midd <- mid[rows, ]
    mid1 <- matrix(0, nrow = k, ncol = k)
    for(i in 1:k){
      if(i == 1){
        mid1[1, 1:k] <- midd[1, 1:k]
      }else{
        mid1[i, 1:k] <- midd[i, ((k+1+k*(i-2)):(i*k))]
      }
    }

    l_t <- function(uu, mid1){
      l <-  sum(log( dt(((sigma_est)^(-1))*sqrt(d_freedom/(d_freedom - 2)) * (mid1 %*% uu), d_freedom ))) - log(det(B_stand_est)) - sum(log(sigma_est*sqrt((d_freedom - 2)/d_freedom)))
      return(l)
    }

    logl <- sum(apply(X = uu, MARGIN = 1, FUN = l_t, mid1 = mid1))

    return(-logl)
  }

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

  ########### starting the computations ------------------------------------------------------------------------


  # if(is.null(residuals(x))){
  #   stop("No residuals retrieved from model")
  # }
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
  beta0 <- B_l_st[row(B_l)!=col(B_l)]
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

  # optimizing the likelihood function 2. stage
  maxL <- nlm(p = theta0, f = loglik, hessian = FALSE)
  beta_est <- maxL$estimate[1:(k*k-k)]

  sigma_est <- maxL$estimate[(k*k-k+1):(k*k)]
  B_stand_est <- diag(k)
  B_stand_est[row(B_stand_est)!=col(B_stand_est)] <- beta_est
  B_mle <- B_stand_est%*%diag(sigma_est)
  d_freedom <- maxL$estimate[(k*k+1):(k*k+k)]
  ll <- maxL$minimum

  # Estimating VAR parameter 3. stage
  if(stage3 == TRUE){
    A <- coef_x

    if(!is.null(Z)){
      Z_t <- Z
    }else{
      yl <- t(y_lag_cr(t(y), p)$lags)
      y <- y[,-c(1:p)]

      if(type == "const"){
        v <- coef_x[, 1]
        A <- cbind(v, A)
        Z_t <- rbind(rep(1, ncol(yl)), yl)
      }else if (type == "trend"){
        trend <- coef_x[, 1]
        A <- cbind(trend, A)
        Z_t <- rbind(seq(1, ncol(yl)), yl)
      }else if(type == "both"){
        v <- coef_x[, 1]

        trend <- coef_x[, 2]
        A <- cbind(v, trend, A)
      }else{
        Z_t <- yl
      }
    }


    A <- c(A)
    maxL2 <- nlm(p = A, f = loglik2, hessian = FALSE)

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
                 K = k,                # number of time series
                 stage3 = stage3
  )
  class(result) <- "svars"
  return(result)
}
