identifyVolatility = function(x, SB, Tob = Tob, u_t = u_t, k = k, y = y, restriction_matrix = restriction_matrix,
                                    Sigma_hat1 = Sigma_hat1, Sigma_hat2 = Sigma_hat2, p = p, TB = TB, SBcharacter,
                                     max.iter, crit = crit, yOut = yOut, type = type){


  if(!is.null(restriction_matrix)){
    B <- t(chol((1/Tob)* crossprod(u_t)))
    naElements <- is.na(restriction_matrix)
    B <- B[naElements]
    restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
  }else{
    B <- t(chol((1/Tob)* crossprod(u_t)))
    B <- c(B)
  }
  Lambda <- rep(1, k)
  S <- c(B, Lambda)

  # optimize the likelihood function
  MLE <- nlm(f = LH, p = S, k = k, TB = TB, Sigma_hat1 = Sigma_hat1,
             Sigma_hat2 = Sigma_hat2, Tob = Tob, hessian = T, restriction_matrix = restriction_matrix,
             restrictions = restrictions, iterlim = 150)

  if(!is.null(restriction_matrix)){
    naElements <- is.na(restriction_matrix)
    B_hat <- restriction_matrix
    B_hat[naElements] <- MLE$estimate[1:sum(naElements)]
    Lambda_hat <- diag(MLE$estimate[(sum(naElements) + 1):length(MLE$estimate)])
  }else{
    B_hat <- matrix(MLE$estimate[1:(k*k)], nrow = k)
    Lambda_hat <- diag(MLE$estimate[(k*k+1):(k*k+k)])
    restrictions <- 0
  }
  ll <- MLE$minimum

  # estimating again with GLS to obatin a more precise estimation
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

    yl <- t(y_lag_cr(t(y), p)$lags)
    yret <- y
    y <- y[,-c(1:p)]

    if(x$type == 'const'){
      Z_t <- rbind(rep(1, ncol(yl)), yl)
    }else if(x$type == 'trend'){
      Z_t <- rbind(seq(1, ncol(yl)), yl)
    }else if(x$type == 'both'){
      Z_t <- rbind(rep(1, ncol(yl)), seq(1, ncol(yl)), yl)
    }else{
      Z_t <- yl
    }

  gls1 <- function(Z, Sig){
    G <- kronecker(tcrossprod(Z), Sig)
    return(G)
  }

  resid.gls <- function(Z_t, k, GLS_hat){
    term1 <- kronecker(t(Z_t), diag(k))%*%GLS_hat
    return(term1)
  }

  Lambda_hat <- list(Lambda_hat)
  B_hat <- list(B_hat)
  ll <- list(ll)
  MLEgls_loop <- list(MLE)
  GLSE <- list(NULL)

  counter <- 1
  Exit <- 1

  while(abs(Exit) > crit & counter < max.iter){

    Sig1 <- solve(tcrossprod(B_hat[[counter]]))
    Sig2 <- solve(B_hat[[counter]]%*%tcrossprod(Lambda_hat[[counter]], B_hat[[counter]]))

    GLS1.1 <- rowSums(apply(Z_t[, 1:(TB-1)], 2, gls1, Sig = Sig1))
    GLS1.2 <- rowSums(apply(Z_t[, (TB):ncol(Z_t)], 2, gls1, Sig = Sig2))

    if(x$type == 'none'){
      GLS1 <- solve(matrix(GLS1.1 + GLS1.2, nrow = k*k*p, byrow = F))
      GLS2.1 <- matrix(0, nrow = k*k*p, ncol = (TB-1))
      GLS2.2 <- matrix(0, nrow = k*k*p, ncol = ncol(y))
    }else if(x$type == 'const' | x$type == 'trend'){
      GLS1 <- solve(matrix(GLS1.1 + GLS1.2, nrow = k*k*p+k, byrow = F))
      GLS2.1 <- matrix(0, nrow = k*k*p+k, ncol = (TB-1))
      GLS2.2 <- matrix(0, nrow = k*k*p+k, ncol = ncol(y))
    }else if(x$type == 'both'){
      GLS1 <- solve(matrix(GLS1.1 + GLS1.2, nrow = k*k*p+k+k, byrow = F))
      GLS2.1 <- matrix(0, nrow = k*k*p+k+k, ncol = (TB-1))
      GLS2.2 <- matrix(0, nrow = k*k*p+k+k, ncol = ncol(y))
    }

    for(i in 1:(TB-1)){
      GLS2.1[,i] <- kronecker(Z_t[,i], Sig1)%*%y[,i]
    }
    for(i in TB:ncol(Z_t)){
      GLS2.2[,i] <- kronecker(Z_t[,i], Sig2)%*%y[,i]
    }


    GLS2.1 <- rowSums(GLS2.1)
    GLS2.2 <- rowSums(GLS2.2)
    GLS2 <- GLS2.1 + GLS2.2

    GLS_hat <- GLS1%*%GLS2

    term1 <- apply(Z_t, 2, resid.gls, k = k, GLS_hat = GLS_hat)
    u_tgls <- t(y) - t(term1)

    resid1gls <- u_tgls[1:TB-1,]
    resid2gls <- u_tgls[TB:Tob,]
    Sigma_hat1gls <- (crossprod(resid1gls)) / (TB-1)
    Sigma_hat2gls <- (crossprod(resid2gls)) / (Tob-TB+1)

    # Determine starting values for B and Lambda
    if(!is.null(restriction_matrix)){
      B <- t(chol((1/Tob)* crossprod(u_t)))
      naElements <- is.na(restriction_matrix)
      B <- B[naElements]
      restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
    }else{
      B <- t(chol((1/Tob)* crossprod(u_t)))
      B <- c(B)
    }
    Lambda <- rep(1, k)
    S <- c(B, Lambda)

    #optimize the likelihood function
    MLEgls <- nlm(f = LH, p = S, k = k, TB = TB, Sigma_hat1 = Sigma_hat1gls,
                  Sigma_hat2 = Sigma_hat2gls, Tob = Tob, hessian = T, restriction_matrix = restriction_matrix,
                  restrictions = restrictions, iterlim = 150)

    if(!is.null(restriction_matrix)){
      naElements <- is.na(restriction_matrix)
      B_hatg <- restriction_matrix
      B_hatg[naElements] <- MLEgls$estimate[1:sum(naElements)]
      Lambda_hatg <- diag(MLEgls$estimate[(sum(naElements) + 1):length(MLEgls$estimate)])
    }else{
      B_hatg <- matrix(MLEgls$estimate[1:(k*k)], nrow = k)
      Lambda_hatg <- diag(MLEgls$estimate[(k*k+1):(k*k+k)])
    }
    ll_g <- MLEgls$minimum


    B_hat <- c(B_hat, list(B_hatg))
    Lambda_hat <- c(Lambda_hat, list(Lambda_hatg))
    ll <- c(ll, list(ll_g))
    GLSE <- c(GLSE, list(GLS_hat))
    MLEgls_loop <- c(MLEgls_loop, list(MLEgls))
    counter <- counter + 1

    Exit <- ll[[counter]] - ll[[counter-1]]
  }

  # extracting the best estimates
  ll <- unlist(ll)
  llf <- ll[which.min(ll)]
  cc <- which.min(ll)
  B_hat <- B_hat[[cc]]
  Lambda_hat <- Lambda_hat[[cc]]
  GLSE <- GLSE[[cc]]
  if(!is.null(GLSE)){
    GLSE <- matrix(GLSE, nrow = k)
  }
  MLEgls <- MLEgls_loop[[cc]]

  # obtaining standard errors from inverse fisher information matrix
  HESS <- solve(MLEgls$hessian)

  for(i in 1:nrow(HESS)){
    if(HESS[i,i] < 0){
      HESS[,i] <- -HESS[,i]
    }
  }
  if(!is.null(restriction_matrix)){
    unRestrictions = k*k - restrictions
    FishObs <- sqrt(diag(HESS))
    B.SE <- restriction_matrix
    B.SE[naElements] <- FishObs[1:unRestrictions]
    Lambda.SE <- FishObs[((k*k+1) - restrictions):((k*k+k)-restrictions)]*diag(k)
  }else{
    FishObs <- sqrt(diag(HESS))
    B.SE <- matrix(FishObs[1:(k*k)], k,k)
    Lambda.SE <- diag(FishObs[(k*k+1):(k*k+k)])
  }


  # Testing the estimated SVAR for identification by menas of wald statistic
  wald <- wald.test(Lambda_hat, HESS, restrictions)

result <- list(
  Lambda = Lambda_hat,    # estimated Lambda matrix (unconditional heteroscedasticity)
  Lambda_SE = Lambda.SE,  # standard errors of Lambda matrix
  B = B_hat,              # estimated B matrix (unique decomposition of the covariance matrix)
  B_SE = B.SE,            # standard errors of B matrix
  n = Tob,                # number of observations
  Fish = HESS,            # observerd fisher information matrix
  Lik = -llf,             # function value of likelihood
  wald_statistic = wald,  # results of wald test
  iteration = counter,     # number of gls estimations
  method = "Changes in Volatility",
  SB = SB,                # Structural Break in number format
  A_hat = GLSE,            # VAR parameter estimated with gls
  type = type,          # type of the VAR model e.g 'const'
  SBcharacter = SBcharacter,             # Structural Break in input character format
  restrictions = restrictions, # number of restrictions
  y = yOut,                # Data
    p = unname(p),                # number of lags
  K = k                 # number of time series
)
return(result)

}
