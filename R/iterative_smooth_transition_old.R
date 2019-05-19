#======================#
## iterative approach ## (Old version solely with R)
#======================#

iterative_smooth_transition_old <- function(transition, u, y, Tob, k, p, crit, max.iter, Z_t, y_loop, restriction_matrix){

  # Function to create a block diagonal matrix
  block.diagonal<-function(...){
    matrixList <- list(...)
    if(is.list(matrixList[[1]])) matrixList<-matrixList[[1]]

    dimensions <- sapply(matrixList,FUN=function(x) dim(x)[1])
    finalDimension <- sum(dimensions)
    finalMatrix <- matrix(0,nrow=finalDimension,ncol=finalDimension)
    index <- 1
    for(k in 1:length(dimensions)){
      finalMatrix[index:(index+dimensions[k]-1),index:(index+dimensions[k]-1)]<-matrixList[[k]]
      index <- index+dimensions[k]
    }
    finalMatrix
  }

  count <- 0 # count variable
  Exit <-  -100  #Exit criterion

  # Creating initial values for structural parameter
  Sigma_hat <- crossprod(u)/(Tob-1-k*p)

  #init_B <- t(chol(Sigma_hat))
  if(!is.null(restriction_matrix)){
    init_B <- t(chol(Sigma_hat))
    B_hat <- list(init_B)
    naElements <- is.na(restriction_matrix)
    init_B <- init_B[naElements]
    restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
  }else{
    init_B <- t(chol(Sigma_hat))
    B_hat <- list(init_B)
    restrictions <- 0
  }
  #init_B <- suppressMessages(expm::sqrtm(Sigma_hat))
  init_Lambda <- diag(k)

  #B_hat <- list(init_B)
  Lambda_hat <- list(init_Lambda)
  ll <- list(likelihood_st(parameter = c(init_B, diag(init_Lambda)), u = u, G = transition, k = k, Tob = Tob,
                          restriction_matrix = restriction_matrix, restrictions = restrictions))

  while( (abs(Exit) > crit) & (count < max.iter) ){
    count <- count + 1

    if(count == 1){
      u_gls <- u
    }

    if(!is.null(restriction_matrix)){
      init_B <- B_hat[[count]]
      naElements <- is.na(restriction_matrix)
      init_B <- init_B[naElements]
    }else{
      init_B <- B_hat[[count]]
    }
    #init_B <- suppressMessages(expm::sqrtm(Sigma_hat))
    parameter <- c(init_B, diag(Lambda_hat[[count]]))

    # Step 1: Optimizing likelihood
    mle <- nlm(f = likelihood_st, p = parameter, u = u_gls, G = transition, k = k, Tob = Tob,
               restriction_matrix = restriction_matrix, restrictions = restrictions,
               hessian = T, iterlim = 150)

    if(!is.null(restriction_matrix)){
      naElements <- is.na(restriction_matrix)
      B <- restriction_matrix
      B[naElements] <- mle$estimate[1:sum(naElements)]
      Lambda <- diag(mle$estimate[(sum(naElements) + 1):length(mle$estimate)])
    }else{
      B <- matrix(mle$estimate[1:(k*k)], nrow = k)
      Lambda <- diag(mle$estimate[(k*k+1):(k*k+k)])
      restrictions <- 0
    }

    B_hat <- c(B_hat, list(B))
    Lambda_hat <- c(Lambda_hat, list(Lambda))
    ll <- c(ll, list(mle$minimum))

    if(count == 1){
      hessian <- list(solve(mle$hessian))
    }else{
      hessian <- c(hessian, list(solve(mle$hessian)))
    }


    # Step 2: Reestimation of VAR parameter with GLS
    Omega_i <- lapply(transition, function(x, B, Lambda){solve((1 - x)*tcrossprod(B, B) + x*B%*%tcrossprod(Lambda, B))},
                      B = B_hat[[(count + 1)]], Lambda = Lambda_hat[[(count + 1)]])

    W <- block.diagonal(Omega_i)

    b_gls <- solve(kronecker(Z_t, diag(k))%*%W%*%kronecker(t(Z_t), diag(k)))%*%kronecker(Z_t, diag(k))%*%W%*%c(y_loop)

    if(count == 1){
      GLSE <- list(b_gls)
    }else{
      GLSE <- c(GLSE, list(b_gls))
    }

    u_gls <- c(y_loop) - kronecker(t(Z_t), diag(k))%*%b_gls
    u_gls <- matrix(u_gls, Tob, k, byrow = T)

    if(count > 1){
      Exit <- ll[[count+1]] - ll[[count]]
    }
  }

  # extracting the best estimates
  ll <- unlist(ll)
  ll_best <- ll[which.min(ll)]
  cc <- which.min(ll)
  B_hat <- B_hat[[cc]]
  Lambda_hat <- Lambda_hat[[cc]]
  GLSE <- GLSE[[cc-1]]
  GLSE <- matrix(GLSE, nrow = k)

  # Optaining standard errors
  HESS <- hessian[[cc-1]]
  for(i in 1:nrow(HESS)){
    if(HESS[i,i] < 0){
      HESS[,i] <- -HESS[,i]
    }
  }
  FishObs <- sqrt(diag(HESS))

  if(!is.null(restriction_matrix)){
    unRestrictions = k*k - restrictions
    B.SE <- restriction_matrix
    B.SE[naElements] <- FishObs[1:unRestrictions]
    Lambda.SE <- FishObs[((k*k+1) - restrictions):((k*k+k)-restrictions)]*diag(k)
  }else{
    B.SE <- matrix(FishObs[1:(k*k)], k,k)
    Lambda.SE <- diag(FishObs[(k*k+1):(k*k+k)])
  }

  return(list(
    Lambda = Lambda_hat,    # estimated Lambda matrix (unconditional heteroscedasticity)
    Lambda_SE = Lambda.SE,  # standard errors of Lambda matrix
    B = B_hat,              # estimated B matrix (unique decomposition of the covariance matrix)
    B_SE = B.SE,            # standard errors of B matrix
    Fish = HESS,            # observerd fisher information matrix
    Lik = -ll_best,         # function value of likelihood
    iteration = count,      # number of gls estimations
    A_hat = GLSE            # VAR parameter estimated with gls
  ))
}
