# likelihood functions for ngml procedure

likelihood_ngml_stage2 <-function(theta, u, il, rows) {

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

  if(all(sigma >0) & det(B_hat(beta))>0 & all(lambda > 2)){

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

likelihood_ngml_stage3 <-function(A, Z_t, y, il, B_stand_est, rows, sigma_est,
                                  d_freedom) {

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
