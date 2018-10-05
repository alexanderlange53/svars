#=========================================#
## Likelihood for univariate GARCH model ##
#=========================================#

likelihood_garch_uni <- function(parameter, est, Sigma_1, Tob, k, lags){

  gamma  <-  parameter[1]
  g      <-  parameter[2]

  # Checking for input validity
  if(gamma > 0.001 & g >= 0.001 & gamma + g < 0.9999){

    # Likelihood function
    Sigma_2 <- t(Sigma_1)
    L <- 0

    for (i in 2:(Tob - p + 1)){
      Sigma_2[i] <-  (1 - g - gamma) + gamma * (est[(i-1)]^2) + g * Sigma_2[(i-1)]
      L <- L + 0.5*(log(Sigma_2[i-1]) + est[i-1]^2 %*% Sigma_2[i-1]^-1)
    }
    if(is.na(L)){
      return(1e25)
    }else{
      return(L)
    }
  }else{
    return(1e25)
  }
}

garch_ll_first_Binv <-  function(parameter, Sigma_e, Tob, k, lags, u){

  B_inv <- diag(k)
  B_inv[col(B_inv) != row(B_inv)] <- parameter[1:(k^2-k)]
  B_norm <-  solve(B_inv)

  diag_comp <- diag(parameter[(k^2-k+1):(k^2)])
  # diag_comp is the inverted and squared diagonal elements of B_inv-matrix (i.e. (norm_inv)^2 ) = sqrt(diag_comp)

  # # B = B_norm%*% solve(diag(diag(B_inv),K,K))
  B <- solve(solve(diag_comp)%*%solve(B_norm))

  L <- 0
  for (i in 1 : (Tob - k)){
    L <- L - ( -0.5*log(det(B%*%diag(Sigma_e[i,(1:k)], k, k)%*% t(B))) -0.5*(u[i, (1:k)] %*% solve(B%*%diag(Sigma_e[i,(1:k)], k, k) %*% t(B)) %*%  u[i, (1:k)] ))
  }

  if(is.na(L) == 0){
    return(-L) ;
  }else{
    return(1e25) ;
  }

}
