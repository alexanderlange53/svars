#=========================================#
## Likelihood for univariate GARCH model ##
#=========================================#

likelihood_garch_uni <- function(parameter, est, Sigma_1, Tob, k){

  gamma  <-  parameter[1]
  g      <-  parameter[2]

  # Checking for input validity
  if(gamma > 0.001 & g >= 0.001 & gamma + g < 0.9999){

    # Likelihood function
    Sigma_2 <- t(Sigma_1)
    L <- 0

    for (i in 2:(Tob)){
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

#===========================================#
## Likelihood for multivariate GARCH model ##
#===========================================#

likelihood_garch_multi <-  function(parameter, Sigma_e, Tob, k, u){

  B_inv <- diag(k)
  B_inv[col(B_inv) != row(B_inv)] <- parameter[1: (k^2 - k)]
  B_norm <-  solve(B_inv)

  diag_comp <- diag(parameter[(k^2 - k + 1): (k^2)])
  # diag_comp is the inverted and squared diagonal elements of B_inv-matrix (i.e. (norm_inv)^2 ) = sqrt(diag_comp)

  B <- solve(solve(diag_comp)%*%solve(B_norm))

  L <- 0
  for (i in 1:Tob){
    L <- L - ( -0.5*log(det(B%*%diag(Sigma_e[i, ], k, k) %*% t(B))) -
                 0.5*(u[i, ] %*% solve(B%*%diag(Sigma_e[i, ], k, k) %*% t(B)) %*%  u[i, ]))
  }

  if(!is.na(L)){
    return(L)
  }else{
    return(1e25)
  }

}
