#=========================================#
## Likelihood for univariate GARCH model ##
#=========================================#

likelihood_garch_uni <- function(parameter, est, Sigma_1, Tob, k){

  gamma  <-  parameter[1]
  g      <-  parameter[2]

  # Checking for input validity
  if(gamma > 0.001 & g >= 0.001 & gamma + g < 0.999){

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

likelihood_garch_multi <-  function(parameter, Sigma_e, Tob, k, u, restriction_matrix){

   if(!is.null(restriction_matrix)){
     if(!is.matrix(restriction_matrix)){
       stop("Please provide a valid input matrix")
     }
     naElements <- is.na(restriction_matrix)

     B <- restriction_matrix
     B[naElements] <- parameter[1:sum(naElements)]
   }else{
     B <- matrix(parameter, k, k)
   }

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
