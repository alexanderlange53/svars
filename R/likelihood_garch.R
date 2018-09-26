#=========================================#
## Likelihood for univariate GARCH model ##
#=========================================#

likelihood_garch_uni <- function(parameter, est, Sigma_1, Tob, k, lags){

  gamma  <-  parameter[1]
  g      <-  parameter[2]

  # Checking for input validity
  if(gamma > 0 & g >= 0 & gamma + g < 1){

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
      return(-L)
    }
  }else{
    return(1e25)
  }
}
