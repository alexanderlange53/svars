sigma_garch_univ <-  function(param, Tob, k, Sigma_e, est, p){

  for (i in 1:(Tob - p - 1)){
    Sigma_e[(i+1)] <-  (param[1] + param[2]*(est[i]^2) + param[3] * Sigma_e[i] )
  }

  Sigma_e[1:end(Sigma_e)[1]]
}
