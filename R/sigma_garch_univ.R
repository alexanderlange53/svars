sigma_garch_univ <-  function(param, Tob, k, Sigma_e, est){

  for (i in 1:(Tob-1)){
    Sigma_e[(i+1)] <-  param[1] + param[2] * est[i]^2 + param[3] * Sigma_e[i]
  }

  return(Sigma_e)
}
