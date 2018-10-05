likelihood_garch_multi <-  function(parameter, Sigma_e, Tob, k, lags, u){

  B_inv <- diag(k)
  B_inv[col(B_inv) != row(B_inv)] <- parameter[1:(k^2-k)]
  B_norm <-  solve(B_inv)

  diag_comp <- diag(parameter[(k^2-k+1):(k^2)])
  # diag_comp is the inverted and squared diagonal elements of B_inv-matrix (i.e. (norm_inv)^2 ) = sqrt(diag_comp)

  # # B = B_norm%*% solve(diag(diag(B_inv),K,K))
  B <- solve(solve(diag_comp)%*%solve(B_norm))

  L <- 0
  for (i in 1 : (Tob - k)){
   L <- L - ( -0.5*log(det(B%*%diag(Sigma_e[i, ])%*% t(B))) -
                0.5*(u[i, ] %*% solve(B%*%diag(Sigma_e[i, ], k, k) %*% t(B)) %*%  u[i, ] ))
  }


  if(is.na(L) == 0){
    return(L) ;
  }else{
    return(1e25) ;
  }

}

library(microbenchmark)
library(ggplot2)
test <- microbenchmark(garch_ll_first_Binv(ini, k = k, Tob = Tob,
                                   Sigma_e = Sigma_e_univ , u = u, lags = p),
               likelihood_garch_multi(ini, k = k, Tob = Tob,
                                   Sigma_e = Sigma_e_univ , u = u, lags = p), times = 50)
autoplot(test)
