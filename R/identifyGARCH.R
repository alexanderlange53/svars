identifyGARCH <- function(B0, k, Tob, restriction_matrix, Sigma_e_univ, parameter_ini_univ, max.iter, crit, u, p, x, type, yOut, coef_x){
  ## Stage 2: Multivariate optimization

   if(!is.null(restriction_matrix)){
     naElements <- is.na(restriction_matrix)
     naElements_inv <- !is.na(restriction_matrix)
     ini <- B0[naElements]
     restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
   }else{
     restrictions <- 0
     ini <- c(B0)
  }

  # create empty vectors and lists for results
  gamma <- rep(NA, k)
  g <-  rep(NA, k)
  param <-  rep(NA, k)
  results_B <- list()
  results_param <- list()
  round <-  1
  Exit <- 1

  #B_inv <- diag(k)
  ll <- list()
  multi_ml <- list()
  uni_ml <- list()
  uni_single_ml <- list()


  while (Exit > crit & round < max.iter){
    max_ml <- nlm(ini, f = likelihood_garch_multi, k = k, Tob = Tob, restriction_matrix = restriction_matrix,
                  Sigma_e = Sigma_e_univ , u = u, iterlim = 150, hessian = T)


    multi_ml[[round]] <- max_ml
    # initials for next round of univariate estimation
    ini <- max_ml$estimate

     if(!is.null(restriction_matrix)){
       naElements <- is.na(restriction_matrix)
       B_est <- restriction_matrix
       B_est[naElements] <- ini[1:sum(naElements)]
     }else{
       B_est <- matrix(ini, k, k)
     }

    B_est_inv <- solve(B_est)
    # save individual B matrices for each round
    results_B[[round]] <- B_est
    # calculate new structural residuals for update of the GARCH parameters
    est_r <- B_est_inv %*% t(u)

    # Evaluating exit criterion
    ll[[round]] <- max_ml$minimum
    if(round > 1){
      Exit <- abs(ll[[round-1]] - max_ml$minimum)
    }

    # re-estimate GARCH part, based on update of estimate of B
    # optimizing the univariate likelihood functions
    maxL <- list()
    gamma_univ <- rep(NA, k)
    g_univ <- rep(NA, k)
    param_univ <- matrix(NA, 3, k)
    Sigma_e_univ <- matrix(NA, Tob, k)

    # first observstion of strucutral variance is the estimated sample variance
    Sigma_e_0 <-  matrix(diag(var(t(est_r))),  Tob, k, byrow = T)

    for(i in 1:k){
      maxL <- nlm(p = parameter_ini_univ[i, ], f = likelihood_garch_uni, k = k, Tob = Tob,
                  Sigma_1 = Sigma_e_0[, i] , est = est_r[i, ], iterlim = 150, hessian = T)
      uni_single_ml[[i]] <- maxL$hessian

      gamma_univ[i] <- maxL$estimate[1]
      g_univ[i] <- maxL$estimate[2]

      param_univ[, i] <- rbind((1- gamma_univ[i]- g_univ[i]), gamma_univ[i], g_univ[i])
      Sigma_e_univ[,i] <- sigma_garch_univ(param_univ[,i], Tob, Sigma_e_0[,i], est_r[i,])
    }

    uni_ml[[round]] <- uni_single_ml
    results_param[[round]] <- parameter_ini_univ <-  cbind(gamma_univ, g_univ)
    round <-  round + 1
  } # end of while loop

  # extracting the best estimates
  ll <- unlist(ll)
  llf <- ll[which.min(ll)]
  # Calculate log likelihood with normalizing constant
  llf <- log(sqrt(1/(2 * pi)^k)) * Tob - llf
  cc <- which.min(ll)
  B_hat <- results_B[[cc]]
  GARCH_param_hat <- results_param[[cc]]

  # Standard errors
  multi_ml <- multi_ml[[cc]]
  HESS <- solve(multi_ml$hessian)

  uni_ml <- uni_ml[[cc]]
  HESS_univ <- tryCatch(lapply(uni_ml, function(x) diag(solve(x))), error = function(e) NA)
  if(any(is.na(HESS_univ))){
    GARCH_SE <- NA
  }else{
    GARCH_SE <- do.call('rbind', HESS_univ)
  }


  for(i in 1:nrow(HESS)){
    if(HESS[i,i] < 0){
      HESS[,i] <- -HESS[,i]
    }
  }

  FishObs <- sqrt(diag(HESS))
  B_inv_SE <- matrix(NA, k, k)

  if(!is.null(restriction_matrix)){
    naElements <- is.na(restriction_matrix)
    B_inv_SE <- restriction_matrix
    B_inv_SE[naElements] <- FishObs[1:sum(naElements)]

    #B_inv_diag_SE <- diag(FishObs[(sum(naElements) + 1):(sum(naElements) + k)])
  }else{
    B_inv_SE <- matrix(FishObs, k, k)
    # diag(B_inv_SE) <- 0
    # B_inv_diag_SE <- diag(FishObs[(k * k - k + 1):(k * k)])
  }

  rownames(B_hat) <- colnames(u)
  rownames(B_inv_SE) <- colnames(u)
  if(all(!is.na(GARCH_SE))){
    colnames(GARCH_SE) <- colnames(GARCH_param_hat) <- c('gamma', 'g')
  }

  # obtaining VAR parameter
  if(inherits(x, "var.boot")){
    A_hat <- coef_x
  }else{
    A <- matrix(0, nrow = k, ncol = k*p)
    for(i in 1:k){
      A[i,] <- coef_x[[i]][1:(k*p),1]
    }

    A_hat <- A

    if(type == "const"){
      v <- rep(1, k)

      for(i in 1:k){
        v[i] <- coef_x[[i]][(k*p+1), 1]
      }

      A_hat <- cbind(v, A)
    }else if (type == "trend"){
      trend <- rep(1, k)

      for(i in 1:k){
        trend[i] <- coef_x[[i]][(k*p+1), 1]
      }

      A_hat <- cbind(trend, A)
    }else if(type == "both"){
      v <- rep(1, k)

      for(i in 1:k){
        v[i] <- coef_x[[i]][(k*p+1), 1]
      }

      trend <- rep(1, k)

      for(i in 1:k){
        trend[i] <- coef_x[[i]][(k*p+2), 1]
      }

      A_hat <- cbind(v, trend, A)
    }
  }

  return(list(
    B = B_hat,              # estimated B matrix (unique decomposition of the covariance matrix)
    B_SE = B_inv_SE,            # standard errors of B matrix
    GARCH_parameter = GARCH_param_hat,
    GARCH_SE  = GARCH_SE,
    n = Tob,                # number of observations
    Fish = HESS,            # observerd fisher information matrix
    Lik = llf,             # function value of likelihood
    iteration = round,     # number of gls estimations
    method = "GARCH",
    restrictions = restrictions, # number of restrictions
    Sigma_e = Sigma_e_univ,
    A_hat = A_hat,            # VAR parameter estimated with gls
    type = type,          # type of the VAR model e.g 'const'
    y = yOut,                # Data
    p = unname(p),                # number of lags
    K = k                 # number of time series
  ))
}
