id.st_boot <- function(x, c_fix = NULL, transition_variable = NULL,
                  gamma_fix = NULL, nc = 1, Z,
                  max.iter = 5, crit = 0.01, restriction_matrix = NULL){

  u <- Tob <- p <- k <- residY <- coef_x <- yOut <- type <- y <-  NULL
  get_var_objects(x)

  rmOut = restriction_matrix
  restriction_matrix <- get_restriction_matrix(restriction_matrix, k)
  restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])

  # Transition function
  transition_f <- function(gamma, cc, st){
    G <- (1 + exp(-exp(gamma)*(st - cc)))^(-1)
    return(G)
  }

    grid_comb <- unique(expand.grid(gamma_fix, c_fix))
    if(is.null(transition_variable)){
      G_grid <- mapply(transition_f, grid_comb[,1], grid_comb[,2], MoreArgs = list(st = seq(1:Tob)))
    }else{
      if(length(transition_variable) != Tob){
        stop('length of transition variable is unequal to data length')
      }
      G_grid <- mapply(transition_f, grid_comb[,1], grid_comb[,2], MoreArgs = list(st = transition_variable))
    }

  if(is.null(Z)){
    yl <- t(YLagCr(t(y), p))
    #yret <- y
    y_loop <- y[,-c(1:p)]

    if(type == 'const'){
      Z_t <- rbind(rep(1, ncol(yl)), yl)
    }else if(type == 'trend'){
      Z_t <- rbind(seq(p + 1, Tob), yl)
    }else if(type == 'both'){
      Z_t <- rbind(rep(1, ncol(yl)), seq(p + 1, Tob), yl)
    }else{
      Z_t <- yl
    }
  }else{
    Z_t <- Z
    y_loop <- y
  }



    best_estimation <- IterativeSmoothTransition(transition = G_grid, u = u, Tob = Tob, k = k, p = p,
                                                 crit = crit, maxIter = max.iter, Z_t = Z_t, Yloop = y_loop,
                                                 RestrictionMatrix = restriction_matrix, restrictions = restrictions)

    transition_function <- G_grid

    transition_coefficient <- gamma_fix
    SB <- c_fix
    comb <- 1


  result <- list(
    Lambda = best_estimation$Lambda,        # estimated Lambda matrix (unconditional heteroscedasticity)
    Lambda_SE = best_estimation$Lambda_SE,  # standard errors of Lambda matrix
    B = best_estimation$B,                  # estimated B matrix (unique decomposition of the covariance matrix)
    B_SE = best_estimation$B_SE,            # standard errors of B matrix
    n = Tob,                                # number of observations
    Fish = best_estimation$Fish,            # observerd fisher information matrix
    Lik = best_estimation$Lik,              # function value of likelihood
    iteration = best_estimation$iteration,  # number of gls estimations
    method = "Smooth transition",
    est_c = SB,       # Structural Break point
    est_g = transition_coefficient, # Parameter which determines the shape of thetransition function
    transition_variable = transition_variable,
    comb = comb,                 # number of all evaluated combinations of gamma and c
    transition_function = transition_function,
    A_hat = best_estimation$A_hat,          # VAR parameter estimated with gls
    type = type,          # type of the VAR model e.g 'const'
    y = yOut,                # Data
    p = p,                # number of lags
    K = k,                 # number of time series
    restrictions = restrictions,
    restriction_matrix = rmOut
  )

  class(result) <- 'svars'
  return(result)
}
