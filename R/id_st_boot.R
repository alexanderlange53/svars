id.st_boot <- function(x, c_fix = NULL, transition_variable = NULL,
                  gamma_fix = NULL, nc = 1, Z,
                  max.iter = 5, crit = 0.01, restriction_matrix = NULL){

  if(inherits(x, "var.boot")){
    u_t <- x$residuals
    Tob <- nrow(u_t)
    k <- ncol(u_t)
    residY <- u_t
  }else{
    u_t <- residuals(x)
    Tob <- nrow(u_t)
    k <- ncol(u_t)
    residY <- u_t
  }

  if(inherits(x, "var.boot")){
    p <- x$p
    y <- t(x$y)
    type = x$type
    coef_x = x$coef_x
  }else if(inherits(x, "varest")){
    p <- x$p
    y <- t(x$y)
  }else if(inherits(x, "nlVar")){
    if(inherits(x, "VECM")){
      stop("id.cv is not available for VECMs")
    }
    p <- x$lag
    y <- t(x$model[, 1:k])
  }else if(inherits(x, "list")){
    p <- x$order
    y <- t(x$data)
  }else{
    stop("Object class is not supported")
  }


  # Function for Z matrix
  y_lag_cr <- function(y, lag_length){
    # create matrix that stores the lags
    y_lag <- matrix(NA, dim(y)[1], dim(y)[2]*lag_length)
    for (i in 1:lag_length) {
      y_lag[(1 + i):dim(y)[1], ((i*ncol(y) - ncol(y)) + 1):(i * ncol(y))] <- y[1:(dim(y)[1] - i), (1:ncol(y))]
    }
    # drop first observation
    y_lag <- as.matrix(y_lag[-(1:lag_length),])
    out <- list(lags = y_lag)
  }

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
    yl <- t(y_lag_cr(t(y), p)$lags)
    #yret <- y
    y_loop <- y[,-c(1:p)]

    if(x$type == 'const'){
      Z_t <- rbind(rep(1, ncol(yl)), yl)
    }else if(x$type == 'trend'){
      Z_t <- rbind(seq(1, ncol(yl)), yl)
    }else if(x$type == 'both'){
      Z_t <- rbind(rep(1, ncol(yl)), seq(1, ncol(yl)), yl)
    }else{
      Z_t <- yl
    }
  }else{
    Z_t <- Z
  }


    best_estimation <- iterative_smooth_transition(transition = G_grid, u_t = u_t, y = y, Tob = Tob, k = k,
                                                   p = p, crit = crit, max.iter = max.iter, Z_t = Z_t, y_loop = y_loop,
                                                   restriction_matrix = restriction_matrix)
    transition_function <- G_grid

    transition_coefficient <- gamma_fix
    SB <- c_fix
    comb <- 1

  if(!is.null(restriction_matrix)){
    restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
  }else{
    restrictions <- 0
  }

  # Testing the estimated SVAR for identification by means of wald statistic
  wald <- wald.test(best_estimation$Lambda, best_estimation$Fish, restrictions)

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
    type = x$type,          # type of the VAR model e.g 'const'
    y = yOut,                # Data
    p = p,                # number of lags
    K = k,                 # number of time series
    restrictions = restrictions,
    restriction_matrix = restriction_matrix
  )

  class(result) <- 'svars'
  return(result)
}
