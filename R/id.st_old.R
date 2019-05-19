id.st_old <- function(x, c_lower = 0.3, c_upper = 0.7, c_step = 5, c_fix = NULL, transition_variable = NULL,
                  gamma_lower = -3, gamma_upper = 2, gamma_step = 0.5, gamma_fix = NULL, nc = 1,
                  max.iter = 5, crit = 0.01, restriction_matrix = NULL, lr_test = FALSE){

  # Gathering information from reduced form model
  u <- Tob <- p <- k <- residY <- coef_x <- yOut <- type <- y <-  NULL
  get_var_objects(x)
  rmOut = restriction_matrix
  restriction_matrix = get_restriction_matrix(restriction_matrix, k)

  # Transition function
  transition_f <- function(gamma, cc, st){
    G <- (1 + exp(-exp(gamma)*(st - cc)))^(-1)
    return(G)
  }


  if(is.null(gamma_fix) &  is.null(c_fix)){
    # Creating grid for iterative procedure with gamma and break point unknown
    gamma_grid <-  seq(gamma_lower, gamma_upper, by = gamma_step)
    if(is.null(transition_variable)){
      cc_grid <- seq(ceiling(c_lower*Tob), floor(c_upper*Tob), by = c_step)
      grid_comb <- unique(expand.grid(gamma_grid, cc_grid))
      G_grid <- mapply(transition_f, grid_comb[,1], grid_comb[,2], MoreArgs = list(st = seq(1:Tob)))
    }else{
      if(length(transition_variable) != Tob){
        stop('length of transition variable is unequal to data length')
      }
      cc_grid <- seq(c_lower, c_upper, by = c_step)
      grid_comb <- unique(expand.grid(gamma_grid, cc_grid))
      G_grid <- mapply(transition_f, grid_comb[,1], grid_comb[,2], MoreArgs = list(st = transition_variable))
    }
  }else if(is.null(gamma_fix)){
    # Creating grid for iterative procedure with fix break point
    gamma_grid <-  seq(gamma_lower, gamma_upper, by = gamma_step)
    grid_comb <- unique(expand.grid(gamma_grid, c_fix))
    G_grid <- mapply(transition_f, grid_comb[,1], grid_comb[,2], MoreArgs = list(st = seq(1:Tob)))
  }else if(is.null(c_fix)){
    # Creating grid for iterative procedure with fix shape of transition function

    if(is.null(transition_variable)){
      cc_grid <- seq(ceiling(c_lower*Tob), floor(c_upper*Tob), by = c_step)
      grid_comb <- unique(expand.grid(gamma_fix, cc_grid))
      G_grid <- mapply(transition_f, grid_comb[,1], grid_comb[,2], MoreArgs = list(st = seq(1:Tob)))
    }else{
      if(length(transition_variable) != Tob){
        stop('length of transition variable is unequal to data length')
      }
      cc_grid <- seq(c_lower, c_upper, by = c_step)
      grid_comb <- unique(expand.grid(gamma_fix, cc_grid))
      G_grid <- mapply(transition_f, grid_comb[,1], grid_comb[,2], MoreArgs = list(st = transition_variable))
    }
  }else{
    grid_comb <- unique(expand.grid(gamma_fix, c_fix))
    if(is.null(transition_variable)){
      G_grid <- mapply(transition_f, grid_comb[,1], grid_comb[,2], MoreArgs = list(st = seq(1:Tob)))
    }else{
      if(length(transition_variable) != Tob){
        stop('length of transition variable is unequal to data length')
      }
      G_grid <- mapply(transition_f, grid_comb[,1], grid_comb[,2], MoreArgs = list(st = transition_variable))
    }
  }

  yl <- t(y_lag_cr(t(y), p)$lags)
  #yret <- y
  y_loop <- y[,-c(1:p)]

  if(x$type == 'const'){
    Z_t <- rbind(rep(1, ncol(yl)), yl)
  }else if(x$type == 'trend'){
    Z_t <- rbind(seq(p + 1, Tob), yl)
  }else if(x$type == 'both'){
    Z_t <- rbind(rep(1, ncol(yl)), seq(p + 1, Tob), yl)
  }else{
    Z_t <- yl
  }

  if(!is.null(gamma_fix) &  !is.null(c_fix)){
    best_estimation <- iterative_smooth_transition_old(transition = G_grid, u = u, y = y, Tob = Tob, k = k,
                                                   p = p, crit = crit, max.iter = max.iter, Z_t = Z_t, y_loop = y_loop,
                                                   restriction_matrix = restriction_matrix)
    transition_function <- G_grid

    transition_coefficient <- gamma_fix
    SB <- c_fix
    comb <- 1

    if(lr_test == TRUE & !is.null(restriction_matrix)){
      unrestricted_estimation <- iterative_smooth_transition_old(G_grid, u = u, y = y, Tob = Tob, k = k,
                                                             p = p, crit = crit, max.iter = max.iter, Z_t = Z_t, y_loop = y_loop,
                                                             restriction_matrix = NULL)
      lRatioTestStatistic = 2 * (unrestricted_estimation$Lik - best_estimation$Lik)
      restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
      pValue = round(1 - pchisq(lRatioTestStatistic, restrictions), 4)
      lRatioTest <- data.frame(testStatistic = lRatioTestStatistic, p.value = pValue)
      rownames(lRatioTest) <- ""
      colnames(lRatioTest) <- c("Test statistic", "p-value")
    }else{
      lRatioTest <- NULL
    }

  }else{
    G_grid <- apply(G_grid, 2, list)

    grid_optimization <- pblapply(G_grid, function(x){iterative_smooth_transition_old(unlist(x),
                                                                                  u = u, y = y,
                                                                                  Tob = Tob, k = k,
                                                                                  p = p, crit = crit,
                                                                                  max.iter = max.iter, Z_t = Z_t,
                                                                                  y_loop = y_loop,
                                                                                  restriction_matrix = restriction_matrix)},
                                  cl = nc)

    max_likelihood <- which.max(sapply(grid_optimization, '[[', 'Lik'))
    best_estimation <- grid_optimization[[max_likelihood]]
    transition_function <- unlist(G_grid[[max_likelihood]])

    transition_coefficient <- grid_comb[max_likelihood, 1]
    SB <- grid_comb[max_likelihood, 2]
    comb <- nrow(grid_comb)

    if(lr_test == TRUE & !is.null(restriction_matrix)){
      grid_comb <- unique(expand.grid(transition_coefficient, SB))
      if(is.null(transition_variable)){
        G_grid <- mapply(transition_f, grid_comb[,1], grid_comb[,2], MoreArgs = list(st = seq(1:Tob)))
      }else{
        if(length(transition_variable) != Tob){
          stop('length of transition variable is unequal to data length')
        }
        G_grid <- mapply(transition_f, grid_comb[,1], grid_comb[,2], MoreArgs = list(st = transition_variable))
      }

      unrestricted_estimation <- iterative_smooth_transition_old(G_grid, u = u, y = y, Tob = Tob, k = k,
                                                             p = p, crit = crit, max.iter = max.iter, Z_t = Z_t, y_loop = y_loop,
                                                             restriction_matrix = NULL)
      lRatioTestStatistic = 2 * (unrestricted_estimation$Lik - best_estimation$Lik)
      restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
      pValue = round(1 - pchisq(lRatioTestStatistic, restrictions), 4)
      lRatioTest <- data.frame(testStatistic = lRatioTestStatistic, p.value = pValue)
      rownames(lRatioTest) <- ""
      colnames(lRatioTest) <- c("Test statistic", "p-value")
    }else{
      lRatioTest <- NULL
    }
  }

  if(!is.null(restriction_matrix)){
    restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
  }else{
    restrictions <- 0
  }

  # Testing the estimated SVAR for identification by means of wald statistic
  wald <- wald.test(best_estimation$Lambda, best_estimation$Fish, restrictions)
  rownames(best_estimation$B) <- colnames(u)
  rownames(best_estimation$Lambda) <- colnames(u)
  rownames(best_estimation$Lambda_SE) <- colnames(u)
  rownames(best_estimation$B_SE) <- colnames(u)

  result <- list(
    Lambda = best_estimation$Lambda,        # estimated Lambda matrix (unconditional heteroscedasticity)
    Lambda_SE = best_estimation$Lambda_SE,  # standard errors of Lambda matrix
    B = best_estimation$B,                  # estimated B matrix (unique decomposition of the covariance matrix)
    B_SE = best_estimation$B_SE,            # standard errors of B matrix
    n = Tob,                                # number of observations
    Fish = best_estimation$Fish,            # observerd fisher information matrix
    Lik = best_estimation$Lik,              # function value of likelihood
    wald_statistic = wald,                  # results of wald test
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
    restriction_matrix = rmOut,
    lr_test = lr_test,
    lRatioTest = lRatioTest
  )

  class(result) <- 'svars'
  return(result)
}
