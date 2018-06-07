#' Identification of SVAR models by means of a smooth transition of volatility
#'
#' Given an estimated VAR model, this function uses a smooth transition in the covariance to identify the structural impact matrix B of the corresponding SVAR model
#' \deqn{y_t=c_t+A_1 y_{t-1}+...+A_p y_{t-p}+u_t
#' =c_t+A_1 y_{t-1}+...+A_p y_{t-p}+B \epsilon_t.}
#' Matrix B corresponds to the decomposition of the pre-break covariance matrix \eqn{\Sigma_1=B B'}.
#' The post-break covariance corresponds to \eqn{\Sigma_2=B\Lambda B'} where \eqn{\Lambda} is the estimated heteroskedasticity matrix.
#'
#' @param x An object of class 'vars', 'vec2var', 'nlVar'. Estimated VAR object
#' @param nc Integer. Number of processor cores
#'           Note that the smooth transition model is computationally extremely demanding.
#' @param c_lower Integer. Starting point for the algorithm to start searching for the volatility shift.
#'                Default is 0.3*(Total number of observations)
#' @param c_upper Integer. Ending point for the algorithm to stop searching for the volatility shift.
#'                Default is 0.7*(Total number of observations)
#' @param c_step Integer. Step width of c. Default is 5
#' @param c_fix Integer. If the transition point is known, it can be passed as an argument
#'              where transition point = Number of observations - c_fix
#' @param transition_variable A numeric vector that represents the transition variable. By default (NULL), the time is used
#'                           as transition variable. Note that c_lower,c_upper, c_step and/or c_fix have to be adjusted
#'                           to the specified transition variable
#' @param gamma_lower Integer. Lower bound for gamma. Small values indicate a flat transition function. Default is -3
#' @param gamma_upper Integer. Upper bound for gamma. Large values indicate a steep transition function. Default is 2
#' @param gamma_step Integer. Step width of gamma. Default is 0.5
#' @param gamma_fix Integer. A fixed value for gamma, alternative to gamma found by the function
#' @param max.iter Integer. Number of maximum GLS iterations
#' @param crit Integer. Critical value for the precision of the GLS estimation
#' @param restriction_matrix Matrix. A matrix containing presupposed entries for matrix B, NA if no restriction is imposed (entries to be estimated)
#' @param lr_test Logical. Indicates whether the restricted model should be tested against the unrestricted model via a likelihood ratio test
#' @return A list of class "svars" with elements
#' \item{Lambda}{Estimated heteroscedasticity matrix \eqn{\Lambda}}
#' \item{Lambda_SE}{Matrix of standard errors of Lambda}
#' \item{B}{Estimated structural impact matrix B, i.e. unique decomposition of the covariance matrix of reduced form residuals}
#' \item{B_SE}{Standard errors of matrix B}
#' \item{n}{Number of observations}
#' \item{Fish}{Observed Fisher information matrix}
#' \item{Lik}{Function value of likelihood}
#' \item{wald_statistic}{Results of pairwise Wald tests}
#' \item{iteration}{Number of GLS estimations}
#' \item{method}{Method applied for identification}
#' \item{est_c}{Structural break (number of observations)}
#' \item{SBcharacter}{Structural break (date; if provided in function arguments)}
#'
#' @references Luetkepohl H., Netsunajev A., 2017. "Structural vector autoregressions with smooth transition \cr
#'   in variances." Journal of Economic Dynamics and Control, 84, 43 - 57. ISSN 0165-1889.
#'
#' @seealso For alternative identification approaches see \code{\link{id.cv}}, \code{\link{id.cvm}}, \code{\link{id.dc}},
#'          or \code{\link{id.ngml}}
#'
#' @examples
#' \donttest{
#' # data contains quartlery observations from 1965Q1 to 2008Q2
#' # x = output gap
#' # pi = inflation
#' # i = interest rates
#' set.seed(23211)
#' v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#' x1 <- id.st(v1)
#' summary(x1)
#' plot(x1)
#'
#' # switching columns according to sign patter
#' x1$B <- x1$B[,c(3,2,1)]
#' x1$B[,3] <- x1$B[,3]*(-1)
#'
#' # Impulse response analysis
#' i1 <- imrf(x1, horizon = 30)
#' plot(i1, scales = 'free_y')
#'
#' # Example with same data set as in Luetkepohl and Nestunajev 2017
#' v1 <- vars::VAR(LN, p = 3, type = 'const')
#' x1 <- id.st(v1, c_fix = 167, gamma_fix = -2.77)
#' summary(x1)
#' plot(x1)
#'
#' }
#' @importFrom steadyICA steadyICA
#' @export

id.st <- function(x, c_lower = 0.3, c_upper = 0.7, c_step = 5, c_fix = NULL, transition_variable = NULL,
                  gamma_lower = -3, gamma_upper = 2, gamma_step = 0.5, gamma_fix = NULL, nc = 1,
                  max.iter = 5, crit = 0.01, restriction_matrix = NULL, lr_test = FALSE){

  # Gathering information from reduced form model
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
    yOut <- x$y
  }else if(inherits(x, "varest")){
    p <- x$p
    y <- t(x$y)
    yOut <- x$y
  }else if(inherits(x, "nlVar")){
    if(inherits(x, "VECM")){
      stop("id.st is not available for VECMs")
    }
    p <- x$lag
    y <- t(x$model[, 1:k])
    yOut <- x$model[, 1:k]
  }else if(inherits(x, "list")){
    p <- x$order
    y <- t(x$data)
    yOut <- x$data
  }else{
    stop("Object class is not supported")
  }

  # Transition function
  transition_f <- function(gamma, cc, st){
    G <- (1 + exp(-exp(gamma)*(st - cc)))^(-1)
    return(G)
  }


  if(is.null(gamma_fix) &  is.null(c_fix)){
    # Creating grid for iterative procedure with gamma and break point unknown
    gamma_grid <-  seq(gamma_lower, gamma_upper, by = gamma_step)
    cc_grid <- seq(ceiling(c_lower*Tob), floor(c_upper*Tob), by = c_step)
    grid_comb <- unique(expand.grid(gamma_grid, cc_grid))
    if(is.null(transition_variable)){
      G_grid <- mapply(transition_f, grid_comb[,1], grid_comb[,2], MoreArgs = list(st = seq(1:Tob)))
    }else{
      if(length(transition_variable) != Tob){
        stop('length of transition variable is unequal to data length')
      }
      G_grid <- mapply(transition_f, grid_comb[,1], grid_comb[,2], MoreArgs = list(st = transition_variable))
    }
  }else if(is.null(gamma_fix)){
    # Creating grid for iterative procedure with fix break point
    gamma_grid <-  seq(gamma_lower, gamma_upper, by = gamma_step)
    grid_comb <- unique(expand.grid(gamma_grid, c_fix))
    G_grid <- mapply(transition_f, grid_comb[,1], grid_comb[,2], MoreArgs = list(st = seq(1:Tob)))
  }else if(is.null(c_fix)){
    # Creating grid for iterative procedure with fix shape of transition function
    cc_grid <- seq(ceiling(c_lower*Tob), floor(c_upper*Tob), by = c_step)
    grid_comb <- unique(expand.grid(gamma_fix, cc_grid))
    if(is.null(transition_variable)){
      G_grid <- mapply(transition_f, grid_comb[,1], grid_comb[,2], MoreArgs = list(st = seq(1:Tob)))
    }else{
      if(length(transition_variable) != Tob){
        stop('length of transition variable is unequal to data length')
      }
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
    Z_t <- rbind(seq(1, ncol(yl)), yl)
  }else if(x$type == 'both'){
    Z_t <- rbind(rep(1, ncol(yl)), seq(1, ncol(yl)), yl)
  }else{
    Z_t <- yl
  }

  if(!is.null(gamma_fix) &  !is.null(c_fix)){
    best_estimation <- iterative_smooth_transition(transition = G_grid, u_t = u_t, y = y, Tob = Tob, k = k,
                                           p = p, crit = crit, max.iter = max.iter, Z_t = Z_t, y_loop = y_loop,
                                           restriction_matrix = restriction_matrix)
    transition_function <- G_grid

    transition_coefficient <- gamma_fix
    SB <- c_fix
    comb <- 1

    if(lr_test == TRUE & !is.null(restriction_matrix)){
      unrestricted_estimation <- iterative_smooth_transition(G_grid, u_t = u_t, y = y, Tob = Tob, k = k,
                                                             p = p, crit = crit, max.iter = max.iter, Z_t = Z_t, y_loop = y_loop,
                                                             restriction_matrix = NULL)
      lRatioTestStatistic = 2 * (unrestricted_estimation$Lik - best_estimation$Lik)
      restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
      pValue = round(1 - pchisq(lRatioTestStatistic, restrictions), 4)
    }else{
      lRatioTestStatistic <- NULL
      pValue <- NULL
    }

  }else{
    G_grid <- apply(G_grid, 2, list)

    grid_optimization <- pblapply(G_grid, function(x){iterative_smooth_transition(unlist(x),
                                                                                  u_t = u_t, y = y,
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

      unrestricted_estimation <- iterative_smooth_transition(G_grid, u_t = u_t, y = y, Tob = Tob, k = k,
                                                             p = p, crit = crit, max.iter = max.iter, Z_t = Z_t, y_loop = y_loop,
                                                             restriction_matrix = NULL)
      lRatioTestStatistic = 2 * (unrestricted_estimation$Lik - best_estimation$Lik)
      restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
      pValue = round(1 - pchisq(lRatioTestStatistic, restrictions), 4)
    }else{
      lRatioTestStatistic <- NULL
      pValue <- NULL
    }
  }

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
    restriction_matrix = restriction_matrix,
    lRatioTestStatistic = lRatioTestStatistic,
    lRatioTestPValue = pValue
  )

  class(result) <- 'svars'
  return(result)
}
