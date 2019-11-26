#' Identification of SVAR models by means of a smooth transition (ST) in covariance
#'
#' Given an estimated VAR model, this function uses a smooth transition in the covariance to identify the structural impact matrix B of the corresponding SVAR model
#' \deqn{y_t=c_t+A_1 y_{t-1}+...+A_p y_{t-p}+u_t
#' =c_t+A_1 y_{t-1}+...+A_p y_{t-p}+B \epsilon_t.}
#' Matrix B corresponds to the decomposition of the pre-break covariance matrix \eqn{\Sigma_1=B B'}.
#' The post-break covariance corresponds to \eqn{\Sigma_2=B\Lambda B'} where \eqn{\Lambda} is the estimated heteroskedasticity matrix.
#'
#' @param x An object of class 'vars', 'vec2var', 'nlVar'. Estimated VAR object
#' @param c_lower Numeric. Starting point for the algorithm to start searching for the volatility shift.
#'                Default is 0.3*(Total number of observations)
#' @param c_upper Numeric. Ending point for the algorithm to stop searching for the volatility shift.
#'                Default is 0.7*(Total number of observations). Note that in case of a stochastic transition variable, the input requires an absolute value
#' @param c_step Integer. Step width of c. Default is 5. Note that in case of a stochastic transition variable, the input requires an absolute value
#' @param c_fix Numeric. If the transition point is known, it can be passed as an argument
#'              where transition point = Number of observations - c_fix
#' @param transition_variable A numeric vector that represents the transition variable. By default (NULL), the time is used
#'                           as transition variable. Note that c_lower,c_upper, c_step and/or c_fix have to be adjusted
#'                           to the specified transition variable
#' @param gamma_lower Numeric. Lower bound for gamma. Small values indicate a flat transition function. Default is -3
#' @param gamma_upper Numeric. Upper bound for gamma. Large values indicate a steep transition function. Default is 2
#' @param gamma_step Numeric. Step width of gamma. Default is 0.5
#' @param gamma_fix Numeric. A fixed value for gamma, alternative to gamma found by the function
#' @param nc Integer. Number of processor cores
#'           Note that the smooth transition model is computationally extremely demanding.
#' @param max.iter Integer. Number of maximum GLS iterations
#' @param crit Numeric. Critical value for the precision of the GLS estimation
#' @param restriction_matrix Matrix. A matrix containing presupposed entries for matrix B, NA if no restriction is imposed (entries to be estimated). Alternatively, a K^2*K^2 matrix can be passed, where ones on the diagonal designate unrestricted and zeros restricted coefficients. (as suggested in Luetkepohl, 2017, section 5.2.1).
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
#' \item{est_g}{Transition coefficient}
#' \item{transition_variable}{Vector of transition variable}
#' \item{comb}{Number of all grid combinations of gamma and c}
#' \item{transition_function}{Vector of transition function}
#' \item{A_hat}{Estimated VAR parameter via GLS}
#' \item{type}{Type of the VAR model e.g., 'const'}
#' \item{y}{Data matrix}
#' \item{p}{Number of lags}
#' \item{K}{Dimension of the VAR}
#' \item{restrictions}{Number of specified restrictions}
#' \item{restriction_matrix}{Specified restriction matrix}
#' \item{lr_test}{Logical, whether a likelihood ratio test is performed}
#' \item{lRatioTest}{Results of likelihood ratio test}
#'
#' @references Luetkepohl H., Netsunajev A., 2017. Structural vector autoregressions with smooth transition \cr
#'   in variances. Journal of Economic Dynamics and Control, 84, 43 - 57. ISSN 0165-1889.
#'
#' @seealso For alternative identification approaches see \code{\link{id.cv}}, \code{\link{id.garch}}, \code{\link{id.cvm}}, \code{\link{id.dc}},
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
#' x1 <- id.st(v1, c_fix = 80, gamma_fix = 0)
#' summary(x1)
#' plot(x1)
#'
#' # switching columns according to sign patter
#' x1$B <- x1$B[,c(3,2,1)]
#' x1$B[,3] <- x1$B[,3]*(-1)
#'
#' # Impulse response analysis
#' i1 <- irf(x1, n.ahead = 30)
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
                  max.iter = 5, crit = 0.001, restriction_matrix = NULL, lr_test = FALSE){

  # Gathering information from reduced form model
  u <- Tob <- p <- k <- residY <- coef_x <- yOut <- type <- y <-  NULL
  get_var_objects(x)

  # preparing restriction amtrix format and get no of restrictions
  rmOut = restriction_matrix
  restriction_matrix = get_restriction_matrix(restriction_matrix, k)
  restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])

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

  #ylold <- t(y_lag_cr(t(y), p)$lags)
  yl <- t(YLagCr(t(y), p))
  #yret <- y
  y_loop <- y[,-c(1:p)]

  if(type == 'const'){
    Z_t <- rbind(rep(1, ncol(yl)), yl)
  }else if(type == 'trend'){
    Z_t <- rbind(seq(p + 1, Tob + p), yl)
  }else if(type == 'both'){
    Z_t <- rbind(rep(1, ncol(yl)), seq(p + 1, Tob + p), yl)
  }else{
    Z_t <- yl
  }

  if(!is.null(gamma_fix) &  !is.null(c_fix)){
    if (!is.null(restriction_matrix)) {
      restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
    } else {
      restrictions <- 0
      restriction_matrix <- matrix(NA, k, k)
    }

    best_estimation <- IterativeSmoothTransition(transition = G_grid, u = u, Tob = Tob, k = k, p = p,
                                                 crit = crit, maxIter = max.iter, Z_t = Z_t, Yloop = y_loop,
                                                 RestrictionMatrix = restriction_matrix, restrictions = restrictions)
    transition_function <- G_grid

    transition_coefficient <- gamma_fix
    SB <- c_fix
    comb <- 1

    if(lr_test == TRUE & restrictions > 0){

      unrestricted_estimation <- IterativeSmoothTransition(transition = G_grid, u = u, Tob = Tob, k = k, p = p,
                                                           crit = crit, maxIter = max.iter, Z_t = Z_t, Yloop = y_loop,
                                                           RestrictionMatrix = matrix(NA, k, k), restrictions = 0)

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

    grid_optimization <- pblapply(G_grid, function(x){IterativeSmoothTransition(unlist(x),
                                                                                  u = u, Tob = Tob, k = k,
                                                                                  p = p, crit = crit, Yloop = y_loop,
                                                                                  maxIter = max.iter, Z_t = Z_t,
                                                                                  RestrictionMatrix = restriction_matrix,
                                                                                  restrictions = restrictions)},
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

      unrestricted_estimation <- IterativeSmoothTransition(G_grid, u = u, Tob = Tob, k = k,
                                                             p = p, crit = crit, maxIter = max.iter, Z_t = Z_t, Yloop = y_loop,
                                                             RestrictionMatrix = matrix(NA, k, k), restrictions = 0)
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
    type = type,          # type of the VAR model e.g 'const'
    y = yOut,                # Data
    p = p,                # number of lags
    K = k,                 # number of time series
    restrictions = restrictions,
    restriction_matrix = rmOut,
    lr_test = lr_test,
    lRatioTest = lRatioTest,
    AIC = (-2) * best_estimation$Lik + 2*(k + p * k^2 + (k + 1) * k + 1)
  )

  class(result) <- 'svars'
  return(result)
}
