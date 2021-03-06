#' Independence-based identification of SVAR models via Cramer-von Mises (CVM) distance
#'
#'Given an estimated VAR model, this function applies independence-based identification for the structural impact matrix B of the corresponding SVAR model
#' \deqn{y_t=c_t+A_1 y_{t-1}+...+A_p y_{t-p}+u_t   =c_t+A_1 y_{t-1}+...+A_p y_{t-p}+B \epsilon_t.}
#' Matrix B corresponds to the unique decomposition of the least squares covariance matrix \eqn{\Sigma_u=B B'} if the vector of structural shocks \eqn{\epsilon_t} contains at most one Gaussian shock (Comon, 1994).
#' A nonparametric dependence measure, the Cramer-von Mises distance (Genest and Remillard, 2004), determines least dependent structural shocks. The minimum is obtained by a two step optimization algorithm similar to the technique described in Herwartz and Ploedt (2016).
#'
#' @param x An object of class 'vars', 'vec2var', 'nlVar'. Estimated VAR object
#' @param dd Object of class 'indepTestDist' (generated by 'indepTest' from package 'copula'). A simulated independent sample of the same size as the data. If not supplied, it will be calculated by the function
#' @param itermax Integer. IMaximum number of iterations for DEoptim
#' @param steptol Numeric. Tolerance for steps without improvement for DEoptim
#' @param iter2 Integer. Number of iterations for the second optimization
#' @return A list of class "svars" with elements
#' \item{B}{Estimated structural impact matrix B, i.e. unique decomposition of the covariance matrix of reduced form errors}
#' \item{A_hat}{Estimated VAR parameter}
#' \item{method}{Method applied for identification}
#' \item{n}{Number of observations}
#' \item{type}{Type of the VAR model, e.g. 'const'}
#' \item{y}{Data matrix}
#' \item{p}{Number of lags}
#' \item{K}{Dimension of the VAR}
#' \item{rotation_angles}{Rotation angles, which lead to maximum independence}
#' \item{inc}{Indicator. 1 = second optimization increased the estimation precision. 0 = second optimization did not increase the estimation precision}
#' \item{test.stats}{Computed test statistics of independence test}
#' \item{iter1}{Number of iterations of first optimization}
#' \item{test1}{Minimum test statistic from first optimization}
#' \item{test2}{Minimum test statistic from second optimization}
#' \item{VAR}{Estimated input VAR object}
#'
#'@references Herwartz, H., 2018. Hodges Lehmann detection of structural shocks - An Analysis of macroeconomic dynamics in the Euro Area, Oxford Bulletin of Economics and Statistics\cr
#'  Herwartz, H. & Ploedt, M., 2016. The macroeconomic effects of oil price shocks: Evidence from a statistical identification approach, Journal of International Money and Finance, 61, 30-44\cr
#'  Comon, P., 1994. Independent component analysis, A new concept?, Signal Processing, 36, 287-314\cr
#'  Genest, C. & Remillard, B., 2004. Tests of independence and randomness based on the empirical copula process, Test, 13, 335-370\cr
#'
#' @seealso For alternative identification approaches see \code{\link{id.st}}, \code{\link{id.garch}}, \code{\link{id.cv}}, \code{\link{id.dc}} or \code{\link{id.ngml}}
#'
#' @examples
#' \donttest{
#' # data contains quarterly observations from 1965Q1 to 2008Q3
#' # x = output gap
#' # pi = inflation
#' # i = interest rates
#' set.seed(23211)
#' v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#' cob <- copula::indepTestSim(v1$obs, v1$K, verbose=FALSE)
#' x1 <- id.cvm(v1, dd = cob)
#' summary(x1)
#'
#' # switching columns according to sign pattern
#' x1$B <- x1$B[,c(3,2,1)]
#' x1$B[,3] <- x1$B[,3]*(-1)
#'
#' # impulse response analysis
#' i1 <- irf(x1, n.ahead = 30)
#' plot(i1, scales = 'free_y')
#' }
#'
#' @importFrom copula indepTestSim
#' @importFrom pbapply pblapply
#' @importFrom DEoptim DEoptim.control DEoptim
#'
#' @export


#--------------------------------------------------#
## Identification via least dependent innovations ##
#--------------------------------------------------#

id.cvm <- function(x, dd = NULL, itermax = 500, steptol = 100, iter2 = 75){


  # if(is.null(residuals(x))){
  #   stop("No residuals retrieved from model")
  # }
  u <- Tob <- p <- k <- residY <- coef_x <- yOut <- type <- y <-  A_hat <- NULL
  get_var_objects(x)

  sigg1 <- crossprod(u)/(Tob-1-k*p)
  faklow1 <- t(chol(sigg1))

  ########### starting the computations ------------------------------------------------------------------------

  lower <- rep(0, k * (k - 1) / 2)
  upper <- rep(pi, k * (k - 1) / 2)

  if(is.null(dd)){
    dd <- indepTestSim(Tob, k, verbose=F)
  }

  ## First step of optimization with DEoptim
  de_control <- list(itermax = itermax, steptol = steptol,  trace = FALSE)

  de_res <- DEoptim(testlik, lower = lower, upper = upper,
                    control = de_control, faklow = faklow1,
                    u = u, dd = dd)

  ## Second step of optimization. Creating randomized starting angles around the optimized angles
  theta_rot <- matrix(rnorm(n = (k*(k-1)/2)*iter2, mean = de_res$optim$bestmem, sd = 0.3), (k*(k-1)/2), iter2)
  theta_rot <- cbind(theta_rot, de_res$optim$bestmem)
  # start vectors for iterative optimization approach
  startvec_list <- as.list(as.data.frame(theta_rot))

  erg_list <- lapply(X = startvec_list,
                       FUN = optim,
                       fn = testlik,
                       gr = NULL,
                       faklow = faklow1,
                       u = u,
                       dd = dd,
                       method = ifelse(k == 2, "Brent", "Nelder-Mead"),
                       lower = ifelse(k == 2, -.Machine$double.xmax/2, -Inf),
                       upper = ifelse(k == 2, .Machine$double.xmax/2, Inf),
                       control = list(maxit = 1000),
                       hessian = FALSE)


  # print log-likelihood values from local maxima
  logliks <- sapply(erg_list, "[[", "value")

  if(min(logliks) < de_res$optim$bestval){
    params <- sapply(erg_list, "[[", "par", simplify = FALSE)
    par_o <- params[[which.min(logliks)]]
    logs <- min(logliks)
    inc <- 1
  }else{
    par_o <- de_res$optim$bestmem
    logs <- de_res$optim$bestval
    inc <- 0
  }

  B_hat <- rotmat(par_o, faklow1)

  # obtaining VAR parameter
  # if(inherits(x, "var.boot")){
  #   A_hat <- coef_x
  # }else{
  #   if(type == "none"){
  #     A_hat <- vars::Bcoef(x)
  #   }else{
  #     A_hat <- vars::Bcoef(x)[, c((k * p+1):ncol(vars::Bcoef(x)),1:(k * p))]
  #   }
  # }


  rownames(B_hat) = colnames(u)

  result <- list(B = B_hat,        # estimated B matrix (unique decomposition of the covariance matrix)
                 A_hat = A_hat,    # estimated VAR parameter
                 method = "Cramer-von Mises distance",
                 n = Tob,          # number of observations
                 type = type,      # type of the VAR model e.g 'const'
                 y = yOut,         # Data
                 p = p,            # number of lags
                 K = k,            # number of time series
                 rotation_angles = par_o, # optimal rotation angles
                 inc = inc,        # wether the second optimization improved the estimation
                 test.stats = logs,# minimum teststatistic obtained
                 iter1 =  de_res$optim$iter, # number of iterations of first optimization
                 test1 = de_res$optim$bestval, # minimum teststatistic from first optimization
                 test2 = min(logliks), # minimum teststatistic from second optimization
                 VAR = x
  )

  class(result) <- "svars"
  return(result)

}
