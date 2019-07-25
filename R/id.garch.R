#' GARCH identification of SVAR models
#'
#' Given an estimated VAR model, this function uses GARCH-type variances to identify the structural impact matrix B of the corresponding SVAR model
#' \deqn{y_t=c_t+A_1 y_{t-1}+...+A_p y_{t-p}+u_t
#' =c_t+A_1 y_{t-1}+...+A_p y_{t-p}+B \epsilon_t.}
#' Matrix B corresponds to the decomposition of the least squares covariance matrix \eqn{\Sigma_u=B\Lambda_t B'}, where \eqn{\Lambda_t} is the estimated conditional heteroskedasticity matrix.
#'
#' @param x An object of class 'vars', 'vec2var', 'nlVar'. Estimated VAR object
#' @param restriction_matrix Matrix. A matrix containing presupposed entries for matrix B, NA if no restriction is imposed (entries to be estimated). Alternatively, a K^2*K^2 matrix can be passed, where ones on the diagonal designate unrestricted and zeros restricted coefficients. (as suggested in Luetkepohl, 2017, section 5.2.1).
#' @param max.iter Integer. Number of maximum likelihood optimizations
#' @param crit Numeric. Critical value for the precision of the GLS estimation
#' @return A list of class "svars" with elements
#' \item{B}{Estimated structural impact matrix B, i.e. unique decomposition of the covariance matrix of reduced form residuals}
#' \item{B_SE}{Standard errors of matrix B}
#' \item{GARCH_parameter}{Estimated GARCH parameters of univariate GARCH models}
#' \item{GARCH_SE}{Standard errors of GARCH parameters}
#' \item{n}{Number of observations}
#' \item{Fish}{Observed Fisher information matrix}
#' \item{Lik}{Function value of likelihood}
#' \item{iteration}{Number of likelihood optimizations}
#' \item{method}{Method applied for identification}
#' \item{A_hat}{Estimated VAR parameter via GLS}
#' \item{type}{Type of the VAR model, e.g. 'const'}
#' \item{restrictions}{Number of specified restrictions}
#' \item{restriction_matrix}{Specified restriction matrix}
#' \item{y}{Data matrix}
#' \item{p}{Number of lags}
#' \item{K}{Dimension of the VAR}
#'
#' @references Normadin, M. & Phaneuf, L., 2004. Monetary Policy Shocks: Testing Identification Conditionsunder Time-Varying Conditional Volatility. Journal of Monetary Economics, 51(6), 1217-1243.\cr
#'  Lanne, M. & Saikkonen, P., 2007. A Multivariate Generalized Orthogonal Factor GARCH Model. Journal of Business & Economic Statistics, 25(1), 61-75.
#'
#' @seealso For alternative identification approaches see \code{\link{id.st}}, \code{\link{id.cvm}}, \code{\link{id.cv}}, \code{\link{id.dc}} or \code{\link{id.ngml}}
#'
#' @examples
#' \donttest{
#' # data contains quartlery observations from 1965Q1 to 2008Q2
#' # assumed structural break in 1979Q3
#' # x = output gap
#' # pi = inflation
#' # i = interest rates
#' set.seed(23211)
#' v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#' x1 <- id.garch(v1)
#' summary(x1)
#'
#' # Impulse response analysis
#' i1 <- irf(x1, n.ahead = 30)
#' plot(i1, scales = 'free_y')
#'
#' # Restrictions
#' # Assuming that the interest rate doesn't influence the output gap on impact
#' restMat <- matrix(rep(NA, 9), ncol = 3)
#' restMat[1,3] <- 0
#' x2 <- id.garch(v1, restriction_matrix = restMat)
#' summary(x2)
#'
#'
#' }
#' @export


#----------------------------#
## Identification via GARCH ##
#----------------------------#

id.garch <- function(x, max.iter = 5, crit = 0.001, restriction_matrix = NULL){

  u <- Tob <- p <- k <- residY <- coef_x <- yOut <- type <- y <-  NULL
  get_var_objects(x)


  restriction_matrix = get_restriction_matrix(restriction_matrix, k)
  restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
  sigg <- crossprod(u)/(Tob - 1 - k * p)

  # Polar decomposition as in Lanne Saikkonen
  eigVectors <- eigen(sigg)$vectors
  eigValues <- diag(eigen(sigg)$values)
  eigVectors <- eigVectors[,rev(1:ncol(eigVectors))]
  eigValues <- diag(rev(eigen(sigg)$values))

  CC <- eigVectors %*% sqrt(eigValues) %*% t(eigVectors)
  B0 <- solve(solve(sqrt(eigValues)) %*% t(eigVectors))

  # Initial structural erros
  ste <- solve(sqrt(eigValues)) %*% t(eigVectors) %*% t(u)

  ##***********************************##
  #### Finding optimal starting values ##
  ##***********************************##

  parameter_consider <- GarchStart(k, ste, Tob)
  parameter_ini_univ <- parameter_consider[[which.min(sapply(parameter_consider, '[[', 'Likelihoods'))]]$ParameterE

  Sigma_e_univ <- parameter_consider[[which.min(sapply(parameter_consider, '[[', 'Likelihoods'))]]$ConVariance

  # Store estimtated GARCH parameter as initial values for multivariate optimization

  if(restrictions > 0){
    resultUnrestricted <- identifyGARCH(B0 = B0, k = k, Tob = Tob, restriction_matrix = NULL, Sigma_e_univ = Sigma_e_univ, coef_x = coef_x, x = x,
                                        parameter_ini_univ = parameter_ini_univ, max.iter = max.iter, crit = crit, u = u, p = p, yOut = yOut, type = type)
    result <- identifyGARCH(B0 = B0, k = k, Tob = Tob, restriction_matrix = restriction_matrix, Sigma_e_univ = Sigma_e_univ, coef_x = coef_x, x = x,
                            parameter_ini_univ = parameter_ini_univ, max.iter = max.iter, crit = crit, u = u, p = p, yOut = yOut, type = type)

    lRatioTestStatistic = 2 * (resultUnrestricted$Lik - result$Lik)
    pValue = round(1 - pchisq(lRatioTestStatistic, result$restrictions), 4)

    #result$lRatioTestStatistic = lRatioTestStatistic
    #result$lRatioTestPValue = pValue
    lRatioTest <- data.frame(testStatistic = lRatioTestStatistic, p.value = pValue)
    rownames(lRatioTest) <- ""
    colnames(lRatioTest) <- c("Test statistic", "p-value")
    result$lRatioTest <- lRatioTest
  }else{
    restriction_matrix <- NULL
    result <- identifyGARCH(B0 = B0, k = k, Tob = Tob, restriction_matrix = restriction_matrix, Sigma_e_univ = Sigma_e_univ, coef_x = coef_x, x = x,
                            parameter_ini_univ = parameter_ini_univ, max.iter = max.iter, crit = crit, u = u, p = p, yOut = yOut, type = type)
  }
  class(result) <- "svars"

  return(result)

}
