#' GARCH identification of SVAR models
#'
#' Given an estimated VAR model, this function applies changes in volatility to identify the structural impact matrix B of the corresponding SVAR model
#' \deqn{y_t=c_t+A_1 y_{t-1}+...+A_p y_{t-p}+u_t
#' =c_t+A_1 y_{t-1}+...+A_p y_{t-p}+B \epsilon_t.}
#' Matrix B corresponds to the decomposition of the pre-break covariance matrix \eqn{\Sigma_1=B B'}.
#' The post-break covariance corresponds to \eqn{\Sigma_2=B\Lambda B'} where \eqn{\Lambda} is the estimated unconditional heteroskedasticity matrix.
#'
#' @param x An object of class 'vars', 'vec2var', 'nlVar'. Estimated VAR object
#' @param restriction_matrix Matrix. A matrix containing presupposed entries for matrix B, NA if no restriction is imposed (entries to be estimated)
#' @param max.iter Integer. Number of maximum GLS iterations
#' @param crit Integer. Critical value for the precision of the GLS estimation
#' @param start.iter Integer. Number of random initial GARCH parameter for univariate optimization. The estimation proceeds with the GARCH parameter, which maximizes the univariate likelihood
#' @return A list of class "svars" with elements
#' \item{Lambda}{Estimated unconditional heteroscedasticity matrix \eqn{\Lambda}}
#' \item{Lambda_SE}{Matrix of standard errors of Lambda}
#' \item{B}{Estimated structural impact matrix B, i.e. unique decomposition of the covariance matrix of reduced form residuals}
#' \item{B_SE}{Standard errors of matrix B}
#' \item{n}{Number of observations}
#' \item{Fish}{Observed Fisher information matrix}
#' \item{Lik}{Function value of likelihood}
#' \item{iteration}{Number of GLS estimations}
#' \item{method}{Method applied for identification}
#' \item{A_hat}{Estimated VAR paramter via GLS}
#' \item{type}{Type of the VAR model, e.g. 'const'}
#' \item{restrictions}{Number of specified restrictions}
#' \item{restriction_matrix}{Specified restriction matrix}
#' \item{y}{Data matrix}
#' \item{p}{Number of lags}
#' \item{K}{Dimension of the VAR}
#'
#' @references Rigobon, R., 2003. Identification through Heteroskedasticity. The Review of Economics and Statistics, 85, 777-792.\cr
#'  Herwartz, H. & Ploedt, M., 2016. Simulation Evidence on Theory-based and Statistical Identification under Volatility Breaks Oxford Bulletin of Economics and Statistics, 78, 94-112.
#'
#' @seealso For alternative identification approaches see \code{\link{id.st}}, \code{\link{id.cvm}}, \code{\link{id.dc}} or \code{\link{id.ngml}}
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

id.garch <- function(x, max.iter = 5, crit = 0.001, start.iter = 200, restriction_matrix = NULL){

  u <- Tob <- p <- k <- residY <- coef_x <- yOut <- type <- y <-  NULL
  get_var_objects(x)

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

  parameter_consider <- list()

  for(startround in 1:start.iter){
    ## Stage 1: Univariate optimization of GARCH(1, 1) parameter
    # Initial values as in Luetkepohl + Schlaak (2018)
    init_gamma <- runif(k)
    init_g <- rep(NA, k)
    test_g <- NA
    for(i in 1:k){
      test_g <- runif(1)
      if(init_gamma[i] + test_g < 1){
        init_g[i] <- test_g
      }else{
        while(init_gamma[i] + test_g > 1){
          test_g <- runif(1)
        }
        init_g[i] <- test_g
      }
    }
    parameter_ini_univ <- cbind(init_gamma, init_g)

    # first observstion of strucutral variance is the estimated sample variance
    Sigma_e_0 <-  matrix(diag(var(t(ste))),  Tob, k, byrow = T)

    # optimizing the univariate likelihood functions
    maxL <- list()
    gamma_univ <- rep(NA, k)
    g_univ <- rep(NA, k)
    param_univ <- matrix(NA, 3, k)
    Sigma_e_univ <- matrix(NA, Tob, k)

    likvalues <- rep(NA, k)

    for(i in 1:k){
      maxL <- nlm(p = parameter_ini_univ[i, ], f = likelihood_garch_uni, k = k, Tob = Tob,
                  Sigma_1 = Sigma_e_0[, i] , est = ste[i, ])
      # Likelihood values
      likvalues[i] <- maxL$minimum

      # Optimized GARCH parameter
      gamma_univ[i] <- maxL$estimate[1]
      g_univ[i] <- maxL$estimate[2]
      # Including a constant
      param_univ[, i] <- rbind((1- gamma_univ[i]- g_univ[i]), gamma_univ[i], g_univ[i])
      # estimated conditional heteroskedasticity
      Sigma_e_univ[,i] <- sigma_garch_univ(param_univ[,i], Tob, Sigma_e_0[,i], ste[i,])
    }

    parameter_consider[[startround]] <- list(parameter_ini_univ = cbind(gamma_univ, g_univ), Lik = mean(likvalues))
  }

  parameter_ini_univ <- parameter_consider[[which.min(sapply(parameter_consider, '[[', 'Lik'))]]$parameter_ini_univ
  # Store estimtated GARCH parameter as initial values for multivariate optimization

  if(!is.null(restriction_matrix)){
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
