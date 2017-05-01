#' Changes in Volatility Identification
#'
#' Identify B matrix based on changes in Volatility.
#'
#' @param x VAR-object. (S)VAR model to determine B matrix for
#' @param SB integer. Structural break either of type integer as the number of observations which belong to the pre-break period or
#'                    Date character. If a date character is provided, either a date Vector which contains the time line of the data
#'                    in corresponding format or then the conventional time parameters need to be provided.
#' @param dateVector vector. Vector of the time period concerned containing SB.
#' @param start character. Start of the time series (only if dateVector is empty)
#' @param end character. End of the time series (only if dateVector is empty)
#' @param frequency character. Frequency of the time series (only if dateVector is empty)
#' @param format character. Date format (only if dateVector is empty)
#' @param restriction_matrix matrix. A matrix containing presupposed Values and NA for values to be estimated
#' @return A list of class "svarIdent" with elements
#' \item{Lambda}{Estimated unconditional heteroscedasticity Matrix}
#' \item{Lambda_SE}{Matrix of standard errors of Lambda}
#' \item{B}{Estimated B matrix, i.e. unique decomposition of the covariance matrix}
#' \item{B_SE}{Standard Errors of B matrix}
#' \item{n}{Number of observations}
#' \item{Fish}{Observerd Fisher information matrix}
#' \item{Lik}{Function value of likelihood}
#' \item{wald_statistic}{Results of pairwise Wald tests}
#' \item{iteration}{Number of GLS estimations}
#' \item{method}{The applied identifaction method}
#' \item{SB}{Structural break as number of observation}
#' \item{SBcharacter}{Structural break as date (if provided in function arguments)}
#'
#'
#'
#' @examples
#' \dontrun{
#' require(vars)
#' require(ggplot2)
#' require(reshape2)
#' # data contains quartlery observations from 1965Q1 to 2008Q3
#' # assumed structural break in 1979Q4
#' # x = output gap
#' # pi = inflation
#' # i = interest rates
#' set.seed(23211)
#' v1 <- VAR(USA, lag.max = 10, ic = "AIC" )
#' x1 <- id.cv(v1, SB = 60)
#' summary(x1)
#'
# switching columns according to sign patter
#' x1$B <- x1$B[,c(3,2,1)]
#' x1$B[,3] <- x1$B[,3]*(-1)
#'
# Impulse response analysis
#' plot(x1, horizon = 30, scales = 'free_y')
#' }
#'
#' @export


#--------------------------------------------#
## Identification via changes in volatility ##
#--------------------------------------------#

# x  : object of class VAR
# SB : Structural Break


id.cv <- function(x, SB, start = NULL, end = NULL, frequency = NULL,
                        format = NULL, dateVector = NULL, max.iter = 15, crit = 0.05, restriction_matrix = NULL){

  if(is.numeric(SB)){
    SBcharacter <- NULL
  }

  if(!is.numeric(SB)){
    SBcharacter <- SB
    SB <- getStructuralBreak(SB = SB, start = start, end = end,
                             frequency = frequency, format = format, dateVector = dateVector)
  }


  u_t <- residuals(x)
  p <- x$p
  Tob <- x$obs
  k <-x$K

  TB <- SB - p

  resid1 <- u_t[1:TB-1,]
  resid2 <- u_t[TB:Tob,]
  Sigma_hat1 <- (crossprod(resid1)) / (TB-1)
  Sigma_hat2 <- (crossprod(resid2)) / (Tob-TB+1)

  if(!is.null(restriction_matrix)){
   resultUnrestricted <- identifyVolatility(x, SB, Tob = Tob, u_t = u_t, k = k, restriction_matrix = NULL,
                                 Sigma_hat1 = Sigma_hat1, Sigma_hat2 = Sigma_hat2, p = p, TB = TB, SBcharacter)
    result <- identifyVolatility(x, SB, Tob = Tob, u_t = u_t, k = k, restriction_matrix = restriction_matrix,
                                           Sigma_hat1 = Sigma_hat1, Sigma_hat2 = Sigma_hat2, p = p, TB = TB, SBcharacter)

    lRatioTestStatistic = 2 * (resultUnrestricted$Lik - result$Lik)
    pValue = round(1 - pchisq(lRatioTestStatistic, result$restrictions), 4)

    result$lRatioTestStatistic = lRatioTestStatistic
    result$lRatioTestPValue = pValue
  }else{
    result <- identifyVolatility(x, SB, Tob = Tob, u_t = u_t, k = k, restriction_matrix = restriction_matrix,
                                 Sigma_hat1 = Sigma_hat1, Sigma_hat2 = Sigma_hat2, p = p, TB = TB, SBcharacter)
  }

  class(result) <- "svarIdent"
 return(result)
}
