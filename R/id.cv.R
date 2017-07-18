#' Changes in Volatility Identification
#'
#' Identify instantaneous response matrix B in SVAR models based on changes in volatility.
#'
#' @param x VAR-object. (S)VAR model to be identified
#' @param SB Integer or date character. The structural break is specified either by an integer (number of observations in the pre-break period) or
#'                    a date character. If a date character is provided, either a date vector containing the whole time line
#'                    in the corresponding format (see examples) or common time parameters need to be provided
#' @param dateVector Vector. Vector of time periods containing SB in corresponding format
#' @param start Character. Start of the time series (only if dateVector is empty)
#' @param end Character. End of the time series (only if dateVector is empty)
#' @param frequency Character. Frequency of the time series (only if dateVector is empty)
#' @param format Character. Date format (only if dateVector is empty)
#' @param restriction_matrix Matrix. A matrix containing presupposed values for matrix B, NA if no restriction is imposed (values to be estimated)
#' @param max.iter Integer. Number of maximum GLS iterations
#' @param crit Integer. Critical value for the precision of the GLS estimation
#' @return A list of class "svars" with elements
#' \item{Lambda}{Estimated unconditional heteroscedasticity matrix}
#' \item{Lambda_SE}{Matrix of standard errors of Lambda}
#' \item{B}{Estimated instantaneous response matrix B, i.e. unique decomposition of the covariance matrix of reduced form residuals}
#' \item{B_SE}{Standard errors of matrix B}
#' \item{n}{Number of observations}
#' \item{Fish}{Observerd Fisher information matrix}
#' \item{Lik}{Function value of likelihood}
#' \item{wald_statistic}{Results of pairwise Wald tests}
#' \item{iteration}{Number of GLS estimations}
#' \item{method}{Method applied for identification}
#' \item{SB}{Structural break (number of observations)}
#' \item{SBcharacter}{Structural break (date; if provided in function arguments)}
#'
#' @references Rigobon, R., 2003. Identification through Heteroskedasticity The Review of Economics and Statistics, 85, 777-792.
#'
#' @examples
#' \dontrun{
#' # data contains quartlery observations from 1965Q1 to 2008Q2
#' # assumed structural break in 1979Q4
#' # x = output gap
#' # pi = inflation
#' # i = interest rates
#' set.seed(23211)
#' v1 <- VAR(USA, lag.max = 10, ic = "AIC" )
#' x1 <- id.cv(v1, SB = 60)
#' summary(x1)
#'
#' # switching columns according to sign patter
#' x1$B <- x1$B[,c(3,2,1)]
#' x1$B[,3] <- x1$B[,3]*(-1)
#'
#' # Impulse response analysis
#' i1 <- imrf(x1, horizon = 30)
#' plot(i1, scales = 'free_y')
#'
#' # Restrictions
#' # Assuming that the interest rate doesn't influence the output gap on impact
#' restMat <- matrix(rep(NA, 9), ncol = 3)
#' restMat[1,3] <- 0
#' x2 <- id.cv(v1, SB = 60, restriction_matrix = restMat)
#'
#' #Structural brake via Dates
#' # given that time series vector with dates is available
#' dateVector = seq(as.Date("1965/1/1"), as.Date("2008/6/1"), "quarter")
#' x3 <- id.cv(v1, SB = "1985-01-01", format = "%Y-%m-%d", dateVector = dateVector)
#'
#' # or pass sequence arguments directly
#' x4 <- id.cv(v1, SB = "1985-01-01", format = "%Y-%m-%d", start = "1965-01-01", end = "2008-06-01",
#' frequency = "quarter")
#' }
#'
#' @export


#--------------------------------------------#
## Identification via changes in volatility ##
#--------------------------------------------#

# x  : object of class VAR
# SB : structural break


id.cv <- function(x, SB, start = NULL, end = NULL, frequency = NULL,
                        format = NULL, dateVector = NULL, max.iter = 50, crit = 0.05, restriction_matrix = NULL){

  u_t <- residuals(x)
  p <- x$p
  Tob <- x$obs
  k <-x$K

  if(is.numeric(SB)){
    SBcharacter <- NULL
  }

  if(!is.numeric(SB)){
    SBcharacter <- SB
    SB <- getStructuralBreak(SB = SB, start = start, end = end,
                             frequency = frequency, format = format, dateVector = dateVector, Tob = Tob, p = p)
  }


  TB <- SB - p

  resid1 <- u_t[1:TB-1,]
  resid2 <- u_t[TB:Tob,]
  Sigma_hat1 <- (crossprod(resid1)) / (TB-1)
  Sigma_hat2 <- (crossprod(resid2)) / (Tob-TB+1)

  if(!is.null(restriction_matrix)){
   resultUnrestricted <- identifyVolatility(x, SB, Tob = Tob, u_t = u_t, k = k, restriction_matrix = NULL,
                                 Sigma_hat1 = Sigma_hat1, Sigma_hat2 = Sigma_hat2, p = p, TB = TB, SBcharacter,
                                 max.iter = max.iter)
    result <- identifyVolatility(x, SB, Tob = Tob, u_t = u_t, k = k, restriction_matrix = restriction_matrix,
                                           Sigma_hat1 = Sigma_hat1, Sigma_hat2 = Sigma_hat2, p = p, TB = TB, SBcharacter,
                                 max.iter = max.iter)

    lRatioTestStatistic = 2 * (resultUnrestricted$Lik - result$Lik)
    pValue = round(1 - pchisq(lRatioTestStatistic, result$restrictions), 4)

    result$lRatioTestStatistic = lRatioTestStatistic
    result$lRatioTestPValue = pValue
  }else{
    restriction_matrix <- NULL
    result <- identifyVolatility(x, SB, Tob = Tob, u_t = u_t, k = k, restriction_matrix = restriction_matrix,
                                 Sigma_hat1 = Sigma_hat1, Sigma_hat2 = Sigma_hat2, p = p, TB = TB, SBcharacter,
                                 max.iter = max.iter)
  }

  class(result) <- "svars"
 return(result)
}
