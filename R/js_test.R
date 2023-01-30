#' Chi-square test for joint hypotheses
#'
#' Based on an existing bootstrap object, the test statistic allows to test joint hypotheses for selected entries of the structural matrix B. The test statistic reads as
#' \deqn{(Rvec(\widehat{B}) - r)'R(\widehat{\mbox{Cov}}[vec(B^*)])^{-1}R'(Rvec(\widehat{b} - r)) \sim \chi^2_J,}
#' where \eqn{\widehat{\mbox{Cov}}[vec(B^*)]} is the estimated covariance of vectorized bootstrap estimates of structural parameters. The composite null hypothesis is \eqn{H_0: Rvec(B)= r}.
#'
#' @param x Object of class 'sboot'
#' @param R A J*K^2 selection matrix, where J is the number of hypotheses and K the number of time series.
#' @param r A J*1 vector of restrictions
#'
#' @return A list of class "jstest" with elements
#' \item{test_statistic}{Test statistic}
#' \item{p_value}{P-value}
#' \item{R}{Selection matrix}
#' \item{r}{Vector of restrictions}
#'
#' @references Herwartz, H., 2018. Hodges Lehmann detection of structural shocks -
#'        An analysis of macroeconomic dynamics in the Euro Area, Oxford Bulletin of Economics and Statistics
#'
#' @seealso \code{\link{mb.boot}}, \code{\link{wild.boot}}
#'
#'@examples
#' \donttest{
#' # data contains quarterly observations from 1965Q1 to 2008Q3
#' # x = output gap
#' # pi = inflation
#' # i = interest rates
#' v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#' x1 <- id.dc(v1)
#'
#' # Bootstrapping of SVAR
#' bb <- wild.boot(x1, nboot = 1000, n.ahead = 30)
#'
#' # Testing the hypothesis of a lower triangular matrix as
#' # relation between structural and reduced form errors
#' R <- rbind(c(0,0,0,1,0,0,0,0,0), c(0,0,0,0,0,0,1,0,0),
#'            c(0,0,0,0,0,0,0,1,0))
#' c.test <- js.test(bb, R)
#' summary(c.test)
#' }
#'
#' @importFrom methods is
#'
#' @export
#'

js.test <- function(x, R, r = NULL){

  if(!is(x, 'sboot')){
    stop("Please provide an object of class 'sboot'")
  }
  if(!is.matrix(R)){
    stop("Please provide R in matrix format")
  }
      if(is.null(r)){
        r <- rep(0, nrow(R))
      }
        df <- nrow(R)

    v <- t(R%*%c(x$point_estimate) - r)%*%solve(R%*%x$cov_bs%*%t(R))%*%(R%*%c(x$point_estimate) - r)

  p.value <- 1 - pchisq(v, df)
  ctest <- list(
    test_statistic = v,
    p_value = p.value,
    R = R,
    r = r
  )
  class(ctest) <- 'jstest'
  return(ctest)
}
