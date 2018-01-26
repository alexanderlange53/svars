#' Chi square test for joint hypothesis testing
#'
#' the test statistic is calculated as
#' \deqn{(Rvec(\widehat{B}) - r)'R(\widehat{\mbox{Cov}}[vec(B^*)])^{-1}R'(Rvec(\widehat{b} - r)) \sim \chi^2_J},
#' where matrix B* is the bootstrap estimation of the structural parameters.
#'
#' @param x Object of class 'sboot'
#' @param R A J*K^2 selection matrix, where J is the number of hypotheses and K the number of time series.
#' @param r A J*1 vector of restrictions
#'
#' @return A list with elements
#' \item{test_statistic}{Test statistic}
#' \item{p_value}{P-value}
#'
#' @references Herwartz, H., 2017. Hodges Lehmann detection of structural shocks -
#'        An analysis of macroeconomic dynamics in the Euro Area, Oxford Bulletin of Economics and Statistics
#'@examples
#' \donttest{
#' # data contains quarterly observations from 1965Q1 to 2008Q3
#' x = output gap
#' pi = inflation
#' i = interest rates
#' v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#' x1 <- id.dc(v1)
#'
#' # Bootstrapping of SVAR
#' bb <- wild.boot(x1, nboot = 1000, horizon = 30)
#'
#' # Testing the hypothesis of a lower triangular matrix as
#' # relation between structural and reduced form errors
#' R <- rbind(c(0,0,0,1,0,0,0,0,0), c(0,0,0,0,0,0,1,0,0),
#'            c(0,0,0,0,0,0,0,1,0))
#' c.test <- joint.significance(bb, R)
#' summary(c.test)
#' }
#'
#' @export
#'

joint.significance <- function(x, R, r = NULL){

  if(class(x)!= 'sboot'){
    stop("Please provide an object of class 'sboot'")
  }
  if(is.null(r)){
    if(!is.null(nrow(R))){
      r <- rep(0, nrow(R))
      df <- nrow(R)
    }else{
      r <- 0
      df <- 1
    }

  }

  if(df > 1){
    v <- t(R%*%c(x$point_estimate) - r)%*%R%*%solve(x$cov_bs)%*%t(R)%*%(R%*%c(x$point_estimate) - r)
  }else{
    v <- t(R%*%c(x$point_estimate) - r)%*%R%*%solve(x$cov_bs)%*%R%*%(R%*%c(x$point_estimate) - r)
  }

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
