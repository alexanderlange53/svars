#' Chow Test for Structural Break
#'
#' The Chow test for structural change is implemented as sample-split and break-point test (see Luetkepohl and Kraetzig, 2004, p. 135). An estimated VAR model and the presupposed structural break need to be provided.
#'
#' @param x An object of class 'vars', 'vec2var', 'nlVar'. Estimated VAR object. Or an object of class 'chow_pretest' from stability()
#' @param SB Integer, vector or date character. The structural break is specified either by an integer (number of observations in the pre-break period),
#'                    a vector of ts() frequencies if a ts object is used in the VAR or a date character. If a date character is provided, either a date vector containing the whole time line
#'                    in the corresponding format or common time parameters need to be provided
#' @param nboot Integer. Number of bootstrap iterations to calculate quantiles and p-values
#' @param rademacher If rademacher="TRUE", the Rademacher distribution is used to generate the bootstrap samples
#' @param dateVector Vector. Vector of time periods containing SB in corresponding format
#' @param start Character. Start of the time series (only if dateVector is empty)
#' @param end Character. End of the time series (only if dateVector is empty)
#' @param frequency Character. Frequency of the time series (only if dateVector is empty)
#' @param format Character. Date format (only if dateVector is empty)
#'
#' @return A list with elements
#' \item{lambda_bp}{Test statistic of the Chow test with break point}
#' \item{testcrit_bp}{Critical value of the test statistic lambda_bp}
#' \item{p.value_bp}{p-value of the test statistic lambda_bp}
#' \item{lambda_sp}{Test statistic of the Chow test with sample split}
#' \item{testcrit_sp}{Critical value of the test statistic lambda_sp}
#' \item{p.value_sp}{p-value of the test statistic lambda_sp}
#'
#' @references Luetkepohl, H., 2005. New introduction to multiple time series analysis, Springer-Verlag, Berlin.\cr
#'      Luetkepohl, H., Kraetzig, M., 2004. Applied time series econometrics, Cambridge University Press, Cambridge.
#'
#' @seealso \code{\link{stability}}
#'
#'@examples
#' \donttest{
#' # Testing for structural break in USA data
#' #' # data contains quartlery observations from 1965Q1 to 2008Q2
#' # assumed structural break in 1979Q3
#' # x = output gap
#' # pi = inflation
#' # i = interest rates
#' set.seed(23211)
#' v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#' z1 <- chow.test(v1, SB = 59)
#' summary(z1)
#'
#' #Using stability() to find potential break point and sample split
#' x1 <- stability(v1, type = "mv-chow-test")
#' z1.1 <- chow.test(x1)
#' summary(z1.1)
#' #Or using sample split as reference
#' x1$break_point <- FALSE
#' z1.1 <- chow.test(x1)
#' summary(z1.1)
#'
#' #Structural brake via Dates
#' #given that time series vector with dates is available
#' dateVector <- seq(as.Date("1965/1/1"), as.Date("2008/7/1"), "quarter")
#' z2 <- chow.test(v1, SB = "1979-07-01", format = "%Y-%m-%d", dateVector = dateVector)
#' summary(z2)
#'
#' # alternatively pass sequence arguments directly
#' z3 <- chow.test(v1, SB = "1979-07-01", format = "%Y-%m-%d",
#'                 start = "1965-01-01", end = "2008-07-01",
#'                 frequency = "quarter")
#' summary(z3)
#'
#' # or provide ts date format (For quarterly, monthly, weekly and daily frequencies only)
#' z4 <- chow.test(v1, SB = c(1979,3))
#' summary(z4)
#' }
#' @import stats
#' @importFrom utils combn
#' @importFrom vars VAR
#' @importFrom expm sqrtm
#'
#' @export
#'

## Chow tests of system structural break ##
#-----------------------------------------#

# Y     : data
# SB    : structural break
# nboot : number of bootstrap iterations
# lags  : maximum lag order

chow.test <- function(x, SB, nboot = 500, rademacher = TRUE ,start = NULL, end = NULL,
                      frequency = NULL, format = NULL, dateVector = NULL){

  if(class(x) == 'chow_pretest'){
    if(x$break_point == TRUE){
      SB <- x$from + which.max(na.omit(x$teststat_bp)) + x$p -1
    }else{
      SB <- x$from + which.max(na.omit(x$teststat_sp)) + x$p -1
    }
    x <- x$ovar
  }

  u <- Tob <- p <- k <- residY <- coef_x <- yOut <- type <- y <-  NULL
  get_var_objects(x)

  # Null hypothesis of no sample split is rejected for large values of lambda
  Tob <- x$obs
  if(is.numeric(SB)){
    SBcharacter <- NULL
  }

  if(!is.numeric(SB)){
    SBcharacter <- SB
    SB <- getStructuralBreak(SB = SB, start = start, end = end,
                             frequency = frequency, format = format, dateVector = dateVector, Tob = Tob, p = p)
  }

  if(length(SB) != 1 & inherits(yOut, "ts")){
    SBts = SB
    SB = dim(window(yOut, end = SB))[1]
    if(frequency(y == 4)){
      SBcharacter = paste(SBts[1], " Q", SBts[2], sep = "")
    }else if(frequency(yOut == 12)){
      SBcharacter = paste(SBts[1], " M", SBts[2], sep = "")
    }else if(frequency(yOut == 52)){
      SBcharacter = paste(SBts[1], " W", SBts[2], sep = "")
    }else if(frequency(yOut == 365.25)){
      SBcharacter = paste(SBts[1], "-", SBts[2], "-", SBts[3], sep = "")
    }else{
      SBcharacter = NULL
    }

  }
  y <- t(y)

  Full <- y

  # splitting sample
  sample1 <- y[1:SB, ]
  sample2 <- y[(SB+1+p):nrow(y), ]

  # estimating VAR for pre and post SB and for full series
  VAR.model <- VAR(Full, p = p, type = type)
  VAR1.model <- VAR(sample1, p = p, type = type)
  VAR2.model <- VAR(sample2, p = p, type = type)

  l1 <- VAR1.model$obs
  l2 <- VAR2.model$obs
  ll <- VAR.model$obs

  # calculating four covariance matrices
  Sigma.1 <- (1/l1)*t(residuals(VAR1.model))%*%(residuals(VAR1.model))
  Sigma.2 <- (1/l2)*t(residuals(VAR2.model))%*%(residuals(VAR2.model))

  Sigma <- (1/l1)*t(residuals(VAR.model)[1:l1,])%*%(residuals(VAR.model)[1:l1,]) +
    (1/l2)*t(residuals(VAR.model)[(ll-l2+1):ll,])%*%(residuals(VAR.model)[(ll-l2+1):ll,])

  Sigma.1.2 <- (1/(l1 + l2))*(t(residuals(VAR.model)[1:l1,])%*%(residuals(VAR.model)[1:l1,]) +
                                t(residuals(VAR.model)[(ll-l2+1):ll,])%*%(residuals(VAR.model)[(ll-l2+1):ll,]))

  # calculating the test statistic
  # lambda_bp : test statistic for break point (tests for change in covariance in addition)
  # lambda_SP : test statistic for structural break (tests only for parameter change)
  lambda_bp <- (l1 + l2)*log(det(Sigma)) - l1*log(det(Sigma.1)) - l2*log(det(Sigma.2))
  lambda_sp <- (l1 + l2)*(log(det(Sigma.1.2)) - log(det((1/(l1 + l2))*(l1*Sigma.1 + l2*Sigma.2))))

if(!is.null(nboot)){
  lambda_bpB <- rep(NA, nboot)
  lambda_spB <- rep(NA, nboot)
  TB <- l1


  # bootstrapping the test statistic to obtain empirical distribution
  # obtaining VAR parameter
  coef_x <- coef(x)

  A <- matrix(0, nrow =k, ncol = k* p)
  yl <- t(y_lag_cr(y, p)$lags)

  for(i in 1:k){
    A[i,] <- coef_x[[i]][1:(k*p),1]
  }

  if(type == "const"){
    v <- rep(1, k)

    for(i in 1:k){
      v[i] <- coef_x[[i]][(k*p+1), 1]
    }

    A <- cbind(v, A)
    Z_t <- rbind(rep(1, ncol(yl)), yl)
  }else if (type == "trend"){
    trend <- rep(1, k)

    for(i in 1:k){
      trend[i] <- coef_x[[i]][(k*p+1), 1]
    }

    A <- cbind(trend, A)
    Z_t <- rbind(seq(1, ncol(yl)), yl)
  }else if(type == "both"){
    v <- rep(1, k)

    for(i in 1:k){
      v[i] <- coef_x[[i]][(k*p+1), 1]
    }

    trend <- rep(1, k)
    Z_t <- rbind(rep(1, ncol(yl)), seq(1, ncol(yl)), yl)
    for(i in 1:k){
      trend[i] <- coef_x[[i]][(k*p+2), 1]
    }

    A <- cbind(v, trend, A)
  }else{
    Z_t <- yl
  }

  residF <- residuals(VAR.model)

  # creating new data
  datFull <- list()

  for(i in 1:nboot){
    u <- residF
    my <- rnorm(n = k)
    if (rademacher == TRUE) {
      my <- (my > 0) - (my < 0)
    }
    et <- u * my
    Ystar <- t(A %*% Z_t + t(et))
    datFull[[i]] <- Ystar
  }

  for(i in 1:nboot){
    xx <- datFull[[i]]
    # splitting sample
    sample1 <- xx[1:SB, ]
    sample2 <- xx[(SB+1+p):nrow(xx), ]

    VARB.model <- VAR(xx, p = p)
    VARB1.model <- VAR(sample1, p = p)
    VARB2.model <- VAR(sample2, p = p)

    l1 <- VARB1.model$obs
    l2 <- VARB2.model$obs
    ll <- VARB.model$obs

    Sigma.1 <- (1/l1)*t(residuals(VARB1.model))%*%(residuals(VARB1.model))
    Sigma.2 <- (1/l2)*t(residuals(VARB2.model))%*%(residuals(VARB2.model))

    Sigma <- (1/l1)*t(residuals(VARB.model)[1:l1,])%*%(residuals(VARB.model)[1:l1,]) +
      (1/l2)*t(residuals(VARB.model)[(ll-l2+1):ll,])%*%(residuals(VARB.model)[(ll-l2+1):ll,])

    Sigma.1.2 <- (1/(l1 + l2))*(t(residuals(VARB.model)[1:l1,])%*%(residuals(VARB.model)[1:l1,]) +
                                  t(residuals(VARB.model)[(ll-l2+1):ll,])%*%(residuals(VARB.model)[(ll-l2+1):ll,]))

    lambda_bpB[i] <- (l1 + l2)*log(det(Sigma)) - l1*log(det(Sigma.1)) - l2*log(det(Sigma.2))
    lambda_spB[i] <- (l1 + l2)*(log(det(Sigma.1.2)) - log(det((1/(l1 + l2))*(l1*Sigma.1 + l2*Sigma.2))))
  }


  df_bp <- p*k^2 + k + (k*(k + 1))/2
  df_sp <- p*k^2 + k

  # obtaining critical values and p-values for both tests
  testcrit_bp <- quantile(lambda_bpB, probs = 0.95)
  EmpDist_bp <- ecdf(lambda_bpB)
  p.value_bp <- 1 - EmpDist_bp(lambda_bp)

  testcrit_sp <- quantile(lambda_spB, probs = 0.95)
  EmpDist_sp <- ecdf(lambda_spB)
  p.value_sp <- 1 - EmpDist_sp(lambda_sp)
}else{
  df_bp <- p*k^2 + k + (k*(k + 1))/2
  df_sp <- p*k^2 + k

  # obtaining critical values and p-values for both tests
  testcrit_bp <- qchisq(p = 0.95, df = df_bp)
  p.value_bp <- 1 - unname(pchisq(lambda_bp, df = df_bp))

  testcrit_sp <- qchisq(p = 0.95, df = df_sp)
  p.value_sp <- 1 - unname(pchisq(lambda_sp, df = df_sp))
}


  chowTest <- list(lambda_bp = unname(lambda_bp),      # test statistic for break point test
                   testcrit_bp = testcrit_bp,  # critical value for 95% quantile
                   p.value_bp = p.value_bp,    # p-value
                   lambda_sp = unname(lambda_sp),      # test statistic for sample split
                   testcrit_sp = testcrit_sp,  # critical value for 95% quantile
                   p.value_sp = p.value_sp,     # p-value
                   SB = SB,                     # Structural breakpoint
                   SBcharacter = SBcharacter,   # Structural Break as character
                   p = p
  )
  class(chowTest) <- "chow"

  return(chowTest)

}
