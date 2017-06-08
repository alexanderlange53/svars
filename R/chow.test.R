#' Chow Test for Structural Break
#'
#' The Chow test is applied to a time series with a presupposed structural break.
#'
#' @param Y Data
#' @param SB integer. Structural break: either of type integer (number of observations in the pre-break period) or
#'                    date character. If a date character is provided, either a date vector containing the whole time line
#'                    in the corresponding format or conventional time parameters need to be provided
#' @param nboot ??
#' @param lags  Maximum number of lag order
#' @param dateVector vector. Vector of all time periods containing SB in corresponding format
#' @param start character. Start of the time series (only if dateVector is empty)
#' @param end character. End of the time series (only if dateVector is empty)
#' @param frequency character. Frequency of the time series (only if dateVector is empty)
#' @param format character. Date format (only if dateVector is empty)
#'
#' @return A list with elements
#' \item{lambda_bp}{Test statistic of the Chow test with break point}
#' \item{testcrit_bp}{Critical value of the test statistic lambda_bp}
#' \item{p.value_bp}{p-value of the test statistic lambda_bp}
#' \item{lambda_sp}{Test statistic of the Chow test with sample split}
#' \item{testcrit_sp}{Critival value of the test statistic lambda_sp}
#' \item{p.value_sp}{p-value of the test statistic lambda_sp}
#'
#' @references
#'
#' @export
#'

## Chow tests of system structural break ##
#-----------------------------------------#

# Y     : data
# SB    : structural break
# nboot : number of bootstrap iterations
# lags  : maximum lag order

chow.test <- function(Y, SB, p, nboot = 500, lags = 12, start = NULL, end = NULL,
                      frequency = NULL, format = NULL, dateVector = NULL){
  # Null Hypothesis of no Sample Split is rejected for large lambda

  if(!is.numeric(SB)){

    SB <- getStructuralBreak(SB = SB, start = start, end = end,
                             frequency = frequency, format = format, dateVector = dateVector)
  }


  Full <- Y

  # splitting sample
  sample1 <- Y[1:SB, ]
  sample2 <- Y[(SB+1):nrow(Y), ]



  # estimating VAR for pre and post SB and for full series
  VAR.model <- VAR(Full, p = p)
  VAR1.model <- VAR(sample1, p = p)
  VAR2.model <- VAR(sample2, p = p)

  l1 <- VAR1.model$obs
  l2 <- VAR2.model$obs
  ll <- VAR.model$obs

  # calculating three covariance matrices
  Sigma.1 <- (1/l1)*t(residuals(VAR1.model))%*%(residuals(VAR1.model))
  Sigma.2 <- (1/l2)*t(residuals(VAR2.model))%*%(residuals(VAR2.model))

  Sigma <- (1/l1)*t(residuals(VAR.model)[1:l1,])%*%(residuals(VAR.model)[1:l1,]) +
    (1/l2)*t(residuals(VAR.model)[(ll-l2+1):ll,])%*%(residuals(VAR.model)[(ll-l2+1):ll,])

  # calculating the test statistic
  # lambda_bp : teststatistic for break point (tests for change in covariance in addition)
  # lambda_SP : teststatistic for structural break (tests only for parameter change)
  lambda_bp <- (l1 + l2)*log(det(Sigma)) - l1*log(det(Sigma.1)) - l2*log(det(Sigma.2))
  lambda_sp <- (l1 + l2)*(log(det(Sigma)) - log(det((1/(l1 + l2))*(l1*Sigma.1 + l2*Sigma.2))))


    lambda_bpB <- rep(NA, nboot)
    lambda_spB <- rep(NA, nboot)
    TB <- l1

    # bootstrapping the teststatistic to obtain empirical distribution
    # obtaining VAR parameter
    coef_x <- coef(VAR.model)
    type <- VAR.model$type

    A <- matrix(0, nrow = VAR.model$K, ncol = VAR.model$K* p)
    for(i in 1:VAR.model$K){
      A[i,] <- coef_x[[i]][1:(VAR.model$K*p),1]
    }
    A_hat <- A
    if(type == 'const'){
      v <- rep(1, VAR.model$K)
      for(i in 1:VAR.model$K){
        v[i] <- coef_x[[i]][(VAR.model$K*p+1), 1]
      }
      A_hat <- cbind(v, A)
    }

    residF <- residuals(VAR.model)

    # creating new error terms
    errors <- list()
    for(i in 1:nboot){
      my <- rnorm(n = ncol(Y))
      if (radermacher == TRUE) {
        my <- (my > 0) - (my < 0)
      }
      errors[[i]] <- u * my
    }

    for(i in 1:nboot){

      BootDataF <- DataGen(A_hatF, Full, residF, sel$selection[1], TB)
      BootData1 <- BootDataF[1:(l1+sel$selection[1]),]
      BootData2 <- BootDataF[(l1+sel$selection[1]+1):nrow(BootDataF),]

      VARB.model <- VAR(BootDataF, p = sel$selection[1])
      VARB1.model <- VAR(BootData1, p = sel$selection[1])
      VARB2.model <- VAR(BootData2, p = sel$selection[1])

      Sigma.1 <- (1/l1)*t(residuals(VARB1.model))%*%(residuals(VARB1.model))
      Sigma.2 <- (1/l2)*t(residuals(VARB2.model))%*%(residuals(VARB2.model))

      Sigma <- (1/l1)*t(residuals(VARB.model)[1:l1,])%*%(residuals(VARB.model)[1:l1,]) +
        (1/l2)*t(residuals(VARB.model)[(ll-l2+1):ll,])%*%(residuals(VARB.model)[(ll-l2+1):ll,])

      lambda_bpB[i] <- (l1 + l2)*log(det(Sigma)) - l1*log(det(Sigma.1)) - l2*log(det(Sigma.2))
      lambda_spB[i] <- (l1 + l2)*(log(det(Sigma)) - log(det((1/(l1 + l2))*(l1*Sigma.1 + l2*Sigma.2))))

      progress(i, nboot)
    }

    K <- VAR.model$K
    df_bp <- sel$selection[1]*K^2 + K + (K*(K + 1))/2
    df_sp <- sel$selection[1]*K^2 + K

    # obtainind critical values an p-values for both tests
    testcrit_bp <- quantile(lambda_bpB, probs = 0.95)
    EmpDist_bp <- ecdf(lambda_bpB)
    p.value_bp <- 1 - EmpDist_bp(lambda_bp)

    testcrit_sp <- quantile(lambda_spB, probs = 0.95)
    EmpDist_sp <- ecdf(lambda_spB)
    p.value_sp <- 1 - EmpDist_sp(lambda_sp)

    return(list(lambda_bp, testcrit_bp, p.value_bp,
                lambda_sp, testcrit_sp, p.value_sp))

}
