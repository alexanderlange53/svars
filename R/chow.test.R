#' Chow Test for Structural Break
#'
#' The Chow test is applied to a time series with a presupposed structural break.
#'
#' @param Y Data of multivariate time series
#' @param SB Integer or date character. The structural break is specified either by an integer (number of observations in the pre-break period) or
#'                    a date character. If a date character is provided, either a date vector containing the whole time line
#'                    in the corresponding format (see examples) or common time parameters need to be provided
#' @param nboot Number of bootrstrap iterations to calculate quantiles and p-values
#' @param lags  Maximum number of lag order
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
#' \item{testcrit_sp}{Critival value of the test statistic lambda_sp}
#' \item{p.value_sp}{p-value of the test statistic lambda_sp}
#'
#' @references Lütkepohl, H., 2005. New introduction to multiple time series analysis Springer-Verlag, Berlin.
#'
#' @export
#'

## Chow tests of system structural break ##
#-----------------------------------------#

# Y     : data
# SB    : structural break
# nboot : number of bootstrap iterations
# lags  : maximum lag order

chow.test <- function(Y, SB, p, nboot = 500, start = NULL, end = NULL,
                      frequency = NULL, format = NULL, dateVector = NULL){
  # Null hypothesis of no sample split is rejected for large values of lambda

  if(!is.numeric(SB)){

    SB <- getStructuralBreak(SB = SB, start = start, end = end,
                             frequency = frequency, format = format, dateVector = dateVector)
  }

  # function to create Z matrix
  y_lag_cr <- function(y, lag_length){
    # create matrix that stores the lags
    y_lag <- matrix(NA, dim(y)[1],dim(y)[2]*lag_length)
    for (i in 1:lag_length) {
      y_lag[(1+i):dim(y)[1],((i*NCOL(y)-NCOL(y))+1):(i*NCOL(y))] <- y[1:(dim(y)[1]-i),(1:NCOL(y))]
    }
    # drop first observation
    y_lag <- as.matrix(y_lag[-(1:lag_length),])
    out <- list(lags = y_lag)
  }

  sqrt.f <- function(Pstar, Sigma_u_star){
    yy <- suppressMessages(sqrtm(Sigma_u_hat_old))%*%solve(suppressMessages(sqrtm(Sigma_u_star)))%*%Pstar
    return(yy)
  }

  Y <- as.matrix(Y)
  Full <- Y

  # splitting sample
  sample1 <- Y[1:SB, ]
  sample2 <- Y[(SB+1):nrow(Y), ]

  # selecting VAR orders
  sel <- VARselect(Full, lag.max = lags)
  if(any(sel$criteria[,sel$selection[1]] == -Inf) | any(is.na(sel$criteria[,sel$selection[1]]))){
    sel$selection[1] <- sel$selection[1] - 2
  }
  if(sel$selection[1] == 1){
    sel$selection[1] <- sel$selection[1] + 1
  }


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
  # lambda_bp : test statistic for break point (tests for change in covariance in addition)
  # lambda_SP : test statistic for structural break (tests only for parameter change)
  lambda_bp <- (l1 + l2)*log(det(Sigma)) - l1*log(det(Sigma.1)) - l2*log(det(Sigma.2))
  lambda_sp <- (l1 + l2)*(log(det(Sigma)) - log(det((1/(l1 + l2))*(l1*Sigma.1 + l2*Sigma.2))))


    lambda_bpB <- rep(NA, nboot)
    lambda_spB <- rep(NA, nboot)
    TB <- l1


    # bootstrapping the test statistic to obtain empirical distribution
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

    Z <- t(y_lag_cr(Y, p)$lags)
    Z <-rbind(rep(1, ncol(Z)), Z)

    residF <- residuals(VAR.model)

    # creating new data
    datFull <- list()

    for(i in 1:nboot){
      u <- residF
      my <- rnorm(n = ncol(Y))
      if (radermacher == TRUE) {
        my <- (my > 0) - (my < 0)
      }
      et <- u * my
      Ystar <- t(A_hat %*% Z + t(et))
      datFull[[i]] <- Ystar
    }

    for(i in 1:nboot){
      xx <- datFull[[i]]
      # splitting sample
      sample1 <- xx[1:SB, ]
      sample2 <- xx[(SB+1):nrow(xx), ]

      VARB.model <- VAR(xx, p = p)
      VARB1.model <- VAR(sample1, p = p)
      VARB2.model <- VAR(sample2, p = p)
      ll <- nrow(xx) - p

      Sigma.1 <- (1/l1)*t(residuals(VARB1.model))%*%(residuals(VARB1.model))
      Sigma.2 <- (1/l2)*t(residuals(VARB2.model))%*%(residuals(VARB2.model))

      Sigma <- (1/l1)*t(residuals(VARB.model)[1:l1,])%*%(residuals(VARB.model)[1:l1,]) +
        (1/l2)*t(residuals(VARB.model)[(ll-l2+1):ll,])%*%(residuals(VARB.model)[(ll-l2+1):ll,])

      lambda_bpB[i] <- (l1 + l2)*log(det(Sigma)) - l1*log(det(Sigma.1)) - l2*log(det(Sigma.2))
      lambda_spB[i] <- (l1 + l2)*(log(det(Sigma)) - log(det((1/(l1 + l2))*(l1*Sigma.1 + l2*Sigma.2))))
    }



    K <- VAR.model$K
    df_bp <- p*K^2 + K + (K*(K + 1))/2
    df_sp <- p*K^2 + K

    # obtaining critical values and p-values for both tests
    testcrit_bp <- quantile(lambda_bpB, probs = 0.95)
    EmpDist_bp <- ecdf(lambda_bpB)
    p.value_bp <- 1 - EmpDist_bp(lambda_bp)

    testcrit_sp <- quantile(lambda_spB, probs = 0.95)
    EmpDist_sp <- ecdf(lambda_spB)
    p.value_sp <- 1 - EmpDist_sp(lambda_sp)

    return(list(lambda_bp,    # Teststatistik for break point test
                testcrit_bp,  # critical value for 95% quantile
                p.value_bp,   # p-value
                lambda_sp,    # teststatistik for sample split
                testcrit_sp,  # critical value for 95% quantile
                p.value_sp    # p-value
                ))

}
