#' Chow Test for Structural Break
#'
#' The Chow test is applied to a time series an presupposed point of structural break.
#'
#' @param Y Data
#' @param SB integer. Structural break either of type integer as the number of observations which belong to the pre-break period or
#'                    Date character. If a date character is provided, either a date Vector which contains the time line of the data
#'                    in corresponding format or then the conventional time parameters need to be provided.
#' @param nboot ??
#' @param lags  Maximum number of Lag order
#' @param dateVector vector. Vector of the time period concerned containing SB
#' @param start character. Start of the time series (only if dateVector is empty)
#' @param end character. End of the time series (only if dateVector is empty)
#' @param frequency character. Frequency of the time series (only if dateVector is empty)
#' @param format character. Date format (only if dateVector is empty)
#' @return A list of results
#'
#' @export
#'

## Chow tests of system structural break ##
#-----------------------------------------#

# Y     : Data
# SB    : Structural Break
# nboot : number of Bootstrap iterations
# lags  : maximum lag order

chow.test <- function(Y, SB, nboot = 500, lags = 12, start = NULL, end = NULL,
                      frequency = NULL, format = NULL, dateVector = NULL){
  # Null Hypothesis of no Sample Split is rejected for large lambda

  if(!is.numeric(SB)){

    SB <- getStructuralBreak(SB = SB, start = start, end = end,
                             frequency = frequency, format = format, dateVector = dateVector)
  }


  Full <- Y

  CoeffMat <- function(Var){
    nY <- Var$K
    nl <- Var$p
    if(nY == 2){
      A_hat <- t(cbind(coef(Var)[[1]][1:(nY*nl),1],
                       coef(Var)[[2]][1:(nY*nl),1]))
    }else if(nY == 3){
      A_hat <- t(cbind(coef(Var)[[1]][1:(nY*nl),1],
                       coef(Var)[[2]][1:(nY*nl),1], coef(Var)[[3]][1:(nY*nl),1]))
    }
    if(nY == 4){
      A_hat <- t(cbind(coef(Var)[[1]][1:(nY*nl),1],
                       coef(Var)[[2]][1:(nY*nl),1], coef(Var)[[3]][1:(nY*nl),1],
                       coef(Var)[[4]][1:(nY*nl),1]))
    }else{
      A_hat <- t(cbind(coef(Var)[[1]][1:(nY*nl),1],
                       coef(Var)[[2]][1:(nY*nl),1], coef(Var)[[3]][1:(nY*nl),1],
                       coef(Var)[[4]][1:(nY*nl),1], coef(Var)[[5]][1:(nY*nl),1]))
    }
    return(A_hat)
  }

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
  VAR.model <- VAR(Full, p = sel$selection[1])
  VAR1.model <- VAR(sample1, p = sel$selection[1])
  VAR2.model <- VAR(sample2, p = sel$selection[1])

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
    for(i in 1:nboot){
      A_hatF <- CoeffMat(VAR.model)
      residF <- residuals(VAR.model)
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
