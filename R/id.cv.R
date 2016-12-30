#' Changes in Volatility Identification
#'
#' Identify B matrix based on changes in Volatility.
#'
#' @param x VAR-object. (S)VAR model to determine B matrix for
#' @param SB integer. Structural break either of type integer as the number of observations which belong to the pre-break period or
#'                    Date character. If a date character is provided, either a date Vector which contains the time line of the data
#'                    in corresponding format or then the conventional time parameters need to be provided.
#' @param dateVector vector. Vector of the time period concerned containing SB
#' @param start character. Start of the time series (only if dateVector is empty)
#' @param end character. End of the time series (only if dateVector is empty)
#' @param frequency character. Frequency of the time series (only if dateVector is empty)
#' @param format character. Date format (only if dateVector is empty)
#' @return A list of results
#'
#' @export


#--------------------------------------------#
## Identification via changes in volatility ##
#--------------------------------------------#

# x  : object of class VAR
# SB : Structural Break


id.cv <- function(x, SB, start = NULL, end = NULL, frequency = NULL,
                        format = NULL, dateVector = NULL, max.iter = 10, crit = 0.05){

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

  # Determine starting values for B and Lambda
  B <- sqrtm((1/Tob)*crossprod(u_t)) + matrix(runif(k*k), nrow = k, byrow = T)
  Lambda <- c(1,1,1)
  S <- c(cbind(B, Lambda))

  # optimize the likelihood function

    MLE <- tryCatch(
      optim(fn = LH, par = S, k = k, TB = TB, Sigma_hat1 = Sigma_hat1,
                 Sigma_hat2 = Sigma_hat2, Tob = Tob, method = 'L-BFGS-B', hessian = T),
    error = function(e) NULL)

  if(!is.null(MLE)){
    B_hat <- matrix(MLE$par[1:(k*k)], nrow = k)
    Lambda_hat <- diag(MLE$par[(k*k+1):(k*k+k)])

    # estimating again with GLS to obatin a more precise estimation
    y <- t(x$y)

    Z_t <- matrix(0, nrow(y)*p, ncol(y))
    for(i in 1:(ncol(y)-p)){
      Z_t[,i] <- c(y[, ((i+1):(i+p))])
    }

    gls1 <- function(Z, Sig){
      G <- kronecker(tcrossprod(Z), Sig)
      return(G)
    }

    resid.gls <- function(Z_t, k, GLS_hat){
      term1 <- kronecker(t(Z_t), diag(k))%*%GLS_hat
      return(term1)
    }

    Lambda_hat <- list(Lambda_hat)
    B_hat <- list(B_hat)

    counter <- 1
    Exit <- 1

    while(abs(Exit) > crit & counter < max.iter){

      Sig1 <- solve(tcrossprod(B_hat[[counter]]))
      Sig2 <- solve(B_hat[[counter]]%*%tcrossprod(Lambda_hat[[counter]], B_hat[[counter]]))

      GLS1.1 <- rowSums(apply(Z_t[, 1:(TB-1)], 2, gls1, Sig = Sig1))
      GLS1.2 <- rowSums(apply(Z_t[, (TB):ncol(Z_t)], 2, gls1, Sig = Sig2))
      GLS1 <- solve(matrix(GLS1.1 + GLS1.2, nrow = k*k*p, byrow = F))

      GLS2.1 <- matrix(0, nrow = k*k*p, ncol = (TB-1))
      GLS2.2 <- matrix(0, nrow = k*k*p, ncol = ncol(y))

      for(i in 1:(TB-1)){
        GLS2.1[,i] <- kronecker(Z_t[,i], Sig1)%*%y[,i]
      }
      for(i in TB:ncol(y)){
        GLS2.2[,i] <- kronecker(Z_t[,i], Sig2)%*%y[,i]
      }


      GLS2.1 <- rowSums(GLS2.1)
      GLS2.2 <- rowSums(GLS2.2)
      GLS2 <- GLS2.1 + GLS2.2

      GLS_hat <- GLS1%*%GLS2

      term1 <- apply(Z_t, 2, resid.gls, k = k, GLS_hat = GLS_hat)
      u_tgls <- t(y) - t(term1)

      resid1gls <- u_tgls[1:TB-1,]
      resid2gls <- u_tgls[TB:Tob,]
      Sigma_hat1gls <- (crossprod(resid1gls)) / (TB-1)
      Sigma_hat2gls <- (crossprod(resid2gls)) / (Tob-TB+1)

      # Determine starting values for B and Lambda
      B <- expm::sqrtm((1/Tob)* crossprod(u_tgls)) + matrix(runif(k*k), nrow = k, byrow = T)
      Lambda <- diag(Lambda_hat[[counter]])
      S <- c(cbind(B, Lambda))

      # optimize the likelihood function
      MLEgls <- optim(fn = LH, par = S, k = k, TB = TB, Sigma_hat1 = Sigma_hat1gls,
                      Sigma_hat2 = Sigma_hat2gls, Tob = Tob, method = 'L-BFGS-B', hessian = T)



      B_hatg <- matrix(MLEgls$par[1:(k*k)], nrow = k)
      Lambda_hatg <- diag(MLEgls$par[(k*k+1):(k*k+k)])

      B_hat <- c(B_hat, list(B_hatg))
      Lambda_hat <- c(Lambda_hat, list(Lambda_hatg))

      counter <- counter +1
      Exit <- sum(diag(Lambda_hat[[counter]])) - sum(diag(Lambda_hat[[counter - 1]]))
    }

    # extracting the last estimates
    B_hat <- B_hat[[counter]]
    Lambda_hat <- Lambda_hat[[counter]]

    # obtaining standard errors from inverse fisher information matrix
    HESS <- solve(MLEgls$hessian)

    for(i in 1:nrow(HESS)){
      if(HESS[i,i] < 0){
        HESS[,i] <- -HESS[,i]
      }
    }

    FishObs <- sqrt(diag(HESS))
    B.SE <- matrix(FishObs[1:(k*k)], k, k)
    Lambda.SE <- FishObs[(k*k+1):(k*k+k)]*diag(k)
  }

    # ordering the columns with respect to the largest absolute values in each column
    B_hat_ord <- matrix(0, k, k)
    control <- rep(0, k)
    for(i in 1:ncol(B_hat)){
      for(j in 1:ncol(B_hat)){
        if(which.max(abs(B_hat[, j])) == i){
          if(control[i] == 0){
            control[i] <- j
            B_hat_ord[, i] <-B_hat[, j]
          }else{
            if(max(B_hat[, j]) > max(B_hat[, control[i]])){
              control[i] <- j
              B_hat_ord[, i] <-B_hat[, j]
            }
          }
        }
      }
    }

    # checking if any column in the orderd matrix is empty and replace it with the unused column in the estimated matrix
    if(any(control == 0)){
      hc <- sapply(1:k, function(x){any(x == control)})
      B_hat_ord[, which(control == 0)] <- B_hat[, which(hc == FALSE)]
      control[which(control == 0)] <- which(hc == FALSE)
    }

    # checking for negative values on the main daigonal
    for(i in 1:ncol(B_hat_ord)){
      if(B_hat_ord[i,i] < 0){
        B_hat_ord[, i] <- B_hat_ord[, i]*(-1)
      }
      if(Lambda_hat[i,i] < 0){
        Lambda_hat[, i] <- Lambda_hat[, i]*(-1)
      }
    }

    # reordering the lambda and S.E. matrices in the same way
    B.SE <- B.SE[, control]
    Lambda_hat <- diag(diag(Lambda_hat[, control]))
    Lambda.SE <- diag(diag(Lambda.SE[, control]))

    # Testing the estimated SVAR for identification by menas of wald statistic
    wald <- wald.test(Lambda_hat, HESS)

  result <- list(
                 Lambda = Lambda_hat,    # estimated Lambda matrix (unconditional heteroscedasticity)
                 Lambda_SE = Lambda.SE,  # standard errors of Lambda matrix
                 B = B_hat_ord,          # estimated B matrix (unique decomposition of the covariance matrix)
                 B_SE = B.SE,            # standard errors of B matrix
                 n = Tob,                # number of observations
                 Fish = HESS,            # observerd fisher information matrix
                 Lik = -MLEgls$value,    # function value of likelihood
                 wald_statistic = wald,  # results of wald test
                 iteration = counter,     # number of gls estimations
                 method = "Changes in Volatility",
                 SB = SB,                # Structural Break in number format
                 SBcharacter             # Structural Break in input character format
                 )
  class(result) <- "svarIdent"
  return(result)
}
