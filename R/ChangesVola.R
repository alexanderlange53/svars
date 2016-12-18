#--------------------------------------------#
## Identification via changes in volatility ##
#--------------------------------------------#

# x  : object of class VAR
# SB : Structural Break

library(expm)

ChangesVola <- function(x, SB){

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

    # obtaining standard errors from inverse fisher information matrix
    HESS <- solve(MLE$hessian)

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
      hc <- apply(B_hat_ord, 2, function(x){all(x == 0)})
      B_hat_ord[, which(hc == TRUE)] <- B_hat[, which(hc == TRUE)]
      control[which(control == 0)] <- which(hc == TRUE)
    }

    # checking for negative values on the main daigonal
    for(i in 1:ncol(B_hat_ord)){
      if(B_hat_ord[i,i] < 0){
        B_hat_ord[, i] <- B_hat_ord[, i]*(-1)
      }
    }

    # reordering the lambda and S.E. matrices in the same way
    B.SE <- B.SE[, control]
    Lambda_hat <- diag(diag(Lambda_hat[, control]))
    Lambda.SE <- diag(diag(Lambda.SE[, control]))

  return(list(
    Lambda = Lambda_hat,
    Lambda_SE = Lambda.SE,
    B = B_hat_ord,
    B_SE = B.SE,
    n = Tob,
    InvFish = HESS,
    Lik = -MLE$value
  ))
}
