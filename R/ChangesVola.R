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
  Sigma_hat1 <- (t(resid1) %*% resid1) / (TB-1)
  Sigma_hat2 <- (t(resid2) %*% resid2) / (Tob-TB+1)

  # Determine starting values for B and Lambda
  B <- sqrtm((1/Tob)*t(u_t) %*% (u_t)) + matrix(runif(k*k), nrow = k, byrow = T)
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
  return(list(
    Lambda = Lambda_hat,
    Lambda_SE = Lambda.SE,
    B = B_hat,
    B_SE = B.SE,
    n = Tob,
    InvFish = HESS,
    Lik = -MLE$value
  ))
}
