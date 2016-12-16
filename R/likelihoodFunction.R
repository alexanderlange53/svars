# likelihood function to optimize
LH <- function(S, Tob, TB, Sigma_hat1, k,  Sigma_hat2) {

  W <- matrix(S[1:(k*k)], nrow = k)
  Psi <- diag(S[(k*k+1):(k*k+k)])
  MW <- det(W %*% t(W))
  MW2 <- det(W %*% Psi %*% t(W))
  MMM <- W %*% t(W)
  MMM2 <- W %*% Psi %*% t(W)

  if(MW > 0 & MW2 > 0){
    L <- -(((TB - 1) / 2) * (log(MW) + sum(diag((Sigma_hat1 %*% solve(MMM)))))) -
      (((Tob - TB + 1) / 2) * (log(MW2) + sum(diag((Sigma_hat2 %*% solve(MMM2))))))
  return(-L)
  }else{
    return(NA)
  }

}
