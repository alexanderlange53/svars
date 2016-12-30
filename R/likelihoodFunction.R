# likelihood function to optimize
LH <- function(S, Tob, TB, Sigma_hat1, k,  Sigma_hat2) {

  W <- matrix(S[1:(k*k)], nrow = k)
  Psi <- diag(S[(k*k+1):(k*k+k)])

  MW <- det(tcrossprod(W))
  MW2 <- det(W %*% tcrossprod(Psi, W))
  MMM <- tcrossprod(W)
  MMM2 <- W %*% tcrossprod(Psi, W)

  if(MW > 0 & MW2 > 0){
    L <- -(((TB - 1) / 2) * (log(MW) + sum(diag((Sigma_hat1 %*% solve(MMM)))))) -
      (((Tob - TB + 1) / 2) * (log(MW2) + sum(diag((Sigma_hat2 %*% solve(MMM2))))))
  return(-L)
  }else{
    return(1000000000)
  }

}
