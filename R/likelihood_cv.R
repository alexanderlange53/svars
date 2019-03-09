# likelihood function to optimize for identification via changes in volatility
likelihood_cv <- function(S, Tob, TB, Sigma_hat1, k,  Sigma_hat2, restriction_matrix, restrictions) {

  if(!is.null(restriction_matrix)){
    if(!is.matrix(restriction_matrix)){
      stop("Please provide a valid input matrix")
    }
    naElements <- is.na(restriction_matrix)
    toFillMatrix <- restriction_matrix
    toFillMatrix[naElements] <- S[1:sum(naElements)]
    W <- toFillMatrix
  }else{
    W <- matrix(S[1:(k*k)], nrow = k)

    restrictions <- 0
  }

  Psi <- diag(S[((k*k+1) - restrictions):((k*k+k)-restrictions)])

  MW <- det(tcrossprod(W))
  MW2 <- det(W %*% tcrossprod(Psi, W))
  MMM <- tcrossprod(W)
  MMM2 <- W %*% tcrossprod(Psi, W)

  if(any(Psi < 0) | MW < 0.01 |  MW2 < 0.01){
    return(1e25)
  }

  L <- suppressWarnings(-(K*Tob)/2*log(2*pi) -(((TB - 1) / 2) * (log(MW) + sum(diag((Sigma_hat1 %*% solve(MMM)))))) -
                          (((Tob - TB + 1) / 2) * (log(MW2) + sum(diag((Sigma_hat2 %*% solve(MMM2)))))))
  return(-L)

}
