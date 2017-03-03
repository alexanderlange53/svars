# likelihood function to optimize
LH <- function(S, Tob, TB, Sigma_hat1, k,  Sigma_hat2, restriction_matrix) {

  if(!is.null(restriction_matrix)){
    if(!is.matrix(restriction_matrix)){
      stop("Please provide a valid input matrix")
    }
    naElements <- is.na(restriction_matrix)
    restriction_matrix[naElements] <- S[naElements]
    W <- restriction_matrix
  }else{
    W <- matrix(S[1:(k*k)], nrow = k)
    restrictions = 0
  }

  Psi <- diag(S[((k*k+1) - restrictions):((k*k+k)-restrictions)])

  MW <- det(tcrossprod(W))
  MW2 <- det(W %*% tcrossprod(Psi, W))
  MMM <- tcrossprod(W)
  MMM2 <- W %*% tcrossprod(Psi, W)

  #if(MW > 0 & MW2 > 0){
    L <- suppressWarnings(-(((TB - 1) / 2) * (log(MW) + sum(diag((Sigma_hat1 %*% solve(MMM)))))) -
      (((Tob - TB + 1) / 2) * (log(MW2) + sum(diag((Sigma_hat2 %*% solve(MMM2)))))))
  return(-L)
  #}else{
  #  return(NA)
 # }

}
