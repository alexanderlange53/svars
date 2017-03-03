# likelihood function to optimize
LH <- function(S, Tob, TB, Sigma_hat1, k,  Sigma_hat2, restriction_matrix) {

  W <- matrix(S[1:(k*k)], nrow = k)

  if(!is.null(restriction_matrix)){
     if(!is.matrix(restriction_matrix)){
         stop("Please provide a valid input matrix")
      }
    inputValues <- !is.na(restriction_matrix)
    W[inputValues] <- restriction_matrix[inputValues]
      }

  Psi <- diag(S[(k*k+1):(k*k+k)])

  MW <- det(tcrossprod(W))
  MW2 <- det(W %*% tcrossprod(Psi, W))
  MMM <- tcrossprod(W)
  MMM2 <- W %*% tcrossprod(Psi, W)

  #if(MW > 0 & MW2 > 0){
    L <- -(((TB - 1) / 2) * (log(MW) + sum(diag((Sigma_hat1 %*% solve(MMM)))))) -
      (((Tob - TB + 1) / 2) * (log(MW2) + sum(diag((Sigma_hat2 %*% solve(MMM2))))))
  return(-L)
  #}else{
  #  return(NA)
 # }

}
