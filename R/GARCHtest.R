GARCHtest <- function(CC, r CovMat, u, h){

  R <- solve(CC) %*% CovMat

  R2 <- Null(R[,1:r]) %*% solve(sqrtm(t(Null(R[,1:r])) %*% Null(R[,1:r])))


  omega2 <- t(R2) %*% solve(CC) %*% t(u)

  #varepsilon <- matrix(NA, nrow = NROW(omega2), ncol = NCOL(omega2))
  varepsilon <- rep(NA, times = NCOL(omega2))

  for (i in 1:NCOL(omega2)) {
    varepsilon[i] <- crossprod(omega2[,i]) - sum(tcrossprod(omega2))/nrow(u)
  }

  # alternative vareps
  et2 <- solve(x1$B) %*% t(u)
  et2 <- et2[1:(k-r), ]

  varepsilon <- rep(NA, times = NCOL(et2))

  for (i in 1:NCOL(et2)) {
    varepsilon[i] <- crossprod(et2[,i]) - sum(tcrossprod(et2))/nrow(u)
  }


  qsiM <- tcrossprod(omega2)
  qsi <- qsiM[lower.tri(qsiM, diag = TRUE)] - qsiM[lower.tri(qsiM, diag = TRUE)]/nrow(u)

  gamma0 <- crossprod(varepsilon)/NCOL(omega2)

  gammah <- rep(0, times = NCOL(omega2))
  for (i in (1 + h):NCOL(omega2)){
    gammah[i] <- varepsilon[i] * varepsilon[i - h]
  }

  gammah <- sum(gammah)/NCOL(omega2)

  Q1 <- NCOL(omega2) * (gammah/gamma0)^2

}

