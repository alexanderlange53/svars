garch_ident_test <- function(x, r) {

  CC <- x$CC
  B <- x$B
  R1 <- (solve(CC)%*%B[,1:r])
  u <- residuals(x$VAR)

  # Calculate orthogonal complement of R1
  R1_comp <- Null(R1)#qr.Q(qr(t(R1)),complete=TRUE)

  R2 <- R1_comp %*% solve(sqrtm(t(R1_comp) %*% R1_comp))

  A2 <- t(R2) %*% solve(CC)

  # First test Q_1 test statistic
  AA <- crossprod(A2)
  csi <- rep(NA, nrow(u))
  for (i in 1:nrow(u)) {
    csi[i] <- u[i,] %*% AA %*% u[i,]
  }

  csi <- csi - mean(csi)

  gamma_1 <- crossprod(csi[-1], csi[-length(csi)])/length(csi)
  gamma_0 <- crossprod(csi, csi)/length(csi)

  Q_1 <- (gamma_1/gamma_0)^2*length(csi)
  p_val_q_1 <- 1 - pchisq(Q_1, 1)

  # Second test Q_2 test statistic
  eta <- matrix(NA, nrow(u), 0.5*(x$K - r)*(x$K- r +1))
  for (i in 1:nrow(u)) {
    tempA <-A2 %*% tcrossprod(u[i,]) %*% t(A2)
    eta[i,] <- tempA[lower.tri(tempA , diag = TRUE)]
  }

  eta_m <- eta - colMeans(eta)
  gam_1 <- crossprod(eta_m[-1,], eta_m[-nrow(eta_m),])/nrow(eta_m)
  gam_0 <- crossprod(eta_m, eta_m)/nrow(eta_m)

  Q_2 <- sum(diag(t(gam_1) %*% solve(gam_0) %*% gam_1 %*% solve(gam_0))) * nrow(u)
  p_val_q_2 <- 1 - pchisq(Q_1, 0.25*((x$K - r)^2*(x$K - r+1)^2))

  # Third LM test
  lm_model <- lm(eta[-1,] ~ eta[-nrow(eta),])
  Sigam_lm <- crossprod(residuals(lm_model)) / nrow(eta)-1
  LM_1 <- 0.5 * nrow(u) * (x$K - r)*(x$K - r+1) - nrow(u) * sum(diag(Sigam_lm%*%solve(gam_0)))
  p_val_lm_1 <- 1 - pchisq(LM_1, 0.25*((x$K - r)^2*(x$K - r+1)^2))



  result <- data.frame(test.stat = c(Q_1, Q_2, LM_1), p.value = c(p_val_q_1, p_val_q_2, p_val_lm_1))
  colnames(result) = c("Test statistic", "p-value")
  rownames(result) <- c('Q_1', 'Q_2', 'LM_1')

  return(result)
}
