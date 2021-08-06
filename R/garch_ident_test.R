garch_ident_test <- function(x) {

   u <- residuals(x$VAR)

   result <- data.frame(matrix(NA, nrow = (x$K-1), ncol = 9))
   hypothesis <- paste("r=", 1,sep = "")

   for (r in 1:(x$K-1)) {
     A2 <- matrix(solve(x$B)[(r+1):x$K,], ncol = x$K)

     # First test Q_1 test statistic
     AA <- crossprod(A2)
     csi <- rep(NA, nrow(u))

     for (i in 1:nrow(u)) {
       csi[i] <- u[i,] %*% AA %*% u[i,]
     }

     csi <- csi - mean(csi)

     gamma_1 <- crossprod(csi[-1], csi[-length(csi)])/length(csi)
     gamma_0 <- crossprod(csi)/length(csi)

     Q_1 <- length(csi)*(gamma_1/gamma_0)^2
     p_val_q_1 <- 1 - pchisq(Q_1, 1)
     dof_q1 <- 1

     # Second test Q_2 test statistic
     eta <- matrix(NA, nrow(u), 0.5*(x$K - r)*(x$K- r +1))
     for (i in 1:nrow(u)) {
       tempA <-A2 %*% tcrossprod(u[i,]) %*% t(A2)
       eta[i,] <- tempA[lower.tri(tempA , diag = TRUE)]
     }

     eta_m <- matrix(NA, nrow(eta), ncol(eta))

     for(i in 1:ncol(eta)) {
       eta_m[,i] <- eta[,i] - colMeans(eta)[i]
     }
     #eta_m <- eta - colMeans(eta)
     gam_1 <- crossprod(eta_m[-1,], eta_m[-nrow(eta_m),])/nrow(eta_m)
     gam_0 <- crossprod(eta_m, eta_m)/nrow(eta_m)

     Q_2 <- sum(diag(t(gam_1) %*% solve(gam_0) %*% gam_1 %*% solve(gam_0))) * nrow(u)
     p_val_q_2 <- 1 - pchisq(Q_2, 0.25*((x$K - r)^2*(x$K - r+1)^2))
     dof_q2 <- 0.25*((x$K - r)^2*(x$K - r+1)^2)

     # Third LM test
     if (ncol(eta) == 1) {
       lm_model <- lm(eta[-1,] ~ eta[-nrow(eta),])
     } else {
       lm_model <- suppressWarnings(VAR(eta, p = 1, type = 'const'))
     }
     Sigam_lm <- crossprod(residuals(lm_model)) / (nrow(eta) - 1)
     LM_1 <- 0.5 * nrow(u) * (x$K - r)*(x$K - r+1) - nrow(u) * sum(diag(Sigam_lm%*%solve(gam_0)))
     p_val_lm_1 <- 1 - pchisq(LM_1, 0.25*((x$K - r)^2*(x$K - r+1)^2))
     dof_lm1 <- 0.25*((x$K - r)^2*(x$K - r+1)^2)



     result[r,] <- c(Q_1, dof_q1, p_val_q_1, Q_2, dof_q2, p_val_q_2, LM_1,dof_lm1, p_val_lm_1)
     if (r>1) {
       hypothesis <- rbind(hypothesis, paste("r=", r, sep = ""))
     }
   }
   colnames(result) = c("Q_1", "dof", "p-value", 'Q_2', "dof", "p-value", 'LM_1', "dof", "p-value")
   rownames(result) <- hypothesis


  return(result)
}
