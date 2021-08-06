cv_ident_test <- function(x) {

  y <- x$y
  p <- x$p
  type <- x$type

  yl <- t(YLagCr(y, p))
  y <- y[(p+1):nrow(y),]

  Lambda_ord <- diag(sort(diag(x$Lambda), decreasing = TRUE))

  if (length(x$SB) == x$n & length(x$SB) > 3) {
    SB_out <- x$SB
    TB <- x$n - sum(x$SB) + 1
    regime_realization <- rep(1, x$n)
    regime_realization[x$SB == 1] <- 2
  } else {
    SB_out <- x$SB
    TB <- x$SB - x$p
    regime_realization <- rep(1, x$n)
    regime_realization[TB:x$n] <- 2
  }


  if(type == 'const'){
    Z_t <- rbind(rep(1, ncol(yl)), yl)
  }else if(type == 'trend'){
    Z_t <- rbind(seq(p + 1, nrow(x$y)), yl)
  }else if(type == 'both'){
    Z_t <- rbind(rep(1, ncol(yl)), seq(p + 1, nrow(x$y)), yl)
  }else{
    Z_t <- yl
  }

  Y_fit <- t(matrix(kronecker(t(Z_t), diag(x$K)) %*% c(x$A_hat), x$K, ncol(Z_t)))
  resid <- y - Y_fit

  resid1 <- resid[regime_realization==1,]
  resid2 <- resid[regime_realization==2,]

  Sigma_hat1 <- (crossprod(resid1)) / (TB - 1)
  Sigma_hat2 <- (crossprod(resid2)) / (x$n - TB + 1)

  test_statistic_all <- NA
  p_value_all <- NA
  dof_all <-NA
  H.null <- NA


  z_m <- matrix(0, x$K, 2)
  w_m <- matrix(0, x$K, 2)
  kappa <- rep(0, 2)
  for(regime in 1:2) {
    for (k in 1:x$K) {
      z_m[k, regime] <- sum((resid[regime_realization == regime, k] - mean(resid[regime_realization == regime, k]))^4)
      if (regime == 1) {
        sigma <- Sigma_hat1
        z_m[k, regime] <- z_m[k, regime] - 6 * sigma[k,k]^2
        z_m[k, regime] <- z_m[k, regime]/(TB - 4)
        w_m_t <- TB/(TB - 1)
        w_m[k, regime] <- w_m_t*(sigma[k,k]^2 - z_m[k, regime]/TB)
      } else {
        sigma <- Sigma_hat2
        z_m[k, regime] <- z_m[k, regime] - 6 * sigma[k,k]^2
        z_m[k, regime] <- z_m[k, regime]/(x$n - TB - 4)
        w_m_t <- (x$n - TB)/((x$n - TB)- 1)
        w_m[k, regime] <- w_m_t*(sigma[k,k]^2 - z_m[k, regime]/(x$n - TB))
      }

    }
    kappa[regime] <- 1/(3 * x$K) * sum(z_m[, regime] / w_m[, regime]) - 1
  }
  tau <- TB / (x$n - p)
  c_tau2 <- solve((1 + kappa[1]) / tau + ( (1 + kappa[2]) /(1-tau)))
  c_tau2_nk <- solve(1/ tau + (1/(1-tau)))

    for (s in 0:(x$K-1)) {
      if (s<(x$K-1)) {
        for (r in 2:(x$K-s)) {
          lambda_sum <- 0
          lambda2 <- Lambda_ord
          lambda_sum_r <- 0
          for (n in (s+1):(s+r)) {
            lambda_sum <- lambda_sum + log(lambda2[n, n])
            lambda_sum_r <- lambda_sum_r + lambda2[n,n]
          }
          lambda_sum_r <- log(lambda_sum_r / r)

          Qr <- c_tau2 * (-1*(x$n-p) * lambda_sum + (x$n-p) * r * lambda_sum_r )
          Qr_nk <- c_tau2_nk * (-1*(x$n-p) * lambda_sum + (x$n-p) * r * lambda_sum_r )
          degrees_of_freedom <- 0.5 * (r+2) * (r-1)

          Qr_star <- 2 * Qr / ((r+2)*(r-1))
          p_value_star <- 1 - df(Qr_star, 0.5*(r+2)*(r-1), (x$n-p - x$K*p - 1))

          p_val <- 1 -pchisq(Qr, degrees_of_freedom)
          p_val_nk <- 1 - pchisq(Qr_nk, degrees_of_freedom)

          test_statistic_all <- c(test_statistic_all, Qr)
          p_value_all <-c(p_value_all, p_val)
          dof_all <- c(dof_all, degrees_of_freedom)

          subs <- (s+1):(s+r)
          hypothesis <- paste("lambda_", subs[1], '=')
          for (ss in 2:length(subs)) {
            if (ss < length(subs)) {
              hypothesis <- paste(hypothesis, paste("lambda_", subs[ss], '=', sep = ""), sep = "")
            } else {
              hypothesis <- paste(hypothesis, paste("lambda_", subs[ss], sep = ""), sep = "")
            }
          }

          H.null <- rbind(H.null, hypothesis)
        }
      }
  }

  result <- data.frame(test.stat = na.omit(test_statistic_all), dof = na.omit(dof_all),
                       p.value = na.omit(p_value_all))

  colnames(result) = c("Test statistic", "dof", "p-value")
  rownames(result) <- na.omit(H.null)

  return(result)
}
