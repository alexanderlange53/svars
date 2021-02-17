id.cv_boot <- function(x, SB, SB2, max.iter = 50, crit = 0.001, restriction_matrix = NULL, Z = NULL){

  u <- Tob <- p <- k <- residY <- coef_x <- yOut <- type <- y <-  A_hat <- NULL
 get_var_objects(x)

 rmOut = restriction_matrix

 # check if varest object is restricted
 if(inherits(x,"varest")){
   if(!is.null(x$restrictions)){
     stop("id.cv currently supports identification of unrestricted VARs only. Consider using id.dc, id.cvm or id.chol instead.")
   }
 }

 # set up restrictions paassed by user

 restriction_matrix <- get_restriction_matrix(restriction_matrix, k)
 restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
  if(is.numeric(SB)){
    SBcharacter <- NULL
  }

 if (is.null(SB2)) {
   if (length(SB) == Tob) {
     SB_out <- SB
     TB <- Tob - sum(SB) + 1
     resid1 <- u[SB == 0,]
     resid2 <- u[SB,]
   } else {
     SB_out <- SB
     TB <- SB - p
     SB <- rep(0, Tob)
     SB[TB:Tob] <- 1
     resid1 <- u[1:TB - 1, ]
     resid2 <- u[TB:Tob, ]
   }

   Sigma_hat1 <- (crossprod(resid1)) / (TB - 1)
   Sigma_hat2 <- (crossprod(resid2)) / (Tob - TB + 1)
 } else {
     SB_out <- SB
     SB_out2 <- SB2
     TB1 <- SB - p
     TB2 <- SB2 - SB - p
     SB <- rep(0, Tob)
     SB[1:TB1] <- 1
     SB2 <- rep(0, Tob)
     SB2[(TB1+1):(TB1+TB2)] <- 1
     SB3 <- rep(0, Tob)
     SB3[(TB1+TB2+1):Tob] <- 1
     resid1 <- u[which(SB == 1), ]
     resid2 <- u[which(SB2 == 1), ]
     resid3 <- u[which(SB3 == 1), ]
     TB3 <- nrow(resid3)

   Sigma_hat1 <- (crossprod(resid1)) / nrow(resid1)
   Sigma_hat2 <- (crossprod(resid2)) / nrow(resid2)
   Sigma_hat3 <- (crossprod(resid3)) / nrow(resid3)
 }




  if(is.null(Z)){

    yl <- t(YLagCr(t(y), p))
    yret <- y
    y <- y[,-c(1:p)]

    if(type == 'const'){
      Z_t <- rbind(rep(1, ncol(yl)), yl)
    }else if(type == 'trend'){
      Z_t <- rbind(seq(p + 1, ncol(yret)), yl)
    }else if(type == 'both'){
      Z_t <- rbind(rep(1, ncol(yl)), seq(p + 1, ncol(yret)), yl)
    }else{
      Z_t <- yl
    }
  }else{
    Z_t <- Z
    yret <- y
  }

 if(is.null(SB2)){
  Regime1 <- which(SB == 0) - 1
  Regime2 <- which(SB == 1) - 1

  best_estimation = IdentifyVolatility(crit = crit, u = u, TB = TB, p = p, k = k, type = type,
                                     Regime1 = Regime1, Regime2 = Regime2,
                                     RestrictionMatrix = restriction_matrix, restrictions = restrictions,
                                     Tob = Tob, SigmaHat1 = Sigma_hat1, SigmaHat2 = Sigma_hat2, Zt = Z_t, y = y,
                                     maxIter = max.iter)

  # Adding normalizing constant
  best_estimation$Lik <- -(Tob * (k / 2) * log(2 * pi) + best_estimation$Lik)

  if(restrictions > 0 ){


    unrestricted_estimation <- IdentifyVolatility(crit = crit, u = u, TB = TB, p = p, k = k, type = type,
                                                Regime1 = Regime1, Regime2 = Regime2,
                                                RestrictionMatrix = matrix(NA, k, k), restrictions = 0,
                                                Tob = Tob, SigmaHat1 = Sigma_hat1, SigmaHat2 = Sigma_hat2, Zt = Z_t, y = y,
                                                maxIter = max.iter)

   # Adding normalizing constant
    unrestricted_estimation$Lik <- -(Tob * (k / 2) * log(2 * pi) + unrestricted_estimation$Lik)

    lRatioTestStatistic = 2 * (unrestricted_estimation$Lik - best_estimation$Lik)
    restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
    pValue = round(1 - pchisq(lRatioTestStatistic, restrictions), 4)
    lRatioTest <- data.frame(testStatistic = lRatioTestStatistic, p.value = pValue)
    rownames(lRatioTest) <- ""
    colnames(lRatioTest) <- c("Test statistic", "p-value")
  }else{
    lRatioTest <- NULL
  }
 } else {
   Regime1 <- which(SB == 1) - 1
   Regime2 <- which(SB2 == 1) - 1
   Regime3 <- which(SB3 == 1) - 1

   best_estimation <- IdentifyVolatility3(crit = crit, u = u, TB1 = TB1, TB2 = TB2, TB3 = TB3, p = p, k = k, type = type,
                                          Regime1 = Regime1, Regime2 = Regime2, Regime3 = Regime3,
                                          RestrictionMatrix = restriction_matrix, restrictions = restrictions,
                                          Tob = Tob, SigmaHat1 = Sigma_hat1, SigmaHat2 = Sigma_hat2, SigmaHat3 = Sigma_hat3,
                                          Zt = Z_t, y = y,
                                          maxIter = max.iter)
   # Adding normalizing constant
   best_estimation$Lik <- -(Tob * (k / 2) * log(2 * pi) + best_estimation$Lik)

   if(restrictions > 0 ){


     unrestricted_estimation <- IdentifyVolatility3(crit = crit, u = u, TB1 = TB1, TB2 = TB2, TB3 = TB3, p = p, k = k, type = type,
                                                    Regime1 = Regime1, Regime2 = Regime2, Regime3 = Regime3,
                                                    RestrictionMatrix = matrix(NA, k, k), restrictions = 0,
                                                    Tob = Tob, SigmaHat1 = Sigma_hat1, SigmaHat2 = Sigma_hat2, SigmaHat3 = Sigma_hat3,
                                                    Zt = Z_t, y = y,
                                                    maxIter = max.iter)
     # Adding normalizing constant
     unrestricted_estimation$Lik <- -(Tob*(k/2)*log(2*pi) + unrestricted_estimation$Lik)

     lRatioTestStatistic = 2 * (unrestricted_estimation$Lik - best_estimation$Lik)
     restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
     pValue = round(1 - pchisq(lRatioTestStatistic, restrictions), 4)
     lRatioTest <- data.frame(testStatistic = lRatioTestStatistic, p.value = pValue)
     rownames(lRatioTest) <- ""
     colnames(lRatioTest) <- c("Test statistic", "p-value")
   }else{
     lRatioTest <- NULL
   }
 }


if(is.null(best_estimation$A_hat)){
  if(inherits(x, "varest")){
    p <- x$p
    y <- t(x$y)
    type = x$type
    coef_x = coef(x)
  }else if(inherits(x, "nlVar")){
    p <- x$lag
    y <- t(x$model[, 1:k])
    coef_x <- t(coef(x))

    if(inherits(x, "VECM")){
      coef_x <- t(tsDyn::VARrep(x))
    }

    if(rownames(coef_x)[1] %in% c("Intercept", "constant")){
      coef_x <- coef_x[c(2:nrow(coef_x),1),]

    }else if(rownames(coef_x)[1] == "Trend"){
      coef_x <- coef_x[c(2:nrow(coef_x),1),]
    }
    if(rownames(coef_x)[1] %in% c("Intercept", "constant", "Trend")){
      coef_x <- coef_x[c(2:nrow(coef_x),1),]
    }
    type <- x$include
    coef_x <- split(coef_x, rep(1:ncol(coef_x), each = nrow(coef_x)))
    coef_x <- lapply(coef_x, as.matrix)
  }else if(inherits(x, "list")){
    p <- x$order
    y <- t(x$data)
    coef_x <- x$coef
    if(x$cnst == TRUE){
      coef_x <- coef_x[c(2:nrow(coef_x),1),]
      type = "const"
    }
    coef_x <- split(coef_x, rep(1:ncol(coef_x), each = nrow(coef_x)))
    coef_x <- lapply(coef_x, as.matrix)

  }else if(inherits(x, "vec2var")){
    coef_x <- vector("list", length = k)
    names(coef_x) <- colnames(x$y)
    p <- x$p
    y <- t(x$y)

    for (i in seq_len(k)) {
      for (j in seq_len(p)) coef_x[[i]] <- c(coef_x[[i]], x$A[[j]][i,])
      coef_x[[i]] <- c(coef_x[[i]], x$deterministic[i,])
    }
    coef_x <- lapply(coef_x, matrix)
    type <- "const"
  }

  A <- matrix(0, nrow = k, ncol = k*p)
  for(i in 1:k){
    A[i,] <- coef_x[[i]][1:(k*p),1]
  }
  A_hat <- A
  if(type == "const"){
    v <- rep(1, k)
    for(i in 1:k){
      v[i] <- coef_x[[i]][(k*p+1), 1]
    }
    A_hat <- cbind(v, A)
  }else if (type == "trend"){
    trend <- rep(1, k)
    for(i in 1:k){
      trend[i] <- coef_x[[i]][(k*p+1), 1]
    }
    A_hat <- cbind(trend, A)
  }else if(type == "both"){
    v <- rep(1, k)
    for(i in 1:k){
      v[i] <- coef_x[[i]][(k*p+1), 1]
    }
    trend <- rep(1, k)
    for(i in 1:k){
      trend[i] <- coef_x[[i]][(k*p+2), 1]
    }
    A_hat <- cbind(v, trend, A)
  }
}

wald <- wald.test(best_estimation$Lambda, best_estimation$Fish, restrictions)
rownames(best_estimation$B) <- colnames(u)
rownames(best_estimation$Lambda) <- colnames(u)
rownames(best_estimation$Lambda_SE) <- colnames(u)
rownames(best_estimation$B_SE) <- colnames(u)

result <- list(
  Lambda = best_estimation$Lambda,    # estimated Lambda matrix (unconditional heteroscedasticity)
  Lambda_SE = best_estimation$Lambda_SE,  # standard errors of Lambda matrix
  B = best_estimation$B,              # estimated B matrix (unique decomposition of the covariance matrix)
  B_SE = best_estimation$B_SE,            # standard errors of B matrix
  n = Tob,                # number of observations
  Fish = best_estimation$Fish,            # observerd fisher information matrix
  Lik = best_estimation$Lik,             # function value of likelihood
  wald_statistic = wald,  # results of wald test
  iteration = best_estimation$iteration,     # number of gls estimations
  method = "Changes in Volatility",
  SB = SB_out,                # Structural Break in number format
  Sb2 = SB2,
  A_hat = best_estimation$A_hat,            # VAR parameter estimated with gls
  type = type,          # type of the VAR model e.g 'const'
  SBcharacter = SBcharacter,             # Structural Break in input character format
  restrictions = restrictions, # number of restrictions
  restriction_matrix = rmOut,
  y = yOut,                # Data
  p = unname(p),                # number of lags
  K = k,# number of time series
  lRatioTest = lRatioTest,
  VAR = x
)

  class(result) <- "svars"
  return(result)
}
