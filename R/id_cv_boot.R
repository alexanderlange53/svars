
id.cv_boot <- function(x, SB, max.iter = 50, crit = 0.001, restriction_matrix = NULL, Z = NULL){

  if(inherits(x, "var.boot")){
    u_t <- x$residuals
    Tob <- nrow(u_t)
    k <- ncol(u_t)
    residY <- u_t
  }else{
    u_t <- residuals(x)
    Tob <- nrow(u_t)
    k <- ncol(u_t)
    residY <- u_t
  }

  if(inherits(x, "var.boot")){
    p <- x$p
    y <- t(x$y)
    type = x$type
    coef_x = x$coef_x
  }else if(inherits(x, "varest")){
    p <- x$p
    y <- t(x$y)
  }else if(inherits(x, "nlVar")){
    if(inherits(x, "VECM")){
      stop("id.cv is not available for VECMs")
    }
    p <- x$lag
    y <- t(x$model[, 1:k])
  }else if(inherits(x, "list")){
    p <- x$order
    y <- t(x$data)
  }else{
    stop("Object class is not supported")
  }

  if(is.numeric(SB)){
    SBcharacter <- NULL
  }

  TB <- SB - p

  resid1 <- u_t[1:TB-1,]
  resid2 <- u_t[TB:Tob,]
  Sigma_hat1 <- (crossprod(resid1)) / (TB-1)
  Sigma_hat2 <- (crossprod(resid2)) / (Tob-TB+1)

  if(!is.null(restriction_matrix)){
    result <- identifyVolatility_boot(x, SB, Tob = Tob, u_t = u_t, k = k, y = y, restriction_matrix = restriction_matrix,
                                 Sigma_hat1 = Sigma_hat1, Sigma_hat2 = Sigma_hat2, p = p, TB = TB, SBcharacter,
                                 max.iter = max.iter, crit = crit, Z)
  }else{
    restriction_matrix <- NULL
    result <- identifyVolatility_boot(x, SB, Tob = Tob, u_t = u_t, k = k, y = y, restriction_matrix = restriction_matrix,
                                 Sigma_hat1 = Sigma_hat1, Sigma_hat2 = Sigma_hat2, p = p, TB = TB, SBcharacter,
                                 max.iter = max.iter, crit = crit, Z)
  }

  class(result) <- "svars"
  return(result)
}
