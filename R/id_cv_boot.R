id.cv_boot <- function(x, SB, max.iter = 50, crit = 0.001, restriction_matrix = NULL, Z = NULL){
  u <- Tob <- p <- k <- residY <- coef_x <- yOut <- type <- y <-  NULL
 get_var_objects(x)

  if(is.numeric(SB)){
    SBcharacter <- NULL
  }

  TB <- SB - p

  resid1 <- u[1:TB-1,]
  resid2 <- u[TB:Tob,]
  Sigma_hat1 <- (crossprod(resid1)) / (TB-1)
  Sigma_hat2 <- (crossprod(resid2)) / (Tob-TB+1)

  if(!is.null(restriction_matrix)){
    result <- identifyVolatility_boot(x, SB, Tob = Tob, u = u, k = k, y = y, restriction_matrix = restriction_matrix,
                                      Sigma_hat1 = Sigma_hat1, Sigma_hat2 = Sigma_hat2, p = p, TB = TB, SBcharacter,
                                      max.iter = max.iter, crit = crit, Z)
  }else{
    restriction_matrix <- NULL
    result <- identifyVolatility_boot(x, SB, Tob = Tob, u = u, k = k, y = y, restriction_matrix = restriction_matrix,
                                      Sigma_hat1 = Sigma_hat1, Sigma_hat2 = Sigma_hat2, p = p, TB = TB, SBcharacter,
                                      max.iter = max.iter, crit = crit, Z)
  }

  class(result) <- "svars"
  return(result)
}
