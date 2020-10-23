#' Historical decomposition for SVAR Models
#'
#' Calculation of historical decomposition for an identified SVAR object 'svars' derived by function id.st( ), id.cvm( ),id.cv( ),id.dc( ) or id.ngml( ).
#'
#' @param x SVAR object of class "svars"
#' @param series Integer. indicating the series that should be decomposed.
#' @param transition Numeric. Value from [0, 1] indicating how many initial values should be discarded, i.e., 0.1 means that the first 10 per cent observations of the sample are considered as transient.
#'
#' @return A list with class attribute "hd" holding the historical decomposition as data frame.
#'
#' @references Kilian, L., Luetkepohl, H., 2017. Structural Vector Autoregressive Analysis, Cambridge University Press.
#'
#' @seealso \code{\link{id.cvm}}, \code{\link{id.dc}}, \code{\link{id.ngml}}, \code{\link{id.cv}}, \code{\link{id.garch}} or \code{\link{id.st}}
#'
#' @examples
#' \donttest{
#' v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#' x1 <- id.dc(v1)
#' x2 <- hd(x1, series = 2)
#' plot(x2)
#' }
#'
#' @export

hd <- function(x, series = 1, transition = 0){


  ## Step 1: Calculate MA coefficients

  if(x$type == "const"){
    A_hat <- x$A_hat[,-1]
  }else if(x$type == "trend"){
    A_hat <- x$A_hat[,-1]
  }else if(x$type == "both"){
    A_hat <- x$A_hat[,-c(1,2)]
  }else{
    A_hat <- x$A_hat
  }

  B_hat <- x$B

  horizon <- x$n

  IR <- array(unlist(IRF(A_hat, B_hat, horizon)), c(x$K, x$K, horizon))

  impulse <- matrix(0, ncol = dim(IR)[2]^2 + 1, nrow = dim(IR)[3])
  colnames(impulse) <- rep("V1", ncol(impulse))
  cc <- 1
  impulse[,1] <- seq(1, dim(IR)[3])
  for(i in 1:dim(IR)[2]){
    for(j in 1:dim(IR)[2]){
      cc <- cc + 1
      impulse[,cc] <- IR[i,j,]
      colnames(impulse)[cc] <- paste("epsilon[",colnames(x$y)[j],"]", "%->%", colnames(x$y)[i])
    }
  }

  # Step 2: Calculate structural errors

  # gathering informations from 'svars' object

  # in case original data came in different format than matrix or ts
  if(!inherits(x$y, c("matrix", "ts"))){
    y = as.matrix(x$y)
  }else{
  y <- x$y
  }
  p <- x$p
  obs <- x$n
  k <- x$K
  B <- x$B

  # calculating covariance from actual VAR
  A <- x$A_hat
  Z <- t(YLagCr(as.matrix(y), p))

  if(x$type == "const"){
    Z <- rbind(rep(1, ncol(Z)), Z)
  }else if(x$type == "trend"){
    Z <- rbind(seq(1, ncol(Z)), Z)
  }else if(x$type == "both"){
    Z <- rbind(rep(1, ncol(Z)), seq(1, ncol(Z)), Z)
  }else{
    Z <- Z
  }

  u <- t(y[-c(1:p),]) - A %*% Z

  s.errors <- solve(B_hat)%*%u

  # Step 3: Match up structural shocks with appropriate impuslse response
  impulse <- impulse[,-1]
  y_hat <- matrix(NA, nrow = obs, ncol = k)

  if (transition == 0) {
    for (i in 1:obs) {
      for (j in 1:k) {
        y_hat[i, j] <- impulse[1:i, j + series * k - k] %*% t(s.errors)[i:1, j]
      }
    }
  } else {
    for (i in (obs - floor(obs * (1 - transition))):obs) {
      for (j in 1:k) {
        y_hat[i, j] <- impulse[(obs - floor(obs * (1 - transition))):i, j + series * k - k] %*% t(s.errors)[i:(obs - floor(obs * (1 - transition))), j]
      }
    }
  }



  y_hat_a <- rowSums(y_hat)

  yhat <- as.data.frame(cbind(seq(1, length(y_hat_a)), (y[-c(1:p), series] - mean(y[-c(1:p), series])), y_hat_a, y_hat))

  colnames(yhat)[3] <- paste("Constructed series ", colnames(y)[series])
  colnames(yhat)[2] <- paste("Demeaned series ", colnames(y)[series])

  for(i in 4:ncol(yhat)){
    colnames(yhat)[i] <- paste("Cumulative effect of ", colnames(y)[i-3], "shock on ", colnames(y)[series])
  }

  if(inherits(x$y, "ts")){
    hidec <- ts(yhat[, -grep("V1", colnames(yhat))], start = start(lag(x$y, k = -x$p)), end = end(x$y), frequency = frequency(x$y))
    histdecomp <- list(hidec = na.omit(hidec))
}else{
   histdecomp <- list(hidec = as.data.frame(na.omit(yhat)))
}
  class(histdecomp) <- "hd"
  return(histdecomp)
}
