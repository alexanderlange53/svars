#' Historical decomposition for SVAR Models
#'
#' Calculation of historical decomposition for an identified SVAR object 'svars' derived by function id.st( ), id.cvm( ),id.cv( ),id.dc( ) or id.ngml( ).
#'
#' @param x SVAR object of class "svars"
#' @param series Integer, indicating the series that should be decomposed.
#'
#' @seealso \code{\link{id.cvm}}, \code{\link{id.dc}}, \code{\link{id.ngml}}, \code{\link{id.cv} or \code{\link{id.st}}
#'
#' @references Kilian, L., Luetkepohl, H., 2017. Structural Vector Autoregressive Analysis, Cambridge University Press.
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

hd <- function(x, series = 1){

  # Function to calculate matrix potence
  "%^%" <- function(A, n){
    if(n == 1){
      A
    }else{
      A %*% (A %^% (n-1))
    }
  }

  # function to create Z matrix
  y_lag_cr <- function(y, lag_length){
    # create matrix that stores the lags
    y_lag <- matrix(NA, dim(y)[1],dim(y)[2]*lag_length)
    for (i in 1:lag_length) {
      y_lag[(1+i):dim(y)[1],((i*NCOL(y)-NCOL(y))+1):(i*NCOL(y))] <- y[1:(dim(y)[1]-i),(1:NCOL(y))]
    }
    # drop first observation
    y_lag <- as.matrix(y_lag[-(1:lag_length),])
    out <- list(lags = y_lag)
  }

  # function to calculate impulse response
  IrF <- function(A_hat, B_hat, horizon){
    k <- nrow(A_hat)
    p <- ncol(A_hat)/k
    if(p == 1){
      irfa <- array(0, c(k, k, horizon))
      irfa[,,1] <- B_hat
      for(i in 1:horizon){
        irfa[,,i] <- (A_hat%^%i)%*%B_hat
      }
      return(irfa)
    }else{
      irfa <- array(0, c(k, k, horizon))
      irfa[,,1] <- B_hat
      Mm <- matrix(0, nrow = k*p, ncol = k*p)
      Mm[1:k, 1:(k*p)] <- A_hat
      Mm[(k+1):(k*p), 1 : ((p-1)*k)] <- diag(k*(p-1))
      Mm1 <- diag(k*p)
      for(i in 1:(horizon-1)){
        Mm1 <- Mm1%*%Mm
        irfa[,,(i+1)] <- Mm1[1:k, 1:k]%*%B_hat
      }
      return(irfa)
    }
  }

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

  IR <- IrF(A_hat, B_hat, horizon)

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

  # gathering informations from vars object
  y <- x$y
  p <- x$p
  obs <- x$n
  k <- x$K
  B <- x$B

  # calculating covariance from actual VAR
  A <- x$A_hat
  Z <- t(y_lag_cr(y, p)$lags)

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
  for(i in 1:obs){
    for(j in 1:k){
      y_hat[i,j] <- impulse[1:i, j+series-1] %*% t(s.errors)[i:1,j]
    }
  }

  y_hat_a <- rowSums(y_hat)

  yhat <- as.data.frame(cbind(seq(1, length(y_hat_a)), y_hat_a, y_hat))

  colnames(yhat)[2] <- colnames(y)[series]

  for(i in 3:ncol(yhat)){
    colnames(yhat)[i] <- paste("Cumulative effect of flow ", colnames(y)[i-2], "shock on ", colnames(y)[series])
  }
  if(inherits(x$y, "ts")){
  histdecomp <- list(hidec = ts(yhat[, -grep("V1", colnames(yhat))], start = start(lag(x$y, k = -x$p)), end = end(x$y), frequency = frequency(x$y)))
}else{
  histdecomp <- list(hidec = as.data.frame(yhat))
}
  class(histdecomp) <- "hd"
  return(histdecomp)
}
