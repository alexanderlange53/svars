#' Counterfactuals for SVAR Models
#'
#' Calculation of Counterfactuals for an identified SVAR object 'svars' derived by function id.st( ), id.cvm( ),id.cv( ),id.dc( ) or id.ngml( ).
#'
#' @param x SVAR object of class "svars"
#' @param series Integer. indicating the series for which the counterfactuals should be calculated.
#' @param transition Numeric. Value from [0, 1] indicating how many initial values should be discarded, i.e., 0.1 means that the first 10 per cent observations of the sample are considered as transient.
#'
#' @return A list with class attribute "hd" holding the Counterfactuals as data frame.
#'
#' @references Kilian, L., Luetkepohl, H., 2017. Structural Vector Autoregressive Analysis, Cambridge University Press.
#'
#' @seealso \code{\link{id.cvm}}, \code{\link{id.dc}}, \code{\link{id.ngml}}, \code{\link{id.cv}}, \code{\link{id.garch}} or \code{\link{id.st}}
#'
#' @examples
#' \donttest{
#' v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#' x1 <- id.dc(v1)
#' x2 <- cf(x1, series = 2)
#' plot(x2)
#' }
#'
#' @export

cf <- function(x, series = 1, transition = 0){

  # Function to calculate matrix potence
  "%^%" <- function(A, n){
    if(n == 1){
      A
    }else{
      A %*% (A %^% (n-1))
    }
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
  Z <- t(YLagCr(y, p))

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

  # Calculating counterfactuals
  yhat_counter <- as.data.frame(matrix(NA, nrow = obs, ncol = k))

  for (i in 1:k){
    yhat_counter[, i] <- (yhat[, 2] - yhat[, (3 + i)])
  }

  colnames(yhat)[3] <- paste("Constructed series ", colnames(y)[series])
  colnames(yhat)[2] <- paste("Demeaned series ", colnames(y)[series])

  for(i in 4:ncol(yhat)){
    colnames(yhat)[i] <- paste("Cumulative effect of flow ", colnames(y)[i-3], "shock on ", colnames(y)[series])
  }

  for(i in 1:ncol(yhat_counter)){
    colnames(yhat_counter)[i] <- paste(colnames(y)[series], "with and without cumulative effect of flow", colnames(y)[i], "shock")
  }

  yhat_counter <- yhat_counter[,-series]
  yhat <- yhat[,c(1, rep(2, ncol(yhat_counter)))]


  if(inherits(x$y, "ts")){
    count <- ts(yhat[, -grep("V1", colnames(yhat))], start = start(lag(x$y, k = -x$p)), end = end(x$y), frequency = frequency(x$y))
    counterfac <- list(actual = na.omit(count), counter = yhat_counter)
  }else{
    counterfac <- list(counter = as.data.frame(na.omit(yhat)), counter = yhat_counter)
  }
  class(counterfac) <- "cf"
  return(counterfac)
}
