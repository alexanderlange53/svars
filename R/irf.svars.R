#' Impulse Response Functions for SVAR Models
#'
#' Calculation of impulse response functions for an identified SVAR object 'svars' derived by function id.cvm( ),id.cv( ),id.dc( ), id.ngml( ) or id.st( ).
#'
#' @param x SVAR object of class "svars".
#' @param n.ahead Integer specifying the steps.
#' @param ... Currently not used.
#'
#' @return A list with class attribute "svarirf" holding the impulse response functions as data frame.
#'
#' @references Luetkepohl, H., 2005. New introduction to multiple time series analysis, Springer-Verlag, Berlin.
#'
#' @seealso \code{\link{id.cvm}}, \code{\link{id.dc}}, \code{\link{id.ngml}}, \code{\link{id.cv}} or \code{\link{id.st}}
#'
#' @examples
#' \donttest{
#' v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#' x1 <- id.ngml(v1)
#' x2 <- irf(x1, n.ahead = 20)
#' plot(x2)
#' }
#'
#' @rdname irf
#' @name irf
#' @aliases irf.svars
#' @import vars
#' @export

irf.svars <- function(x, ..., n.ahead = 20){
  if(!(class(x)=="svars")){
    stop("\nPlease provide an object of class 'svars'.\n")
  }
  # Function to calculate matrix potence
  "%^%" <- function(A, n){
    if(n == 1){
      A
    }else{
      A %*% (A %^% (n-1))
    }
  }

  # function to calculate impulse response
  IrF <- function(A_hat, B_hat, n.ahead){
    k <- nrow(A_hat)
    p <- ncol(A_hat)/k
    if(p == 1){
      irfa <- array(0, c(k, k, n.ahead))
      irfa[,,1] <- B_hat
      for(i in 2:n.ahead){
        irfa[,,i] <- (A_hat%^%(i-1))%*%B_hat
      }
      return(irfa)
    }else{
      irfa <- array(0, c(k, k, n.ahead))
      irfa[,,1] <- B_hat
      Mm <- matrix(0, nrow = k*p, ncol = k*p)
      Mm[1:k, 1:(k*p)] <- A_hat
      Mm[(k+1):(k*p), 1 : ((p-1)*k)] <- diag(k*(p-1))
      Mm1 <- diag(k*p)
      for(i in 1:(n.ahead-1)){
        Mm1 <- Mm1%*%Mm
        irfa[,,(i+1)] <- Mm1[1:k, 1:k]%*%B_hat
      }
      return(irfa)
    }
  }

  if(x$type == 'const'){
    A_hat <- x$A_hat[,-1]
  }else if(x$type == 'trend'){
    A_hat <- x$A_hat[,-1]
  }else if(x$type == 'both'){
    A_hat <- x$A_hat[,-c(1,2)]
  }else{
    A_hat <- x$A_hat
  }

  B_hat <- x$B

  IR <- IrF(A_hat, B_hat, n.ahead)

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
  impulse <- list(irf = as.data.frame(impulse))
  class(impulse) <- "svarirf"
  return(impulse)
}
