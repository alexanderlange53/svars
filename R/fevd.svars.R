#' Forecast error variance decomposition for SVAR Models
#'
#' Calculation of forecast error variance decomposition for an identified SVAR object 'svars' derived by function id.st( ), id.cvm( ),id.cv( ),id.dc( ) or id.ngml( ).
#'
#' @param x SVAR object of class "svars".
#' @param n.ahead Integer specifying the steps.
#' @param ... Currently not used.
#'
#' @return A list with class attribute "svarfevd" holding the forecast error variance decompositions as data frames.
#'
#' @references Kilian, L., Luetkepohl, H., 2017. Structural Vector Autoregressive Analysis, Cambridge University Press.
#'
#' @seealso \code{\link{id.cvm}}, \code{\link{id.garch}}, \code{\link{id.dc}}, \code{\link{id.ngml}}, \code{\link{id.cv}} or \code{\link{id.st}}
#'
#' @examples
#' \donttest{
#' v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#' x1 <- id.dc(v1)
#' x2 <- fevd(x1, n.ahead = 30)
#' plot(x2)
#' }
#'
#' @rdname fevd
#' @name fevd
#' @aliases fevd.svars
#' @import  vars
#' @export

fevd.svars <- function(x, n.ahead = 10, ...){
  if(!(is(x, "svars"))){
    stop("\nPlease provide an object of class 'svars'.\n")
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

  IR <-  array(unlist(IRF(A_hat, B_hat, n.ahead)), c(x$K, x$K, n.ahead))

  fe <- list()
  for(i in 1:nrow(B_hat)){
    fe[[i]] <- as.data.frame(t(IR[i,,]))
    colnames(fe[[i]]) <- colnames(x$y)
  }
  names(fe) <- colnames(x$y)
  fe2 <- fe

  for(i in 1:length(fe)){
    for(j in 1:n.ahead){
      fe2[[i]][j,] <- (colSums(fe[[i]][j:1,]^2)/sum(fe[[i]][j:1,]^2))*100
    }
  }

  class(fe2) <- "svarfevd"
  return(fe2)
}
