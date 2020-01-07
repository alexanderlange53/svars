#' Recursive identification of SVAR models via Cholesky decomposition
#'
#' Given an estimated VAR model, this function uses the Cholesky decomposition to identify the structural impact matrix B of the corresponding SVAR model
#' \deqn{y_t=c_t+A_1 y_{t-1}+...+A_p y_{t-p}+u_t
#' =c_t+A_1 y_{t-1}+...+A_p y_{t-p}+B \epsilon_t.}
#' Matrix B corresponds to the decomposition of the least squares covariance matrix \eqn{\Sigma_u=B\Lambda_t B'}.
#'
#' @param x An object of class 'vars', 'vec2var', 'nlVar'. Estimated VAR object
#' @param order_k Vector. Vector of characters or integers specifying the assumed structure of the recursive causality. Change the causal ordering in the instantaneous effects without permuting variables and re-estimating the VAR model.
#' @return A list of class "svars" with elements
#' \item{B}{Estimated structural impact matrix B, i.e. unique decomposition of the covariance matrix of reduced form residuals}
#' \item{n}{Number of observations}
#' \item{method}{Method applied for identification}
#' \item{order_k}{Ordering of the variables as assumed for recursive causalitiy}
#' \item{A_hat}{Estimated VAR parameter}
#' \item{type}{Type of the VAR model, e.g. 'const'}
#' \item{y}{Data matrix}
#' \item{p}{Number of lags}
#' \item{K}{Dimension of the VAR}
#' \item{VAR}{Estimated input VAR object}
#'
#'
#' @references Luetkepohl, H., 2005. New introduction to multiple time series analysis, Springer-Verlag, Berlin.
#'
#' @seealso For alternative identification approaches see \code{\link{id.st}}, \code{\link{id.cvm}}, \code{\link{id.cv}}, \code{\link{id.dc}} or \code{\link{id.ngml}}
#'
#' @examples
#' \donttest{
#'
#' # data contains quarterly observations from 1965Q1 to 2008Q3
#' # x = output gap
#' # pi = inflation
#' # i = interest rates
#' set.seed(23211)
#' v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#' x1 <- id.chol(v1)
#' x2 <- id.chol(v1, order_k = c("pi", "x", "i")) ## order_k = c(2,1,3)
#' summary(x1)
#'
#'
#' # impulse response analysis
#' i1 <- irf(x1, n.ahead = 30)
#' i2 <- irf(x2, n.ahead = 30)
#' plot(i1, scales = 'free_y')
#' plot(i2, scales = 'free_y')
#'
#'
#' }
#' @export


#----------------------------------------------#
## Identification via Cholesky decomposition  ##
#----------------------------------------------#

id.chol <- function(x, order_k = NULL){
  # define
  u <- Tob <- p <- k <- residY <- coef_x <- yOut <- type <- y <-  NULL
  get_var_objects(x)
  names_k <- colnames(yOut)
  sigg <- crossprod(u) / (Tob- 1 - k * p)

  if(is.null(order_k)){
    order_k <- 1:k
  }else if(is.character(order_k)){
    order_k <- sapply(order_k, FUN=function(x) which(names_k == x))
    if(is.list(order_k)){
      stop("Check variable names given as characters in 'order_k' or use integers instead!")
    }
  }
  names(order_k) <- names_k

  # Choleski decomposition
  perm <- diag(k)[ , order_k, drop=FALSE] # permutation matrix
  psig <- t(perm) %*% sigg %*% perm  # permute in accordance with recursive causality
  P <- t(chol(psig))
  B <- perm %*% P %*% t(perm) # repermute via inverse using solve(perm) == t(perm)
  rownames(B) <- names_k

  # obtaining VAR parameter
  if(inherits(x, "var.boot")){
    A_hat <- coef_x
  }else{
    if(type == "none"){
      A_hat <- vars::Bcoef(x)
    }else{
      A_hat <- vars::Bcoef(x)[, c((k * p+1):ncol(vars::Bcoef(x)),1:(k * p))]
    }
  }

  # return result
  result <- list(B = B,          # estimated B matrix (unique decomposition of the covariance matrix)
                 A_hat = A_hat,  # estimated VAR parameter
                 method =        "Cholesky",
                 order_k = order_k, # ordering of the variables as assumed for recursive causalitiy
                 n = Tob,        # number of observations
                 type = type,    # type of the VAR model e.g 'const'
                 y = yOut,       # Data
                 p = unname(p),  # number of lags
                 K = k, # number of time series
                 VAR = x
  )
  class(result) <- "svars"
  return(result)
}
