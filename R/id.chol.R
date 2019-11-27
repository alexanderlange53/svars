#' Recursuve identification of SVAR models via Cholesky decomposition
#'
#' Given an estimated VAR model, this function uses the Cholesky decomposition to identify the structural impact matrix B of the corresponding SVAR model
#' \deqn{y_t=c_t+A_1 y_{t-1}+...+A_p y_{t-p}+u_t
#' =c_t+A_1 y_{t-1}+...+A_p y_{t-p}+B \epsilon_t.}
#' Matrix B corresponds to the decomposition of the least squares covariance matrix \eqn{\Sigma_u=B\Lambda_t B'}.
#'
#' @param x An object of class 'vars', 'vec2var', 'nlVar'. Estimated VAR object
#' @return A list of class "svars" with elements
#' \item{B}{Estimated structural impact matrix B, i.e. unique decomposition of the covariance matrix of reduced form residuals}
#' \item{n}{Number of observations}
#' \item{method}{Method applied for identification}
#' \item{A_hat}{Estimated VAR parameter}
#' \item{type}{Type of the VAR model, e.g. 'const'}
#' \item{y}{Data matrix}
#' \item{p}{Number of lags}
#' \item{K}{Dimension of the VAR}
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
#' summary(x1)
#'
#'
#' # impulse response analysis
#' i1 <- irf(x1, n.ahead = 30)
#' plot(i1, scales = 'free_y')
#'
#'
#' }
#' @export


#----------------------------------------------#
## Identification via Cholesky decomposition  ##
#----------------------------------------------#

id.chol <- function(x){

  u <- Tob <- p <- k <- residY <- coef_x <- yOut <- type <- y <-  NULL
  get_var_objects(x)

  sigg <- crossprod(u) / (Tob- 1 - k * p)

  # Choleski decomposition
  P <- t(chol(sigg))

  # obtaining VAR parameter
  # obtaining VAR parameter
  if(inherits(x, "var.boot")){
    A_hat <- coef_x
  }else{
    A_hat <- vars::Bcoef(x)[, c((k * p+1):ncol(vars::Bcoef(x)),1:(k * p))]
  }




  # if(inherits(x, "var.boot")){
  #   A_hat <- coef_x
  # }else{
  #   A <- matrix(0, nrow = k, ncol = k * p)
  #   for(i in 1:k){
  #     A[i,] <- coef_x[[i]][1:(k * p),1]
  #   }
  #
  #   A_hat <- A
  #
  #   if(type == "const"){
  #     v <- rep(1, k)
  #
  #     for(i in 1:k){
  #       v[i] <- coef_x[[i]][(k*p+1), 1]
  #     }
  #
  #     A_hat <- cbind(v, A)
  #   }else if (type == "trend"){
  #     trend <- rep(1, k)
  #
  #     for(i in 1:k){
  #       trend[i] <- coef_x[[i]][(k*p+1), 1]
  #     }
  #
  #     A_hat <- cbind(trend, A)
  #   }else if(type == "both"){
  #     v <- rep(1, k)
  #
  #     for(i in 1:k){
  #       v[i] <- coef_x[[i]][(k*p+1), 1]
  #     }
  #
  #     trend <- rep(1, k)
  #
  #     for(i in 1:k){
  #       trend[i] <- coef_x[[i]][(k*p+2), 1]
  #     }
  #
  #     A_hat <- cbind(v, trend, A)
  #   }
  # }

  result <- list(B = P,       # estimated B matrix (unique decomposition of the covariance matrix)
                 A_hat = A_hat,  # estimated VAR parameter
                 method =        "Cholesky",
                 n = Tob,        # number of observations
                 type = type,    # type of the VAR model e.g 'const'
                 y = yOut,       # Data
                 p = unname(p),  # number of lags
                 K = k          # number of time series
  )
  class(result) <- "svars"
  return(result)
}
