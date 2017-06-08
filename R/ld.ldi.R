#' Least dependent innovations identification
#'
#' Identify the instantaneous response matrix B in SVAR models based on least dependent innovations.
#'
#' @param x VAR-object. (S)VAR model to be identified
#' @return A list of class "svars" with elements
#' \item{B}{Estimated instanstaneous response matrix B, i.e. unique decomposition of the covariance matrix of reduced form errors}
#' \item{A_hat}{Estimated VAR parameter}
#' \item{method}{Method applied for identifaction}
#' \item{n}{Number of observations}
#' \item{type}{Type of the VAR model, e.g. 'const'}
#'
#'
#' @examples
#' \dontrun{
#' # data contains quarterly observations from 1965Q1 to 2008Q3
#' # x = output gap
#' # pi = inflation
#' # i = interest rates
#' set.seed(23211)
#' v1 <- VAR(USA, lag.max = 10, ic = "AIC" )
#' x1 <- id.ldi(v1)
#' summary(x1)
#'
#' # switching columns according to sign pattern
#' x1$B <- x1$B[,c(3,2,1)]
#' x1$B[,3] <- x1$B[,3]*(-1)
#'
#' # impulse response analysis
#' i1 <- imrf(x1, horizon = 30)
#' plot(i1, scales = 'free_y')
#' }
#'
#' @export


#--------------------------------------------------#
## Identification via least dependent innovations ##
#--------------------------------------------------#

id.ldi <- function(x){

  # getting informations from VAR estimation
  u <- residuals(x)
  p <- x$p
  Tob <- x$obs
  k <- x$K

  if (class(x) == "vec2var") {
    # TODO: trend cases

    coef_x <- vector("list", length = k)
    names(coef_x) <- colnames(x$y)

    for (i in seq_len(k)) {
      for (j in seq_len(p)) coef_x[[i]] <- c(coef_x[[i]], x$A[[j]][i,])
      coef_x[[i]] <- c(coef_x[[i]], x$deterministic[i,])
    }

    coef_x <- lapply(coef_x, matrix)

    type <- "const"
  } else {
    coef_x <- coef(x)
    type <- x$type
  }

  ########### starting the computations ------------------------------------------------------------------------

  sigg <- crossprod(u)/(Tob-1-k*p)

  # Choleski decomposition
  P_chol <- t(chol(sigg))

  # minimize dCov with 'steadyICA'
  u_chol <- t(solve(P_chol)%*%t(u))
  ICA <- steadyICA(u_chol, symmetric=TRUE)

  # structural matrix Sigma_u = BB'
  P <- P_chol%*%ICA$W

  # obtaining VAR parameter
  A <- matrix(0, nrow = k, ncol = k*p)
  for(i in 1:k){
    A[i,] <- coef_x[[i]][1:(k*p),1]
  }
  A_hat <- A
  if(type == 'const'){
    v <- rep(1, k)
    for(i in 1:k){
      v[i] <- coef_x[[i]][(k*p+1), 1]
    }
    A_hat <- cbind(v, A)
  }

  result <- list(B = P,       # estimated B matrix (unique decomposition of the covariance matrix)
              A_hat = A_hat,  # estimated VAR parameter
              method =        "Least dependent innovations",
              obs = Tob,      # number of observations
              type = type,    # type of the VAR model e.g 'const'
              y = x$y,        # Data
              p = x$p,        # number of lags
              K = x$K         # number of time series
              )
  class(result) <- "svars"
  return(result)
}
