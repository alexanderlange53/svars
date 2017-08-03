#' Independence-based identification of SVAR models based on distance covariances
#'
#'Given an estimated VAR model, this function applies independence-based identification for the structural impact matrix B of the corresponding SVAR model
#' \deqn{y_t=c_t+A_1 y_{t-1}+...+A_p y_{t-p}+u_t  =c_t+A_1 y_{t-1}+...+A_p y_{t-p}+B \epsilon_t.}
#' Matrix B corresponds to the unique decomposition of the least squares covariance matrix \eqn{\Sigma_u=B B'} if the vector of structural shocks \eqn{\epsilon_t} contains at most one Gaussian shock (Comon, 1994).
#' A nonparametric dependence measure, the distance covariance (Szekely et al, 2007), determines least dependent structural shocks. The algorithm described in Matteson and Tsay (2013) is applied to calculate the matrix B.
#'
#' @param x An object of class 'vars', 'vec2var', 'nlVar'. Estimated VAR object
#' @param PIT Logical. If PIT='TRUE', the distribution and density of the independent components are estimated using gaussian kernel density estimates
#'  @return A list of class "svars" with elements
#' \item{B}{Estimated structural impact matrix B, i.e. unique decomposition of the covariance matrix of reduced form errors}
#' \item{A_hat}{Estimated VAR parameter}
#' \item{method}{Method applied for identifaction}
#' \item{n}{Number of observations}
#' \item{type}{Type of the VAR model, e.g. 'const'}
#'
#' @seealso For alternative identification approaches see \code{\link{id.cvm}}, \code{\link{id.cv}} or \code{\link{id.ngml}}
#'
#'@references Matteson, D. S. & Tsay, R. S., 2013. Independent Component Analysis via Distance Covariance, pre-print\cr
#'      Szekely, G. J.; Rizzo, M. L. & Bakirov, N. K., 2007. Measuring and testing dependence by correlation of distances Ann. Statist., 35, 2769-2794\cr
#'      Comon, P., 1994. Independent component analysis, A new concept?, Signal Processing, 36, 287-314
#' @examples
#' \dontrun{
#' # data contains quarterly observations from 1965Q1 to 2008Q3
#' # x = output gap
#' # pi = inflation
#' # i = interest rates
#' set.seed(23211)
#' v1 <- VAR(USA, lag.max = 10, ic = "AIC" )
#' x1 <- id.dc(v1)
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

id.dc <- function(x, PIT=FALSE){

  # getting informations from VAR estimation
  # u <- residuals(x)
  # p <- x$p
  # Tob <- x$obs
  # k <- x$K
  #
  # if (class(x) == "vec2var") {
  #   # TODO: trend cases
  #
  #   coef_x <- vector("list", length = k)
  #   names(coef_x) <- colnames(x$y)
  #
  #   for (i in seq_len(k)) {
  #     for (j in seq_len(p)) coef_x[[i]] <- c(coef_x[[i]], x$A[[j]][i,])
  #     coef_x[[i]] <- c(coef_x[[i]], x$deterministic[i,])
  #   }
  #
  #   coef_x <- lapply(coef_x, matrix)
  #
  #   type <- "const"
  # } else {
  #   coef_x <- coef(x)
  #   type <- x$type
  # }
  if(is.null(residuals(x))){
    stop("No residuals retrieved from model")
  }
  u <- residuals(x)
  Tob <- nrow(u)
  k <- ncol(u)
  residY <- u

  if(length(class(x)) == 1 & "varest" %in% class(x)){
    p <- x$p
    y <- t(x$y)
    type = x$type
    coef_x = coef(x)
  }else if(class(x)[length(class(x))] == "nlVar"){
    p <- x$lag
    y <- t(x$model[, 1:k])
    coef_x <- t(coef(x))

    if(rownames(coef_x)[1] == "Intercept"){
      coef_x <- coef_x[c(2:nrow(coef_x),1),]
      type = "const"
    }else if(rownames(coef_x)[1] == "Trend"){
      coef_x <- coef_x[c(2:nrow(coef_x),1),]
      type <- "trend"
    }
    if(rownames(coef_x)[1] %in% c("Intercept", "Trend")){
      coef_x <- coef_x[c(2:nrow(coef_x),1),]
      type <- "both"
    }
    coef_x <- split(coef_x, rep(1:ncol(coef_x), each = nrow(coef_x)))
    coef_x <- lapply(coef_x, as.matrix)
  }else if(class(x) == "list"){
    p <- x$order
    y <- t(x$data)
    coef_x <- x$coef
    if(x$cnst == TRUE){
      coef_x <- coef_x[c(2:nrow(coef_x),1),]
      type = "const"
    }
    coef_x <- split(coef_x, rep(1:ncol(coef_x), each = nrow(coef_x)))
    coef_x <- lapply(coef_x, as.matrix)

  }else if(class(x) == "vec2var"){
    coef_x <- vector("list", length = k)
    names(coef_x) <- colnames(x$y)
    p <- x$p
    y <- t(x$y)

    for (i in seq_len(k)) {
      for (j in seq_len(p)) coef_x[[i]] <- c(coef_x[[i]], x$A[[j]][i,])
      coef_x[[i]] <- c(coef_x[[i]], x$deterministic[i,])
    }
    coef_x <- lapply(coef_x, matrix)
    type <- "const"

  }else{
    stop("Object class is not supported")
  }
  ########### starting the computations ------------------------------------------------------------------------

  sigg <- crossprod(u)/(Tob-1-k*p)

  # Choleski decomposition
  P_chol <- t(chol(sigg))

  # minimize dCov with 'steadyICA'
  u_chol <- t(solve(P_chol)%*%t(u))
  ICA <- steadyICA(u_chol, symmetric=TRUE, PIT=PIT)

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
              method =        "Distance covariances",
              n = Tob,        # number of observations
              type = type,    # type of the VAR model e.g 'const'
              y = t(y),        # Data
              p = p,        # number of lags
              K = k,         # number of time series
              PIT=PIT
              )
  class(result) <- "svars"
  return(result)
}
