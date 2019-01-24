#' Instrument variable identification of SVAR
#'
#'Given an estimated VAR model, this function applies independence-based identification for the structural impact matrix B of the corresponding SVAR model
#' \deqn{y_t=c_t+A_1 y_{t-1}+...+A_p y_{t-p}+u_t  =c_t+A_1 y_{t-1}+...+A_p y_{t-p}+B \epsilon_t.}
#' Matrix B corresponds to the unique decomposition of the least squares covariance matrix \eqn{\Sigma_u=B B'} if the vector of structural shocks \eqn{\epsilon_t} contains at most one Gaussian shock (Comon, 1994).
#' A nonparametric dependence measure, the distance covariance (Szekely et al, 2007), determines least dependent structural shocks. The algorithm described in Matteson and Tsay (2013) is applied to calculate the matrix B.
#'
#' @param x An object of class 'vars', 'vec2var', 'nlVar'. Estimated VAR object
#' @param PIT Logical. If PIT='TRUE', the distribution and density of the independent components are estimated using gaussian kernel density estimates
#' @return A list of class "svars" with elements
#' \item{B}{Estimated structural impact matrix B, i.e. unique decomposition of the covariance matrix of reduced form errors}
#' \item{A_hat}{Estimated VAR parameter}
#' \item{method}{Method applied for identification}
#' \item{n}{Number of observations}
#' \item{type}{Type of the VAR model, e.g. 'const'}
#' \item{y}{Data matrix}
#' \item{p}{Number of lags}
#' \item{K}{Dimension of the VAR}
#' \item{F_statistic}{Test statistic for weak instrument}
#'
#' @seealso For alternative identification approaches see \code{\link{id.st}}, \code{\link{id.cvm}}, \code{\link{id.cv}} or \code{\link{id.ngml}}
#'
#'@references Lunsford, K. G., 2016. "Identifying Structural VARs with a Proxy Variable and a Test for a Weak Proxy"
#' @examples
#'
#'
#' @export

id.iv <- function(x, instruments = NULL){

  u <- Tob <- p <- k <- residY <- coef_x <- yOut <- type <- y <-  NULL
  get_var_objects(x)

  if(is.null(instruments)){
    stop('Please provide a valid instrument')
  }

  # Checking length of instrument
  if(Tob != length(instruments)){
    stop('length of instrument variable is unequal to data length')
  }

  #==========================#
  ## Calculating instrument ##
  #==========================#

  # regressing instruments on residuals
  Pi <- solve(crossprod(u)/Tob) %*% (crossprod(u, instruments)/Tob)

  # covariance of VAR innovations and instrument
  phi2 <- (crossprod(instruments, u)/Tob) %*% solve(crossprod(u)/Tob) %*% (crossprod(u, instruments)/Tob)

  # Impact relation
  # B_1 <- (crossprod(u, instruments)/Tob) %*% expm::sqrtm((crossprod(instruments, u)/Tob) %*%
  #                                                         solve(crossprod(u)/Tob) %*% (crossprod(u, instruments)/Tob))
  B_1 <- (crossprod(u, instruments)/Tob)  %*% (1/sqrt(phi2))

  # calculating structural errors
  s_errors <- u %*% Pi %*% sqrt(phi2) ^ (-1)

  #===============================#
  ## Testing for weak instrument ##
  #===============================#

  F_test <- weak_instrument_test(Tob = Tob, u = u, k = k, instruments = instruments)

  # obtaining VAR parameter
  if(inherits(x, "var.boot")){
    A_hat <- coef_x
  }else{
    A <- matrix(0, nrow = k, ncol = k*p)
    for(i in 1:k){
      A[i,] <- coef_x[[i]][1:(k*p),1]
    }

    A_hat <- A

    if(type == "const"){
      v <- rep(1, k)

      for(i in 1:k){
        v[i] <- coef_x[[i]][(k*p+1), 1]
      }

      A_hat <- cbind(v, A)
    }else if (type == "trend"){
      trend <- rep(1, k)

      for(i in 1:k){
        trend[i] <- coef_x[[i]][(k*p+1), 1]
      }

      A_hat <- cbind(trend, A)
    }else if(type == "both"){
      v <- rep(1, k)

      for(i in 1:k){
        v[i] <- coef_x[[i]][(k*p+1), 1]
      }

      trend <- rep(1, k)

      for(i in 1:k){
        trend[i] <- coef_x[[i]][(k*p+2), 1]
      }

      A_hat <- cbind(v, trend, A)
    }
  }

  result <- list(B = B_1,       # estimated B matrix (unique decomposition of the covariance matrix)
                 A_hat = A_hat,  # estimated VAR parameter
                 method =        "Instrument Variables",
                 n = Tob,        # number of observations
                 type = type,    # type of the VAR model e.g 'const'
                 y = yOut,       # Data
                 p = unname(p),  # number of lags
                 K = k,          # number of time series
                 F_statistic = F_test # Test on weak instrument
  )
  class(result) <- "svars"
  return(result)

}
