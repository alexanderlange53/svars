#' Least dependent innovations identification based on Cramer von Mises distance
#'
#' Identify the instantaneous response matrix B in SVAR models based on least dependent innovations.
#'
#' @param x VAR-object. (S)VAR model to be identified
#' @param iter number of randomized starting points for optimization
#' @param dd object of class 'indepTestDist'. A simulated independent sample of the same size as the data. If not supplied the function calculates it
#' @param cores number of cores for estimation
#' @return A list of class "svars" with elements
#' \item{B}{Estimated instanstaneous response matrix B, i.e. unique decomposition of the covariance matrix of reduced form errors}
#' \item{A_hat}{Estimated VAR parameter}
#' \item{method}{Method applied for identifaction}
#' \item{n}{Number of observations}
#' \item{type}{Type of the VAR model, e.g. 'const'}
#' \item{test.stats}{Computed test statistics of independence test}
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
#' cob <- indepTestSim(v1$obs, v1$K, verbose=F)
#' x1 <- id.cvm(v1, dd = cob)
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

id.cvm <- function(x, iter = 500, dd = NULL, cores = 1){

  # getting informations from VAR estimation
  u <- residuals(x)
  p <- x$p
  Tob <- x$obs
  k <- x$K

  sigg1 <- crossprod(u)/(Tob-1-k*p)
  faklow1 <- t(chol(sigg1))

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

  # Defining number of starting rotation angles
  theta_rot <- matrix(runif((k*(k-1)/2)*iter, 0, pi), (k*(k-1)/2), iter)

  # start vectors for iterative optimization approach
  startvec_list <- as.list(as.data.frame(theta_rot))

    if(k == 3){
      testlik <- testlik_3dim
      rmat <- rotmat_3dim
    }else if(k == 4){
      testlik <- testlik_4dim
      rmat <- rotmat_4dim
    }else if(k == 5){
      testlik <- testlik_5dim
      rmat <- rotmat_5dim
    }

    if(is.null(dd)){
      dd <- indepTestSim(Tob, k, verbose=F)
    }

    erg_list <- pblapply(X = startvec_list,
                       FUN = optim,
                       fn = testlik,
                       gr = NULL,
                       faklow = faklow1,
                       u = u,
                       dd = dd,
                       method = "Nelder-Mead",
                       lower = -Inf,
                       upper = Inf,
                       control = list(maxit = 1000),
                       hessian = FALSE, cl = cores)


    # print log-likelihood values from local maxima
    logliks <- sapply(erg_list, "[[", "value")

    # print corresponding parameter vectors column-wise in a matrix
    params <- sapply(erg_list, "[[", "par")

    # calculating B matrix with optimal rotation angle
    par_min <- params[ , which.min(logliks)]
    B_hat <- rmat(par_min, u, faklow1)

    logs <- logliks/(10000000)
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

  result <- list(B = B_hat,       # estimated B matrix (unique decomposition of the covariance matrix)
                 A_hat = A_hat,  # estimated VAR parameter
                 method =        "CvM",
                 obs = Tob,      # number of observations
                 type = type,    # type of the VAR model e.g 'const'
                 y = x$y,        # Data
                 p = x$p,        # number of lags
                 K = x$K,        # number of time series
                 test.stats = sort(logs) # teststatistics
  )
  class(result) <- "svars"
  return(result)
}
