#' Non-Gaussian maximum likelihood identification of SVAR models
#'
#' Given an estimated VAR model, this function applies identification by means of a non-Gaussian likelihood for the structural impact matrix B of the corresponding SVAR model
#' \deqn{y_t=c_t+A_1 y_{t-1}+...+A_p y_{t-p}+u_t   =c_t+A_1 y_{t-1}+...+A_p y_{t-p}+B \epsilon_t.}
#' Matrix B corresponds to the unique decomposition of the least squares covariance matrix \eqn{\Sigma_u=B B'} if the vector of structural shocks \eqn{\epsilon_t} contains at most one Gaussian shock (Comon, 94).
#' A likelihood function of independent t-distributed structural shocks \eqn{\epsilon_t=B^{-1}u_t} is maximized with respect to the entries of B and the degrees of freedom of the t-distribution (Lanne et al., 2017).
#'
#' @param x An object of class 'vars', 'vec2var', 'nlVar'. Estimated VAR object
#' @param stage3 Logical. If stage3="TRUE", the VAR parameters are estimated via non-gaussian maximum likelihood (computationally demanding)
#' @return A list of class "svars" with elements
#' \item{B}{Estimated structural impact matrix B, i.e. unique decomposition of the covariance matrix of reduced form errors}
#' \item{sigma}{Estimated scale of the standardized matrix B_stand, i.e. \eqn{B=B_stand*diag(\sigma_1,...,\sigma_K)}}
#' \item{sigma_SE}{Standard errors of the scale}
#' \item{df}{Estimated degrees of freedom}
#' \item{df_SE}{Standard errors of the degrees of freedom}
#' \item{Fish}{Observed Fisher information matrix}
#' \item{A_hat}{Estimated VAR parameter}
#' \item{B_stand}{Estimated standardized structural impact matrix}
#' \item{B_stand_SE}{Standard errors of standardized matrix B_stand}
#' \item{Lik}{Function value of likelihood}
#' \item{method}{Method applied for identifaction}
#' \item{n}{Number of observations}
#' \item{type}{Type of the VAR model, e.g. 'const'}
#'
#'@references Lanne, M., Meitz, M., Saikkonen, P., 2017. Identification and estimation of non-Gaussian structural vector autoregressions. J. Econometrics 196 (2), 288-304.\cr
#'Comon, P., 1994. Independent component analysis, A new concept?, Signal Processing, 36, 287-314
#'
#' @seealso For alternative identification approaches see \code{\link{id.cvm}}, \code{\link{id.dc}} or \code{\link{id.cv}}
#'
#' @examples
#' \donttest{
#' # data contains quarterly observations from 1965Q1 to 2008Q3
#' # x = output gap
#' # pi = inflation
#' # i = interest rates
#' set.seed(23211)
#' v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#' x1 <- id.ngml(v1)
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
#' @importFrom tsDyn VARrep
#' @export


#------------------------------------------------------#
## Identification via non-Gaussian maximum likelihood ##
#------------------------------------------------------#


id.ngml <- function(x, stage3 = FALSE){

  # likelihood function to optimize
  loglik <-function(theta) {

    Tob <- nrow(u)
    K <- ncol(u)

    beta <- theta[1:(K*(K-1))]
    sigma <- theta[(K*(K-1)+1):K^2]
    lambda <- theta[(K^2+1):length(theta)]

    B_hat <- function(beta){
      B_hat <- diag(K)
      B_hat[row(B_hat)!=col(B_hat)] <- beta
      return(B_hat)
    }

    if(all(sigma >0) & det(B_hat(beta))>0 & all(lambda > 0)){

      logl <- rep(0, Tob)

      mid <- il %*% kronecker(diag(K), solve(B_hat(beta)))
      midd <- mid[rows, ]
      mid1 <- matrix(0, nrow = K, ncol = K)
      for(i in 1:K){
        if(i == 1){
          mid1[1, 1:K] <- midd[1, 1:K]
        }else{
          mid1[i, 1:K] <- midd[i, ((K+1+K*(i-2)):(i*K))]
        }
      }

      l_t <- function(uu, sigma, lambda, mid1, beta){
        l <-  sum(log(dt((sigma)^(-1) * (mid1 %*% uu), lambda ))) - log(det(B_hat(beta))) - sum(log(sigma))
        return(l)
      }

      logl <- sum(apply(X = u, MARGIN = 1, FUN = l_t, sigma = sigma, lambda = lambda, mid1 = mid1, beta = beta))

      return(-logl)

    } else {return(NA)}
  }

  resid.ls <- function(Z_t, k, A){
    term1 <- kronecker(t(Z_t), diag(k))%*%A
    return(term1)
  }

  loglik2 <-function(A) {

      logl <- rep(0, Tob)

      term1 <- apply(Z_t, 2, resid.ls, k = k, A = A)
      uu <- t(y) - t(term1)

      mid <- il %*% kronecker(diag(k), solve(B_stand_est))
      midd <- mid[rows, ]
      mid1 <- matrix(0, nrow = k, ncol = k)
      for(i in 1:k){
        if(i == 1){
          mid1[1, 1:k] <- midd[1, 1:k]
        }else{
          mid1[i, 1:k] <- midd[i, ((k+1+k*(i-2)):(i*k))]
        }
      }

      l_t <- function(uu, mid1){
        l <-  sum(log(dt((sigma_est)^(-1) * (mid1 %*% uu), d_freedom ))) - log(det(B_stand_est)) - sum(log(sigma_est))
        return(l)
      }

      logl <- sum(apply(X = uu, MARGIN = 1, FUN = l_t, mid1 = mid1))

      return(-logl)
  }

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

  ########### starting the computations ------------------------------------------------------------------------


  if(is.null(residuals(x))){
    stop("No residuals retrieved from model")
  }
  u <- residuals(x)
  Tob <- nrow(u)
  k <- ncol(u)
  residY <- u

  if(inherits(x, "varest")){
    p <- x$p
    y <- t(x$y)
    type = x$type
    coef_x = coef(x)
  }else if(inherits(x, "nlVar")){
    p <- x$lag
    y <- t(x$model[, 1:k])
    coef_x <- t(coef(x))

    if(inherits(x, "VECM")){
     coef_x <- t(VARrep(x))
    }

    if(rownames(coef_x)[1] %in% c("Intercept", "constant")){
      coef_x <- coef_x[c(2:nrow(coef_x),1),]
      type = "const"
    }else if(rownames(coef_x)[1] == "Trend"){
      coef_x <- coef_x[c(2:nrow(coef_x),1),]
      type <- "trend"
    }
    if(rownames(coef_x)[1] %in% c("Intercept", "constant", "Trend")){
      coef_x <- coef_x[c(2:nrow(coef_x),1),]
      type <- "both"
    }
    coef_x <- split(coef_x, rep(1:ncol(coef_x), each = nrow(coef_x)))
    coef_x <- lapply(coef_x, as.matrix)
  }else if(inherits(x, "list")){
    p <- x$order
    y <- t(x$data)
    coef_x <- x$coef
    if(x$cnst == TRUE){
      coef_x <- coef_x[c(2:nrow(coef_x),1),]
      type = "const"
    }
    coef_x <- split(coef_x, rep(1:ncol(coef_x), each = nrow(coef_x)))
    coef_x <- lapply(coef_x, as.matrix)

    }else if(inherits(x, "vec2var")){
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

  # calculating the covariance matrix
  Sigma_hat <- crossprod(residY)/(Tob-1-k*p)

  # choleski decomposition of sigma_u
  B_l <- t(chol(Sigma_hat))
  # standardized choleski decomp
  B_l_st <- B_l%*%solve(diag(diag(B_l)))

  # starting values
  beta0 <- B_l_st[row(B_l)!=col(B_l)]
  sigma0 <- rep(1,k)
  lambda0 <- rep(5,k)
  theta0 <- c(beta0,sigma0,lambda0)

  # Creating selection matrix for likelihood
  il <- matrix(0, k*k, k*k)
  rows <- rep(0, k)
  for(i in 1:k){
    if(i == 1){
      il[i,i] <- 1
      rows[i] <- 1
    }else{
      il[(i+(k*(i-1))),(i+(k*(i-1)))] <- 1
      rows[i] <- i+(k*(i-1))
    }
  }

  # optimizing the likelihood function 2. stage
  maxL <- optim(theta0, loglik, method = 'BFGS', hessian = TRUE)
  beta_est <- maxL$par[1:(k*k-k)]

  sigma_est <- maxL$par[(k*k-k+1):(k*k)]
  B_stand_est <- diag(k)
  B_stand_est[row(B_stand_est)!=col(B_stand_est)] <- beta_est
  B_mle <- B_stand_est%*%diag(sigma_est)
  d_freedom <- maxL$par[(k*k+1):(k*k+k)]
  ll <- maxL$value

  # obating standard errors from observed fisher information
  HESS <- solve(maxL$hessian)
  for(i in 1:nrow(HESS)){
    if(HESS[i,i] < 0){
      HESS[,i] <- -HESS[,i]
    }
  }
  FishObs <- sqrt(diag(HESS))
  B.SE <- matrix(0, k, k)
  B.SE[row(B.SE)!=col(B.SE)] <- FishObs[1:(k*k-k)]
  sigma_SE <- FishObs[(k*k-k+1):(k*k)]
  d_SE <- FishObs[(k*k+1):(k*k+k)]

  # getting variances and covariances for S.E. of non stand B
  covariance <- HESS[1:(k*k-k),((k*k-k+1):(k*k))]
  B.SE.2 <- diag(sigma_SE)


  jj <- 0
  for(i in 1:k){
    for(j in 1:k){
      if(i != j){
        jj <- jj + 1
        B.SE.2[j,i] <- sqrt(2*covariance[jj,i]^2 + (B.SE[j,i]^2 + B_stand_est[j,i]^2)*(sigma_SE[i]^2 + sigma_est[i]^2) -
                       (covariance[jj, i] + B_stand_est[j,i] * sigma_est[i])^2)
      }
    }
  }

  # Estimating VAR parameter 3. stage
  if(stage3 == TRUE){
    #y <- t(x$y)
    yl <- t(y_lag_cr(t(y), p)$lags)

    y <- y[,-c(1:p)]

    A <- matrix(0, nrow = k, ncol = k*p)

    for(i in 1:k){
      A[i,] <- coef_x[[i]][1:(k*p),1]
    }

    if(type == "const"){
      v <- rep(1, k)

      for(i in 1:k){
        v[i] <- coef_x[[i]][(k*p+1), 1]
      }

      A <- cbind(v, A)
      Z_t <- rbind(rep(1, ncol(yl)), yl)
    }else if (type == "trend"){
      trend <- rep(1, k)

      for(i in 1:k){
        trend[i] <- coef_x[[i]][(k*p+1), 1]
      }

      A <- cbind(trend, A)
      Z_t <- rbind(seq(1, ncol(yl)), yl)
    }else if(type == "both"){
      v <- rep(1, k)

      for(i in 1:k){
        v[i] <- coef_x[[i]][(k*p+1), 1]
      }

      trend <- rep(1, k)
      Z_t <- rbind(rep(1, ncol(yl)), seq(1, ncol(yl)), yl)
      for(i in 1:k){
        trend[i] <- coef_x[[i]][(k*p+2), 1]
      }

      A <- cbind(v, trend, A)
    }else{
      Z_t <- yl
    }

    A <- c(A)
    maxL2 <- optim(A, loglik2, method = 'BFGS', hessian = TRUE)

    A_hat <- matrix(maxL2$par, nrow = k)
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


  # ordering the columns with respect to the largest absolute values in each column
  B_hat_ord <- matrix(0, k, k)
  control <- rep(0, k)
  for(i in 1:ncol(B_mle)){
    for(j in 1:ncol(B_mle)){
      if(which.max(abs(B_mle[, j])) == i){
        if(control[i] == 0){
          control[i] <- j
          B_hat_ord[, i] <- B_mle[, j]
        }else{
          if(max(B_mle[, j]) > max(B_mle[, control[i]])){
            control[i] <- j
            B_hat_ord[, i] <- B_mle[, j]
          }
        }
      }
    }
  }

  # checking if any column in the orderd matrix is empty and replace it with the unused column in the estimated matrix
  if(any(control == 0)){
    hc <- sapply(1:k, function(x){any(x == control)})
    B_hat_ord[, which(control == 0)] <- B_mle[, which(hc == FALSE)]
    control[which(control == 0)] <- which(hc == FALSE)
  }

  # checking for negative values on the main diagonal
  for(i in 1:ncol(B_hat_ord)){
    if(B_hat_ord[i,i] < 0){
      B_hat_ord[, i] <- B_hat_ord[, i]*(-1)
    }
  }

  result <- list(B = B_hat_ord,       # estimated B matrix (unique decomposition of the covariance matrix)
              B_SE = B.SE.2,          # standard errors
              sigma = sigma_est,      # estimated scale of the standardized B
              sigma_SE = sigma_SE,    # standard errors of the scale
              df = d_freedom,         # estimated degrees of freedom of the distribution
              df_SE = d_SE,           # standard errors of the degrees of freedom
              Fish = HESS,            # observed fisher information matrix
              A_hat = A_hat,          # estimated VAR parameter
              B_stand = B_stand_est,  # estimated standardized B matrix
              B_stand_SE = B.SE ,     # standard errors
              Lik = -ll,              # value of maximum likelihood
              method = "Non-Gaussian maximum likelihood",
              n = Tob,              # number of observations
              type = type,            # type of the VAR model e.g 'const'
              y = t(y),                # Data
              p = p,                # number of lags
              K = k,                # number of time series
              stage3 = stage3
              )
         class(result) <- "svars"
         return(result)
}
