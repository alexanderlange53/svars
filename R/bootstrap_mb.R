#'Moving block bootstrap for IRFs of identified SVARs
#'
#'Calculating confidence bands for impulse response via moving block bootstrap
#'
#'@param x SVAR object of class "svars"
#'@param b.length Length of each block
#'@param horizon Time horizon of impulse response functions
#'@param nboot Number of bootstrap iterations
#'@param nc Number of processor cores (Not available on windows machines)
#'@param dd Object of class 'indepTestDist'. A simulated independent sample of the same size as the data.
#'If not supplied, it will be calculated by the function
#'@param signrest A list containing vectors with the sign pattern of a specific shock,
#'which should be tested on the apperance in the bootstrap. If no list is supplied,
#'then the function counts the frequency of bootstrapped covariance decompositions that have the same sign pattern as the point estimate.
#'@param itermax Maximum number of iterations for DEoptim
#'@param steptol Tolerance for steps without improvement for DEoptim
#'@param iter2 Number of iterations for the second optimization
#' @return A list of class "sboot" with elements
#' \item{boot_mean}{Mean of bootstrapped covariance decompositions}
#' \item{sign_complete}{Frequency of appearance of the complete sign pattern in all bootstrapped covaraince decompositions}
#' \item{sign_part}{Frequency of appearance of single shocks according to a specific sign pattern
#'  in all bootstrapped covaraince decompositions}
#'
#'@seealso \code{\link{id.cvm}}, \code{\link{id.dc}}, \code{\link{id.ngml}} or \code{\link{id.cv}}
#'
#'@references Brueggemann, R., Jentsch, C., and Trenkler, C. (2016). Inference in VARs with conditional heteroskedasticity of unknown form. Journal of Econometrics 191, 69-85.
#'
#' @examples
#' \donttest{
#' # data contains quarterly observations from 1965Q1 to 2008Q3
#' # x = output gap
#' # pi = inflation
#' # i = interest rates
#' set.seed(23211)
#' v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#' x1 <- id.dc(v1)
#' summary(x1)
#'
#' # impulse response analysis with confidence bands
#' # Checking how often theory based impact relations appear
#' signrest <- list(demand = c(1,1,1), supply = c(-1,1,1), money = c(-1,-1,1))
#' bb <- mb.boot(x1, b.length = 15, nboot = 500, horizon = 30, nc = 1, signrest = signrest)
#' summary(bb)
#' plot(bb, lowerq = 0.16, upperq = 0.84)
#' }
#'
#' @importFrom expm expm
#' @export


mb.boot <- function(x, b.length = 15, horizon, nboot, nc = 1, dd = NULL, signrest = NULL,  itermax = 300, steptol = 200, iter2 = 50){
  # x: vars object
  # B: estimated covariance matrix from true data set
  # horizon: Time horizon for Irf
  # nboot: number of bootstrap replications
  if(x$method == "Cramer-von Mises distance" & is.null(dd)){
    dd <- copula::indepTestSim(x$n, x$K, verbose=F)
  }

  # function to create Z matrix
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

  sqrt.f <- function(Pstar, Sigma_u_star){
    yy <- suppressMessages(sqrtm(Sigma_u_hat_old))%*%solve(suppressMessages(sqrtm(Sigma_u_star)))%*%Pstar
    return(yy)
  }


  # gathering informations from vars object
  y <- x$y
  p <- x$p
  obs <- x$n
  k <- x$K
  B <- x$B

  if(length(signrest) > k){
    stop('too many sign restrictions')
  }

  # calculating covariance from actual VAR
  A <- x$A_hat
  Z <- t(y_lag_cr(y, p)$lags)

  if(x$type == 'const'){
    Z <- rbind(rep(1, ncol(Z)), Z)
  }else if(x$type == 'trend'){
    Z <- rbind(seq(1, ncol(Z)), Z)
  }else if(x$type == 'both'){
    Z <- rbind(rep(1, ncol(Z)), seq(1, ncol(Z)), Z)
  }else{
    Z <- Z
  }

  u <- t(y[-c(1:p),]) - A %*% Z
  Sigma_u_hat_old <- tcrossprod(u)/(obs - 1 - k * p)

  # creating new error terms
  errors <- list()

  # creating blocks
  N <- obs/b.length
  blocks <- array(NA, c(b.length, k, obs - b.length + 1))
  u <- t(u)
  for(i in 0:(obs-b.length)){
    blocks[,,(i+1)] <- u[(i+1):(i+b.length),]
  }

  for(i in 1:nboot){
    epsilon.star <- matrix(0, b.length*ceiling(N), ncol(u))
    epsilon.star <- list()
    # stacking randomly selected blocks at each other
    for(kk in 1:ceiling(N)){
      epsilon.star[[kk]] <- blocks[,,floor(runif(1, 1, obs-b.length+2))]
    }
    epsilon.star <- do.call('rbind', epsilon.star)

    # centering new errors
    for(s in 1:b.length){
      b.mean <- colSums(epsilon.star[1 : (s+(obs - b.length)),])/(obs - b.length +1)
      for(j in 0:floor(N)){
        epsilon.star[j*b.length + s,] <- epsilon.star[j*b.length + s,] - b.mean
      }
    }

    # cutting of unnecessary observations
    epsilon.star <- epsilon.star[1:obs,]

    errors[[i]] <- t(epsilon.star)
  }

  # Bootstrapfunction
  bootf <- function(Ustar1){

    Ystar <- t(A %*% Z + Ustar1)

    Bstar <- t(Ystar) %*% t(Z) %*% solve(Z %*% t(Z))

    Ustar <- t(y[-c(1:p),]) - Bstar %*% Z

    Sigma_u_star <- tcrossprod(Ustar)/(ncol(Ustar1) - 1 - k * p)

    varb <- list(y = y,
                 coef_x = Bstar,
                 residuals = t(Ustar),
                 p = p,
                 type = x$type)
    class(varb) <- 'var.boot'

    if(x$method == "Non-Gaussian maximum likelihood"){
      temp <- id.ngml(varb, stage3 = x$stage3)
    }else if(x$method == "Changes in Volatility"){
      temp <- tryCatch(id.cv(varb, SB = x$SB,max.iter = 5000), error = function(e) NULL)
    }else if(x$method == "Cramer-von Mises distance"){
      temp <- id.cvm(varb, itermax = itermax, steptol = steptol, iter2 = iter2, dd)
    }else{
      temp <- id.dc(varb, PIT=x$PIT)
    }

    if(!is.null(temp)){
      Pstar <- temp$B

      Pstar1 <- sqrt.f(Pstar, Sigma_u_star)
      diag_sigma_root <- diag(diag(suppressMessages(sqrtm(Sigma_u_hat_old))))

      frobP <- frobICA_mod(t(solve(diag_sigma_root)%*%Pstar1), t(solve(diag_sigma_root)%*%B), standardize=TRUE)
      Pstar <- Pstar1%*%frobP$perm

      temp$B <- Pstar

      ip <- imrf(temp, horizon = horizon)
      return(list(ip, Pstar))
    }else{
      return(NA)
    }
  }

  bootstraps <- pblapply(errors, bootf, cl = nc)

  delnull  <-  function(x){
    x[unlist(lapply(x, length) != 0)]
  }

  bootstraps <- lapply(bootstraps, function (x)x[any(!is.na(x))])
  bootstraps <- delnull(bootstraps)

  Bs <- array(0, c(k,k,length(bootstraps)))
  ipb <- list()
  for(i in 1:length(bootstraps)){
    Bs[,,i] <- bootstraps[[i]][[2]]
    ipb[[i]] <- bootstraps[[i]][[1]]
  }

  # calculating covariance matrix of vectorized bootstrap matrices
  v.b <-  matrix(Bs, ncol = k^2, byrow = T)
  cov.bs <- cov(v.b)

  # Calculating Standard errors for LDI methods
  if(x$method == "Cramer-von Mises distance" | x$method == "Distance covariances"){
    SE <- matrix(sqrt(diag(cov.bs)),k,k)
    rownames(SE) <- rownames(x$B)
  }else{
    SE <- NULL
  }

  # Calculating Bootstrap means
  boot.mean <- matrix(colMeans(v.b),k,k)
  rownames(boot.mean) <- rownames(x$B)

  # Checking for signs
  if(is.null(signrest)){
    sign.mat <- matrix(FALSE, nrow = k, ncol = k)
    sign.complete <- 0
    sign.part <- rep(0, times = k)

    for(i in 1:length(bootstraps)){

      pBs <- permutation(Bs[,,i])
      sign.mat <-lapply(pBs, function(z){sapply(1:k, function(ii){all(z[,ii]/abs(z[,ii])  == x$B[,ii]/abs(x$B[,ii])) | all(z[,ii]/abs(z[,ii])  == x$B[,ii]/abs(x$B[,ii])*(-1))})})

      if(any(unlist(lapply(sign.mat, function(sign.mat)all(sign.mat == TRUE))))){
        sign.complete <- sign.complete + 1
      }

      for(j in 1:k){
        check <- rep(FALSE, k)
        for(l in 1:k){
          check[l] <- any(all(pBs[[1]][,l]/abs(pBs[[1]][,l]) == x$B[,j]/abs(x$B)[,j]) | all(pBs[[1]][,l]/abs(pBs[[1]][,l]) == x$B[,j]/abs(x$B)[,j]*(-1)))
        }
        if(sum(check) == 1){
          sign.part[[j]] <- sign.part[[j]] + 1
        }
      }
    }
  }else{
    nrest <- length(signrest)
    sign.part <- rep(list(0), nrest )
    sign.complete <- 0
    for(j in 1:length(bootstraps)){
      check.full <- 0
      for(i in 1:nrest){
        check <- rep(FALSE, k)
        for(l in 1:k){
          check[l] <- any(all(Bs[,l,j]/abs(Bs[,l,j]) == signrest[[i]]) | all(Bs[,l,j]/abs(Bs[,l,j]) == signrest[[i]]*(-1)))
        }
        if(sum(check) == 1){
          sign.part[[i]] <- sign.part[[i]] + 1
          check.full <- check.full + 1
        }
      }
      if(check.full == nrest){
        sign.complete <- sign.complete + 1
      }
    }
    names(sign.part) <- names(signrest)
  }

  ## Impulse response of actual model
  ip <- imrf(x, horizon = horizon)

  result <- list(true = ip,
                 bootstrap = ipb,
                 SE = SE,
                 nboot = nboot,
                 b_length = b.length,
                 point_estimate = x$B,
                 boot_mean = boot.mean,
                 signrest = signrest,
                 sign_complete = sign.complete,
                 sign_part = sign.part,
                 cov_bs = cov.bs,
                 method = 'Moving block bootstrap')
  class(result) <- 'sboot'
  return(result)
}


