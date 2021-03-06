#' Moving block bootstrap for IRFs of identified SVARs
#'
#' Calculating confidence bands for impulse response via moving block bootstrap
#'
#' @param x SVAR object of class "svars"
#' @param design character. If design="fixed", a fixed design bootstrap is performed. If design="recursive", a recursive design bootstrap is performed.
#' @param b.length Integer. Length of each block
#' @param n.ahead Integer specifying the steps
#' @param nboot Integer. Number of bootstrap iterations
#' @param nc Integer. Number of processor cores
#' @param dd Object of class 'indepTestDist'. A simulated independent sample of the same size as the data.
#' If not supplied, it will be calculated by the function
#' @param signrest A list with vectors containing 1 and -1, e.g. c(1,-1,1), indicating a sign pattern of specific shocks to be tested
#' with the help of the bootstrap samples.
#'@param signcheck Boolean. Whether the sign pattern should be checked for each bootstrap iteration.
#' Note that this procedure is computationally extremely demanding for high dimensional VARs, since the number of possible permutations of B is K!,
#'  where K is the number of variables in the VAR.
#' @param itermax Integer. Maximum number of iterations for DEoptim
#' @param steptol Numeric. Tolerance for steps without improvement for DEoptim
#' @param iter2 Integer. Number of iterations for the second optimization
#' @return A list of class "sboot" with elements
#' \item{true}{Point estimate of impulse response functions}
#' \item{bootstrap}{List of length "nboot" holding bootstrap impulse response functions}
#' \item{SE}{Bootstrapped standard errors of estimated covariance decomposition
#' (only if "x" has method "Cramer von-Mises", or "Distance covariances")}
#' \item{nboot}{Number of bootstrap iterations}
#' \item{design}{character. Whether a fixed design or recursive design bootstrap is performed}
#' \item{b_length}{Length of each block}
#' \item{point_estimate}{Point estimate of covariance decomposition}
#' \item{boot_mean}{Mean of bootstrapped covariance decompositions}
#' \item{signrest}{Evaluated sign pattern}
#' \item{sign_complete}{Frequency of appearance of the complete sign pattern in all bootstrapped covariance decompositions}
#' \item{sign_part}{Frequency of bootstrapped covariance decompositions which conform the complete predetermined sign pattern. If signrest=NULL,
#'  the frequency of bootstrapped covariance decompositions that hold the same sign pattern as the point estimate is provided.}
#' \item{sign_part}{Frequency of single shocks in all bootstrapped covariance decompositions which accord to a specific predetermined sign pattern}
#' \item{cov_bs}{Covariance matrix of bootstrapped parameter in impact relations matrix}
#' \item{method}{Used bootstrap method}
#' \item{VAR}{Estimated input VAR object}
#'
#' @references Brueggemann, R., Jentsch, C., and Trenkler, C., 2016. Inference in VARs with conditional heteroskedasticity of unknown form. Journal of Econometrics 191, 69-85.\cr
#'   Herwartz, H., 2017. Hodges Lehmann detection of structural shocks -
#'        An analysis of macroeconomic dynamics in the Euro Area, Oxford Bulletin of Economics and Statistics.
#'
#' @seealso \code{\link{id.cvm}}, \code{\link{id.dc}}, \code{\link{id.ngml}}, \code{\link{id.garch}}, \code{\link{id.cv}} or \code{\link{id.st}}
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
#' bb <- mb.boot(x1, b.length = 15, nboot = 500, n.ahead = 30, nc = 1, signrest = signrest)
#' summary(bb)
#'
#' # Plotting IRFs with confidance bands
#' plot(bb, lowerq = 0.16, upperq = 0.84)
#'
#' # With different confidence levels
#' plot(bb, lowerq = c(0.05, 0.1, 0.16), upperq = c(0.95, 0.9, 0.84))
#'
#' # Halls percentile
#' plot(bb, lowerq = 0.16, upperq = 0.84, percentile = 'hall')
#'
#' # Bonferroni bands
#' plot(bb, lowerq = 0.16, upperq = 0.84, percentile = 'bonferroni')
#' }
#'
#' @importFrom expm expm
#' @export


mb.boot <- function(x, design = "recursive", b.length = 15, n.ahead = 20, nboot = 500, nc = 1, dd = NULL,
                    signrest = NULL, signcheck = TRUE,  itermax = 300, steptol = 200, iter2 = 50){
  # x: vars object
  # B: estimated covariance matrix from true data set
  # n.ahead: Time steps for Irf
  # nboot: number of bootstrap replications
  if(x$method == "Cramer-von Mises distance" & is.null(dd)){
    dd <- copula::indepTestSim(x$n, x$K, verbose=F)
  }

  sqrt.f <- function(Pstar, Sigma_u_star){
    yy <- suppressMessages(sqrtm(Sigma_u_hat_old))%*%solve(suppressMessages(sqrtm(Sigma_u_star)))%*%Pstar
    return(yy)
  }


  # gathering informations from vars object

  # in case original data came in different format than matrix or ts
  if(!inherits(x$y, c("matrix", "ts"))){
    y = as.matrix(x$y)
  }else{
    y <- x$y
  }
  p <- x$p
  obs <- x$n
  k <- x$K
  B <- x$B
  restriction_matrix = x$restriction_matrix
  restriction_matrix <- get_restriction_matrix(restriction_matrix, k)
  restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])

  if(length(signrest) > k){
    stop('too many sign restrictions')
  }

  # calculating covariance from actual VAR
  A <- x$A_hat
  Z <- t(YLagCr(y, p))

  if(x$type == 'const'){
    Z <- rbind(rep(1, ncol(Z)), Z)
  }else if(x$type == 'trend'){
    Z <- rbind(seq(p + 1, ncol(Z)+ p), Z)
  }else if(x$type == 'both'){
    Z <- rbind(rep(1, ncol(Z)), seq(p + 1, ncol(Z) + p), Z)
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
  for (i in 0:(obs - b.length)) {
    blocks[, , (i + 1)] <- u[(i + 1):(i + b.length),]
  }

  for (i in 1:nboot) {
    epsilon.star <- matrix(0, b.length*(ceiling(N) + 1), ncol(u))
    epsilon.star <- list()
    # stacking randomly selected blocks at each other
    for (kk in 1:(ceiling(N) + 1)) {
      epsilon.star[[kk]] <- blocks[, , floor(runif(1, 1, obs - b.length + 2))]
    }
    epsilon.star <- do.call('rbind', epsilon.star)

    # centering new errors
    for(s in 1:b.length){
      b.mean <- colSums(epsilon.star[1 : (s + (obs - b.length)),])/(obs - b.length + 1)
      for(j in 0:floor(N)){
        epsilon.star[j * b.length + s,] <- epsilon.star[j * b.length + s,] - b.mean
      }
    }

    # cutting of unnecessary observations
    epsilon.star <- epsilon.star[1:obs, ]

    errors[[i]] <- t(epsilon.star)
  }

  # Bootstrapfunction
  bootf <- function(Ustar1){

    if(design == "recursive"){
      Ystar <- matrix(0, nrow(y), k)
      # adding pre sample values
      Ystar[1:p,] <- y[1:p,]

      if (x$type == 'const' | x$type == 'trend') {
        for (i in (p + 1):nrow(y)) {
          for (j in 1:k) {
            Ystar[i, j] <- A[j, 1] + A[j, -1] %*% c(t(Ystar[(i - 1):(i - p), ])) + Ustar1[j, (i - p)]
          }
        }
      } else if (x$type == 'both') {
        for (i in (p + 1):nrow(y)) {
          for (j in 1:k) {
            Ystar[i, j] <- A[j, 1] + A[j, 2] + A[j, -c(1, 2)] %*% c(t(Ystar[(i - 1):(i - p),])) + Ustar1[j, (i - p)]
          }
        }
      }else if (x$type == 'none') {
        for (i in (p + 1):nrow(y)) {
          for (j in 1:k) {
            Ystar[i, j] <- A[j, ] %*% c(t(Ystar[(i - 1):(i - p), ])) + Ustar1[j, (i - p)]
          }
        }
      }

      # Delete pre sample values
      Ystar <- Ystar[-c(1:p), ]

      varb <- suppressWarnings(VAR(Ystar, p = x$p, type = x$type))
      Ustar <- residuals(varb)
      Sigma_u_star <- crossprod(Ustar)/(obs - 1 - k * p)

      if(x$method == "Non-Gaussian maximum likelihood"){
        temp <- id.ngml_boot(varb, stage3 = x$stage3, restriction_matrix = x$restriction_matrix)
      }else if(x$method == "Changes in Volatility"){
        if (length(x$SB) > 3) {
          SB <- x$SB[-c(1:p)]
        } else {
          SB <- x$SB
        }
        temp <- tryCatch(id.cv_boot(varb, SB = SB, SB2 = x$SB2, restriction_matrix = x$restriction_matrix),
                         error = function(e) NULL)
      }else if(x$method == "Cramer-von Mises distance"){
        temp <- id.cvm(varb, itermax = itermax, steptol = steptol, iter2 = iter2, dd)
      }else if(x$method == "Distance covariances"){
        temp <- id.dc(varb, PIT=x$PIT)
      }else if(x$method == "Smooth transition"){
        temp <- id.st(varb, c_fix = x$est_c, transition_variable = x$transition_variable, restriction_matrix = x$restriction_matrix,
                      gamma_fix = x$est_g, max.iter = x$iteration, crit = 0.01)
      }else if(x$method == "GARCH"){
        temp <- tryCatch(id.garch(varb, restriction_matrix = x$restriction_matrix, max.iter = x$max.iter,
                                  crit = x$crit),
                         error = function(e) NULL)
      }else if(x$method == "Cholesky"){
        temp <- id.chol(varb, order_k = x$order_k)
      }
    } else if (design == "fixed") {
      Ystar <- t(A %*% Z + Ustar1)
      Bstar <- t(Ystar) %*% t(Z) %*% solve(Z %*% t(Z))
      Ustar <- Ystar - t(Bstar %*% Z)
      Sigma_u_star <- crossprod(Ustar)/(ncol(Ustar1) - 1 - k * p)

      varb <- list(y = Ystar,
                   coef_x = Bstar,
                   residuals = Ustar,
                   p = p,
                   type = x$type)
      class(varb) <- 'var.boot'

      if(x$method == "Non-Gaussian maximum likelihood"){
        temp <- id.ngml_boot(varb, stage3 = x$stage3, Z = Z, restriction_matrix = x$restriction_matrix)
      }else if(x$method == "Changes in Volatility"){
        temp <- tryCatch(id.cv_boot(varb, SB = x$SB, SB2 = x$SB2, Z = Z, restriction_matrix = x$restriction_matrix),
                         error = function(e) NULL)
      }else if(x$method == "Cramer-von Mises distance"){
        temp <- id.cvm(varb, itermax = itermax, steptol = steptol, iter2 = iter2, dd)
      }else if(x$method == "Distance covariances"){
        temp <- id.dc(varb, PIT=x$PIT)
      }else if(x$method == "GARCH"){
        temp <- tryCatch(id.garch(varb, restriction_matrix = x$restriction_matrix, max.iter = x$max.iter,
                                  crit = x$crit),
                         error = function(e) NULL)
      }else if(x$method == "Cholesky"){
        temp <- id.chol(varb, order_k = x$order_k)
      }else{
        temp <- tryCatch(id.st_boot(varb, c_fix = x$est_c, transition_variable = x$transition_variable, restriction_matrix = x$restriction_matrix,
                                    gamma_fix = x$est_g, max.iter = x$iteration, crit = 0.01, Z = Z),
                         error = function(e) NULL)
      }
    }

    if(!is.null(temp)){
      Pstar <- temp$B

      if (x$method != "Cholesky") {
        if(!is.null(x$restriction_matrix)){
          Pstar1 <- Pstar
          frobP <- frobICA_mod(Pstar1, B, standardize=TRUE)
        }else{
          Pstar1 <- sqrt.f(Pstar, Sigma_u_star)
          diag_sigma_root <- diag(diag(suppressMessages(sqrtm(Sigma_u_hat_old))))

          frobP <- frobICA_mod(t(solve(diag_sigma_root)%*%Pstar1), t(solve(diag_sigma_root)%*%B), standardize=TRUE)
        }
        Pstar <- Pstar1%*%frobP$perm
        temp$B <- Pstar
      }

      ip <- irf(temp, n.ahead = n.ahead)
      return(list(ip, Pstar, temp$A_hat))
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

  ## Obtaining Bootstrap estimates of VAR parameter
  Aboot <- array(0, c(nrow(A), ncol(A),length(bootstraps)))

  for(i in 1:length(bootstraps)){
    Bs[,,i] <- bootstraps[[i]][[2]]
    ipb[[i]] <- bootstraps[[i]][[1]]
    Aboot[, , i] <- bootstraps[[i]][[3]]
  }

  A_hat_boot <- matrix(Aboot, ncol = nrow(A)*ncol(A), byrow = TRUE)
  A_hat_boot_mean <- matrix(colMeans(A_hat_boot), nrow(A), ncol(A))

  # calculating covariance matrix of vectorized bootstrap matrices
  v.b <-  matrix(Bs, ncol = k^2, byrow = T)
  cov.bs <- cov(v.b)

  # Calculating Standard errors for LDI methods
  #if(x$method == "Cramer-von Mises distance" | x$method == "Distance covariances" | x$method == "GARCH"){
  SE <- matrix(sqrt(diag(cov.bs)),k,k)
  rownames(SE) <- rownames(x$B)
  #}else{
  #  SE <- NULL
  #}

  # Calculating Bootstrap means
  boot.mean <- matrix(colMeans(v.b),k,k)
  rownames(boot.mean) <- rownames(x$B)

  # Checking for signs
  if (signcheck == TRUE) {
    if(restrictions > 0 | x$method == 'Cholesky'){
      if(!is.null(signrest)){
        cat('Testing signs only possible for unrestricted model \n')
      }
      sign.part <- NULL
      sign.complete <- NULL
    }else{
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
            check <- rep(FALSE, length(signrest[[i]][!is.na(signrest[[i]])]))
            for(l in 1:k){
              check[l] <- any(all(Bs[!is.na(signrest[[i]]),l,j]/abs(Bs[!is.na(signrest[[i]]),l,j]) == signrest[[i]][!is.na(signrest[[i]])]) |
                                all(Bs[!is.na(signrest[[i]]),l,j]/abs(Bs[!is.na(signrest[[i]]),l,j]) == signrest[[i]][!is.na(signrest[[i]])]*(-1)))
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
    }
  } else {
    sign.part <- NULL
    sign.complete <- NULL
  }


  ## Impulse response of actual model
  ip <- irf(x, n.ahead = n.ahead)

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
                 A_hat = x$A_hat,
                 design = design,
                 A_hat_boot_mean = A_hat_boot_mean,
                 Omodel = x,
                 boot_B = Bs,
                 rest_mat = restriction_matrix,
                 method = 'Moving block bootstrap',
                 VAR = x$VAR,
                 signcheck = signcheck)
  class(result) <- 'sboot'
  return(result)
}
