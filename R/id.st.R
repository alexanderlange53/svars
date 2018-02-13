#' Identification of SVAR models by means of a smooth transition of volatility
#'
#' Given an estimated VAR model, this function identifies the structural impact matrix B of the corresponding SVAR model
#' by means of smooth transition
#' \deqn{y_t=c_t+A_1 y_{t-1}+...+A_p y_{t-p}+u_t
#' =c_t+A_1 y_{t-1}+...+A_p y_{t-p}+B \epsilon_t.}
#' Matrix B corresponds to the decomposition of the pre-break covariance matrix \eqn{\Sigma_1=B B'}.
#' The post-break covariance corresponds to \eqn{\Sigma_2=B\Lambda B'} where \eqn{\Lambda} is the estimated unconditional heteroskedasticity matrix.
#'
#' @param x An object of class 'vars', 'vec2var', 'nlVar'. Estimated VAR object
#' @param nc Number of processor cores (Not available on windows machines).
#'           Note the smooth transition model is computationally extremely demanding.
#' @param c_lower Start point where the algorithm starts to search for the volatility shift.
#'                Defalt is 0.3*(Total number of observations)
#' @param c_upper End point where the algorithm stops to search for the volatility shift.
#'                Defalt is 0.7*(Total number of observations)
#' @param c_step Step width of c, default is 5.
#' @param c_fix If the break point is known, it can be passed as argument,
#'              where break point = Numberof observations - c_fix
#' @param gamma_lower Lower bound for gamma. Small values indicate a flat transition function. Default is -3
#' @param gamma_upper Upper bound for gamma. Large values indicate a steep transition function. Default is 2
#' @param gamma_step Step width of gamma, default is 0.5
#' @param gamma_fix A fixed parameter for gamma, instead of letting the function search for gamma.
#' @param max.iter Integer. Number of maximum GLS iterations
#' @param crit Integer. Critical value for the precision of the GLS estimation
#' @return A list of class "svars" with elements
#' \item{Lambda}{Estimated heteroscedasticity matrix \eqn{\Lambda}}
#' \item{Lambda_SE}{Matrix of standard errors of Lambda}
#' \item{B}{Estimated structural impact matrix B, i.e. unique decomposition of the covariance matrix of reduced form residuals}
#' \item{B_SE}{Standard errors of matrix B}
#' \item{n}{Number of observations}
#' \item{Fish}{Observed Fisher information matrix}
#' \item{Lik}{Function value of likelihood}
#' \item{wald_statistic}{Results of pairwise Wald tests}
#' \item{iteration}{Number of GLS estimations}
#' \item{method}{Method applied for identification}
#' \item{SB}{Structural break (number of observations)}
#' \item{SBcharacter}{Structural break (date; if provided in function arguments)}
#'
#' @references Rigobon, R., 2003. Identification through Heteroskedasticity. The Review of Economics and Statistics, 85, 777-792.\cr
#'  Herwartz, H. & Ploedt, M., 2016. Simulation Evidence on Theory-based and Statistical Identification under Volatility Breaks Oxford Bulletin of Economics and Statistics, 78, 94-112.
#'
#' @seealso For alternative identification approaches see \code{\link{id.cvm}}, \code{\link{id.dc}} or \code{\link{id.ngml}}
#'
#' @examples
#' \donttest{
#' # data contains quartlery observations from 1965Q1 to 2008Q2
#' # assumed structural break in 1979Q4
#' # x = output gap
#' # pi = inflation
#' # i = interest rates
#' set.seed(23211)
#' v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#' x1 <- id.st(v1)
#' summary(x1)
#'
#' # switching columns according to sign patter
#' x1$B <- x1$B[,c(3,2,1)]
#' x1$B[,3] <- x1$B[,3]*(-1)
#'
#' # Impulse response analysis
#' i1 <- imrf(x1, horizon = 30)
#' plot(i1, scales = 'free_y')
#' }
#' @importFrom steadyICA steadyICA
#' @export

id.st <- function(x, nc = 1, c_lower = 0.3, c_upper = 0.7, c_step = 5, c_fix = NULL,
                  gamma_lower = -3, gamma_upper = 2, gamma_step = 0.5, gamma_fix = NULL,
                  max.iter = 50, crit = 0.01){

  # Gathering informations from reduced form model
  if(inherits(x, "var.boot")){
    u_t <- x$residuals
    Tob <- nrow(u_t)
    k <- ncol(u_t)
    residY <- u_t
  }else{
    u_t <- residuals(x)
    Tob <- nrow(u_t)
    k <- ncol(u_t)
    residY <- u_t
  }

  if(inherits(x, "var.boot")){
    p <- x$p
    y <- t(x$y)
    type = x$type
    coef_x = x$coef_x
  }else if(inherits(x, "varest")){
    p <- x$p
    y <- t(x$y)
  }else if(inherits(x, "nlVar")){
    if(inherits(x, "VECM")){
      stop("id.cv is not available for VECMs")
    }
    p <- x$lag
    y <- t(x$model[, 1:k])
  }else if(inherits(x, "list")){
    p <- x$order
    y <- t(x$data)
  }else{
    stop("Object class is not supported")
  }

  # Function to create a block diagonal matrix
  block.diagonal<-function(...){
    matrixList <- list(...)
    if(is.list(matrixList[[1]])) matrixList<-matrixList[[1]]

    dimensions <- sapply(matrixList,FUN=function(x) dim(x)[1])
    finalDimension <- sum(dimensions)
    finalMatrix <- matrix(0,nrow=finalDimension,ncol=finalDimension)
    index <- 1
    for(k in 1:length(dimensions)){
      finalMatrix[index:(index+dimensions[k]-1),index:(index+dimensions[k]-1)]<-matrixList[[k]]
      index <- index+dimensions[k]
    }
    finalMatrix
  }


  # Function for Z matrix
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

  # Transition function
  transition <- function(gamma, cc, st){
    G <- (1 + exp(-exp(gamma)*(st - cc)))^(-1)
    return(G)
  }

  # Likelihod function
  likelihood.st <- function(parameter, u_t, G){

    if(any(parameter[(k*k+1):(k*k+k)] < 0)){
      return(return(1e25))
    }

    B <- matrix(parameter[1:(k*k)], k, k)
    Lambda <- diag(parameter[(k*k+1):(k*k+k)])
    Sigma_1 <- tcrossprod(B)
    Sigma_2 <- B%*%tcrossprod(Lambda, B)

    Omega <- lapply(G, function(xx, B, Lambda)(1 - xx)*Sigma_1 + xx*Sigma_2,
                    B = B, Lambda = Lambda)
    Omega_inv <- lapply(Omega, function(x)solve(x))

    Likelihood_part1 <- suppressWarnings(lapply(Omega, function(x) log(det(x))))
    Likelihood_part1 <- Reduce('+', Likelihood_part1)

    Omega_huge <- block.diagonal(Omega_inv)
    Likelihood_part2 <- u_t%*%Omega_huge%*%u_t

    L <- -0.5*Likelihood_part1 - 0.5*Likelihood_part2

    if(!is.na(L)){
      return(-L)
    }else{
      return(1e25)
    }

  }

  if(is.null(gamma_fix) &  is.null(c_fix)){
    # Creating grid for iterative procedure with gamma and break point unknown
    gamma_grid <-  seq(gamma_lower, gamma_upper, by = gamma_step)
    cc_grid <- seq(ceiling(c_lower*Tob), floor(c_upper*Tob), by = c_step)
    grid_comb <- unique(expand.grid(gamma_grid, cc_grid))
    G_grid <- mapply(transition, grid_comb[,1], grid_comb[,2], MoreArgs = list(st = seq(1:Tob)))
  }else if(is.null(gamma_fix)){
    # Creating grid for iterative procedure with fix break point
    gamma_grid <-  seq(gamma_lower, gamma_upper, by = gamma_step)
    grid_comb <- unique(expand.grid(gamma_grid, c_fix))
    G_grid <- mapply(transition, grid_comb[,1], grid_comb[,2], MoreArgs = list(st = seq(1:Tob)))
  }else if(is.null(c_fix)){
    # Creating grid for iterative procedure with fix shape of transition function
    cc_grid <- seq(ceiling(c_lower*Tob), floor(c_upper*Tob), by = c_step)
    grid_comb <- unique(expand.grid(gamma_fix, cc_grid))
    G_grid <- mapply(transition, grid_comb[,1], grid_comb[,2], MoreArgs = list(st = seq(1:Tob)))
  }else{
    grid_comb <- unique(expand.grid(gamma_fix, c_fix))
    G_grid <- mapply(transition, gamma_fix, c_fix, MoreArgs = list(st = seq(1:Tob)))
  }


  # iterative approach
  iterative_smooth_transition <- function(transition, u_t, y, Tob, k, p, crit, max.iter){

    count <- 0 # count variable
    Exit <-  -100  #Exit criterion

    # Creating initial values for structural parameter
    Sigma_hat <- crossprod(u_t)/(Tob-1-k*p)

    init_B <- t(chol(Sigma_hat))
    init_Lambda <- diag(k)

    B_hat <- list(init_B)
    Lambda_hat <- list(init_Lambda)
    u_t2 = c(t(u_t))
    ll <- list(likelihood.st(parameter = c(init_B, diag(init_Lambda)), u_t = u_t2, G = transition))


    while( (abs(Exit) > crit) & (count < max.iter) ){
      count <- count + 1

      if(count == 1){
        u_t_gls <- u_t
      }

      parameter <- c(B_hat[[count]], diag(Lambda_hat[[count]]))

      # Step 1: Optimizing likelihood
      u_t2 = c(t(u_t_gls))
      mle <- tryCatch(
                    nlm(f = likelihood.st, p = parameter, u_t = u_t2, G = transition,
                    hessian = T), error = function(e) NULL)
      if(is.null(mle)){
        count2 <- 0
        while(is.null(mle) & count2 < 25){
          MW <- -1
          #MW2 <- -1
          while(MW < 0.5){
            B_help <- suppressMessages(expm::sqrtm((1/Tob)* crossprod(u_t_gls))) + matrix(runif(k*k), nrow = k, byrow = T)
            MW <- det(tcrossprod(B_help))
          }
          parameter <- c(B_help, rep(1,k))
          mle <- tryCatch(
            nlm(f = likelihood.st, p = parameter, u_t = u_t_gls, G = transition,
                  hessian = T), error = function(e) NULL)
          count2 <- count2 + 1
        }
        if(is.null(mle)){
          return(NA)
        }
      }

      B_hat <- c(B_hat, list(matrix(mle$estimate[1:(k*k)], nrow = k)))
      Lambda_hat <- c(Lambda_hat, list(diag(mle$estimate[(k*k+1):(k*k+k)])))
      ll <- c(ll, list(mle$minimum))
      if(count == 1){
        hessian <- list(solve(mle$hessian))
      }else{
        hessian <- c(hessian, list(solve(mle$hessian)))
      }


      # Step 2: Reestimation of VAR parameter with GLS
      yl <- t(y_lag_cr(t(y), p)$lags)
      yret <- y
      y_loop <- y[,-c(1:p)]

      if(x$type == 'const'){
        Z_t <- rbind(rep(1, ncol(yl)), yl)
      }else if(x$type == 'trend'){
        Z_t <- rbind(seq(1, ncol(yl)), yl)
      }else if(x$type == 'both'){
        Z_t <- rbind(rep(1, ncol(yl)), seq(1, ncol(yl)), yl)
      }else{
        Z_t <- yl
      }

      Omega_i <- lapply(transition, function(x, B, Lambda){solve((1 - x)*tcrossprod(B, B) + x*B%*%tcrossprod(Lambda, B))},
                        B = B_hat[[(count + 1)]], Lambda = Lambda_hat[[(count + 1)]])

      W <- block.diagonal(Omega_i)

      b_gls <- solve(kronecker(Z_t, diag(k))%*%W%*%kronecker(t(Z_t), diag(k)))%*%kronecker(Z_t, diag(k))%*%W%*%c(y_loop)

      if(count == 1){
        GLSE <- list(b_gls)
      }else{
        GLSE <- c(GLSE, list(b_gls))
      }

      u_t_gls <- c(y_loop) - kronecker(t(Z_t), diag(k))%*%b_gls
      u_t_gls <- matrix(u_t_gls, Tob, k)

      if(count > 1){
        Exit <- ll[[count+1]] - ll[[count]]
      }
    }

    # extracting the best estimates
    ll <- unlist(ll)
    ll_best <- ll[which.min(ll)]
    cc <- which.min(ll)
    B_hat <- B_hat[[cc]]
    Lambda_hat <- Lambda_hat[[cc]]
    GLSE <- GLSE[[cc-1]]
    GLSE <- matrix(GLSE, nrow = k)

    # Optaining standard errors
    HESS <- hessian[[cc-1]]
    for(i in 1:nrow(HESS)){
      if(HESS[i,i] < 0){
        HESS[,i] <- -HESS[,i]
      }
    }
    FishObs <- sqrt(diag(HESS))
    B.SE <- matrix(FishObs[1:(k*k)], k,k)
    Lambda.SE <- diag(FishObs[(k*k+1):(k*k+k)])

    # Testing the estimated SVAR for identification by menas of wald statistic
    # wald <- wald.test(Lambda_hat, HESS, restrictions)

    return(list(
      Lambda = Lambda_hat,    # estimated Lambda matrix (unconditional heteroscedasticity)
      Lambda_SE = Lambda.SE,  # standard errors of Lambda matrix
      B = B_hat,              # estimated B matrix (unique decomposition of the covariance matrix)
      B_SE = B.SE,            # standard errors of B matrix
      Fish = HESS,            # observerd fisher information matrix
      Lik = -ll_best,             # function value of likelihood
      iteration = count,     # number of gls estimations
      A_hat = GLSE             # VAR parameter estimated with gls
    ))
  }

  if(!is.null(gamma_fix) &  !is.null(c_fix)){
    best_estimation <- iterative_smooth_transition(G_grid, u_t = u_t, y = y, Tob = Tob, k = k,
                                           p = p, crit = crit, max.iter = max.iter)

  }else{
    G_grid <- apply(G_grid, 2, list)

    grid_optimization <- pblapply(G_grid, function(x){iterative_smooth_transition(unlist(x),
                                                                                  u_t = u_t, y = y,
                                                                                  Tob = Tob, k = k,
                                                                                  p = p, crit = crit,
                                                                                  max.iter = max.iter)},
                                  cl = nc)
    # Checking for NA's
    if(any(is.na(grid_optimization))){
      G_grid[[which(is.na(grid_optimization))]] <- NULL
      grid_optimization <- lapply(grid_optimization, function (x)x[any(!is.na(x))])
      delnull  <-  function(x){
        x[unlist(lapply(x, length) != 0)]
      }
      grid_optimization <- delnull(grid_optimization)
    }

    max_likelihood <- which.max(sapply(grid_optimization, '[[', 'Lik'))
    best_estimation <- grid_optimization[[max_likelihood]]
    transition_function <- unlist(G_grid[[max_likelihood]])
  }



  # Testing the estimated SVAR for identification by means of wald statistic
  wald <- wald.test(best_estimation$Lambda, best_estimation$Fish, 0)

  result <- list(
    Lambda = best_estimation$Lambda,        # estimated Lambda matrix (unconditional heteroscedasticity)
    Lambda_SE = best_estimation$Lambda_SE,  # standard errors of Lambda matrix
    B = best_estimation$B,                  # estimated B matrix (unique decomposition of the covariance matrix)
    B_SE = best_estimation$B_SE,            # standard errors of B matrix
    n = Tob,                                # number of observations
    Fish = best_estimation$Fish,            # observerd fisher information matrix
    Lik = best_estimation$Lik,              # function value of likelihood
    wald_statistic = wald,                  # results of wald test
    iteration = best_estimation$iteration,  # number of gls estimations
    method = "Smooth transition",
    SB = grid_comb[max_likelihood,2],       # Structural Break point
    transition_coefficient = grid_comb[max_likelihood,1], # Parameter which determines the shape of thetransition function
    comb = nrow(grid_comb),                 # number of all evaluated combinations of gamma and c
    transition_function = transition_function,
    A_hat = best_estimation$A_hat,          # VAR parameter estimated with gls
    type = x$type,          # type of the VAR model e.g 'const'
    y = t(y),                # Data
    p = p,                # number of lags
    K = k                 # number of time series
  )

  class(result) <- 'svars'
  return(result)
}
