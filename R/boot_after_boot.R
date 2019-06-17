#' Bootstrap after Bootstrap
#'
#' Bootstrap intervals based on bias-adjusted estimators
#'
#' @param x SVAR object of class "sboot"
#' @param Croot Numeric. Threshold of the modulus of the highest root of the companion Matrix associated with the VAR parameter of the original reduced form model.
#' @param nc Integer. Number of processor cores (Not available on windows machines)
#' @return A list of class "sboot" with elements
#' \item{true}{Point estimate of impulse response functions}
#' \item{bootstrap}{List of length "nboot" holding bootstrap impulse response functions}
#' \item{SE}{Bootstraped standard errors of estimated covariance decomposition
#' (only if "x" has method "Cramer von-Mises", or "Distance covariances")}
#' \item{nboot}{Number of bootstrap iterations}
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
#'
#' @references Kilian, L. (1998). Small-sample confidence intervals for impulse response functions. Review of Economics and Statistics 80, 218-230.
#'
#' @seealso \code{\link{mb.boot}}, \code{\link{wild.boot}}
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
#' # Bootstrap
#' bb <- mb.boot(x1, b.length = 15, nboot = 300, n.ahead = 30, nc = 1, signrest = NULL)
#' summary(bb)
#' plot(bb, lowerq = 0.16, upperq = 0.84)
#'
#' # Bias-adjusted bootstrap
#' bb2 <- ba.boot(bb, nc = 1)
#' plot(bb2, lowerq = 0.16, upperq = 0.84)
#' }
#'
#' @export


ba.boot <- function(x, Croot = 1,  nc = 1){

  if(class(x) != "sboot"){
    stop("THe bootstrap-after-bootstrap can only be applied to already bootstrapped objects.")
  }
  # Calculating difference between sample estimate and bootstrap estimate
  Psi <- x$A_hat_boot_mean - x$A_hat

  # Checking largest root of companion matrix
  if (Oroots(x$A_hat, x$Omodel$K, x$Omodel$p)[1] >= Croot) {
    A <- x$A_hat
  } else {
    A <- x$A_hat - Psi
  }


  # Iterative bias correcting AR parameter if necessarry
  count <- 0
  delta_loop <- 1
  Psi_loop <- Psi
  A_loop <- A
  while (Oroots(A, x$Omodel$K, x$Omodel$p)[1] >= Croot) {
    A <- A_loop - Psi_loop * delta_loop
    delta_loop <- delta_loop - 0.01
    count <- count + 1
  }

  # Replace original AR parameter with Bias corrected slope parameter
  x$Omodel$A_hat <- A

  # Resample again
  if(x$method == "Wild bootstrap"){
    result <- wild.boot(x$Omodel, recursive = x$recursive, rademacher = x$rademacher, n.ahead = NROW(x$true$irf), signrest = x$signrest,
                        nboot = x$nboot, nc = nc)
  }else{
    result <- mb.boot(x$Omodel, recursive = x$recursive, b.length =  x$b_length, n.ahead = NROW(x$true$irf), signrest = x$signrest,
                        nboot = x$nboot, nc = nc)
  }

  result$BC <- Psi # Bias correction Term
  result$count <- count # Number of bias corrections needed
  result$root <- Oroots(A, x$Omodel$K, x$Omodel$p)[1] # Modulus of highest root

  result$true <- x$true # Using original point estimate

  return(result)
}

Oroots <-  function(A, k, p){
  companion <- matrix(0, nrow = k * p, ncol = k * p)
  companion[1:k, 1:(k * p)] <- A[, (ncol(A) - k * p + 1) : ncol(A)]
  if(p > 1){
    j <- 0
    for( i in (k + 1) : (k * p)){
      j <- j + 1
      companion[i, j] <- 1
    }
  }
  roots <- eigen(companion)$values
  return(Mod(roots))
}



