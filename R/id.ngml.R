#' Non-Gaussian maximum likelihood identification of SVAR models
#'
#' Given an estimated VAR model, this function applies identification by means of a non-Gaussian likelihood for the structural impact matrix B of the corresponding SVAR model
#' \deqn{y_t=c_t+A_1 y_{t-1}+...+A_p y_{t-p}+u_t   =c_t+A_1 y_{t-1}+...+A_p y_{t-p}+B \epsilon_t.}
#' Matrix B corresponds to the unique decomposition of the least squares covariance matrix \eqn{\Sigma_u=B B'} if the vector of structural shocks \eqn{\epsilon_t} contains at most one Gaussian shock (Comon, 94).
#' A likelihood function of independent t-distributed structural shocks \eqn{\epsilon_t=B^{-1}u_t} is maximized with respect to the entries of B and the degrees of freedom of the t-distribution (Lanne et al., 2017).
#'
#' @param x An object of class 'vars', 'vec2var', 'nlVar'. Estimated VAR object
#' @param stage3 Logical. If stage3="TRUE", the VAR parameters are estimated via non-gaussian maximum likelihood (computationally demanding)
#' @param restriction_matrix Matrix. A matrix containing presupposed entries for matrix B, NA if no restriction is imposed (entries to be estimated)
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
#' \item{method}{Method applied for identification}
#' \item{n}{Number of observations}
#' \item{type}{Type of the VAR model, e.g. 'const'}
#'
#'@references Lanne, M., Meitz, M., Saikkonen, P., 2017. Identification and estimation of non-Gaussian structural vector autoregressions. J. Econometrics 196 (2), 288-304.\cr
#'Comon, P., 1994. Independent component analysis, A new concept?, Signal Processing, 36, 287-314
#'
#' @seealso For alternative identification approaches see \code{\link{id.st}}, \code{\link{id.cvm}}, \code{\link{id.dc}} or \code{\link{id.cv}}
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
#' i1 <- irf(x1, n.ahead = 30)
#' plot(i1, scales = 'free_y')
#' }
#' @importFrom tsDyn VARrep
#' @export


#------------------------------------------------------#
## Identification via non-Gaussian maximum likelihood ##
#------------------------------------------------------#

id.ngml <- function(x, stage3 = FALSE, restriction_matrix = NULL){

  if(inherits(x, "var.boot")){
    u <- x$residuals
    Tob <- nrow(u)
    k <- ncol(u)
    residY <- u
  }else{
    u <- residuals(x)
    Tob <- nrow(u)
    k <- ncol(u)
    residY <- u
  }

  if(inherits(x, "var.boot")){
    p <- x$p
    y <- t(x$y)
    yOut <- x$y
    type = x$type
    coef_x = x$coef_x
  }else if(inherits(x, "varest")){
    p <- x$p
    y <- t(x$y)
    yOut <- x$y
    type = x$type
    coef_x = coef(x)
  }else if(inherits(x, "nlVar")){
    p <- x$lag
    y <- t(x$model[, 1:k])
    yOut <- x$model[, 1:k]
    coef_x <- t(coef(x))

    if(inherits(x, "VECM")){
      coef_x <- t(VARrep(x))
    }

    if(rownames(coef_x)[1] %in% c("Intercept", "constant")){
      coef_x <- coef_x[c(2:nrow(coef_x),1),]

    }else if(rownames(coef_x)[1] == "Trend"){
      coef_x <- coef_x[c(2:nrow(coef_x),1),]
    }
    if(rownames(coef_x)[1] %in% c("Intercept", "constant", "Trend")){
      coef_x <- coef_x[c(2:nrow(coef_x),1),]
    }
    type <- x$include
    coef_x <- split(coef_x, rep(1:ncol(coef_x), each = nrow(coef_x)))
    coef_x <- lapply(coef_x, as.matrix)
  }else if(inherits(x, "list")){
    p <- x$order
    y <- t(x$data)
    y <- x$data
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
    yOut <- x$y
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

  if(!is.null(restriction_matrix)){
    resultUnrestricted <- identifyNGML(x = x, coef_x = coef_x, Sigma_hat = Sigma_hat, u = u, k = k, p = p, Tob = Tob, yOut = yOut, type = type, il = il,
                                       rows = rows, stage3 = stage3, restriction_matrix = NULL, restrictions = 0, y = y)
    result <- identifyNGML(x = x, coef_x = coef_x, Sigma_hat = Sigma_hat, u = u, k = k, p = p, Tob = Tob, yOut = yOut, type = type, il = il,
                           rows = rows, stage3 = stage3, restriction_matrix = restriction_matrix, restrictions = restrictions, y = y)

    lRatioTestStatistic = 2 * (resultUnrestricted$Lik - result$Lik)
    pValue = round(1 - pchisq(lRatioTestStatistic, result$restrictions), 4)

    lRatioTest <- data.frame(testStatistic = lRatioTestStatistic, p.value = pValue)
    rownames(lRatioTest) <- ""
    colnames(lRatioTest) <- c("Test statistic", "p-value")
    result$lRatioTest <- lRatioTest
  }else{
    restriction_matrix <- NULL
    result <- identifyNGML(x = x, coef_x = coef_x, Sigma_hat = Sigma_hat, u = u, k = k, p = p, Tob = Tob, yOut = yOut, type = type, il = il,
                           rows = rows, stage3 = stage3, restriction_matrix = restriction_matrix, restrictions = restrictions, y = y)
     }
  class(result) <- "svars"
  return(result)
}
