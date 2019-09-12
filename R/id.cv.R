#' Changes in volatility identification of SVAR models
#'
#' Given an estimated VAR model, this function applies changes in volatility to identify the structural impact matrix B of the corresponding SVAR model
#' \deqn{y_t=c_t+A_1 y_{t-1}+...+A_p y_{t-p}+u_t
#' =c_t+A_1 y_{t-1}+...+A_p y_{t-p}+B \epsilon_t.}
#' Matrix B corresponds to the decomposition of the pre-break covariance matrix \eqn{\Sigma_1=B B'}.
#' The post-break covariance corresponds to \eqn{\Sigma_2=B\Lambda B'} where \eqn{\Lambda} is the estimated unconditional heteroskedasticity matrix.
#'
#' @param x An object of class 'vars', 'vec2var', 'nlVar'. Estimated VAR object
#' @param SB Integer, vector or date character. The structural break is specified either by an integer (number of observations in the pre-break period),
#'                    a vector of ts() frequencies if a ts object is used in the VAR or a date character. If a date character is provided, either a date vector containing the whole time line
#'                    in the corresponding format (see examples) or common time parameters need to be provided
#' @param dateVector Vector. Vector of time periods containing SB in corresponding format
#' @param start Character. Start of the time series (only if dateVector is empty)
#' @param end Character. End of the time series (only if dateVector is empty)
#' @param frequency Character. Frequency of the time series (only if dateVector is empty)
#' @param format Character. Date format (only if dateVector is empty)
#' @param restriction_matrix Matrix. A matrix containing presupposed entries for matrix B, NA if no restriction is imposed (entries to be estimated). Alternatively, a K^2*K^2 matrix can be passed, where ones on the diagonal designate unrestricted and zeros restricted coefficients. (as suggested in Luetkepohl, 2017, section 5.2.1).
#' @param max.iter Integer. Number of maximum GLS iterations
#' @param crit Numeric. Critical value for the precision of the GLS estimation
#' @return A list of class "svars" with elements
#' \item{Lambda}{Estimated unconditional heteroscedasticity matrix \eqn{\Lambda}}
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
#' \item{A_hat}{Estimated VAR parameter via GLS}
#' \item{type}{Type of the VAR model, e.g. 'const'}
#' \item{SBcharacter}{Structural break (date; if provided in function arguments)}
#' \item{restrictions}{Number of specified restrictions}
#' \item{restriction_matrix}{Specified restriction matrix}
#' \item{y}{Data matrix}
#' \item{p}{Number of lags}
#' \item{K}{Dimension of the VAR}
#'
#' @references Rigobon, R., 2003. Identification through Heteroskedasticity. The Review of Economics and Statistics, 85, 777-792.\cr
#'  Herwartz, H. & Ploedt, M., 2016. Simulation Evidence on Theory-based and Statistical Identification under Volatility Breaks Oxford Bulletin of Economics and Statistics, 78, 94-112.
#'
#' @seealso For alternative identification approaches see \code{\link{id.st}}, \code{\link{id.garch}}, \code{\link{id.cvm}}, \code{\link{id.dc}} or \code{\link{id.ngml}}
#'
#' @examples
#' #' # data contains quartlery observations from 1965Q1 to 2008Q2
#' # assumed structural break in 1979Q3
#' # x = output gap
#' # pi = inflation
#' # i = interest rates
#' set.seed(23211)
#' v1 <- vars::VAR(USA, lag.max = 10, ic = "AIC" )
#' x1 <- id.cv(v1, SB = 59)
#' summary(x1)
#'
#' # switching columns according to sign patter
#' x1$B <- x1$B[,c(3,2,1)]
#' x1$B[,3] <- x1$B[,3]*(-1)
#'
#' # Impulse response analysis
#' i1 <- irf(x1, n.ahead = 30)
#' plot(i1, scales = 'free_y')
#'
#' # Restrictions
#' # Assuming that the interest rate doesn't influence the output gap on impact
#' restMat <- matrix(rep(NA, 9), ncol = 3)
#' restMat[1,3] <- 0
#' x2 <- id.cv(v1, SB = 59, restriction_matrix = restMat)
#' summary(x2)
#'
#' # In alternative Form
#' restMat <- diag(rep(1,9))
#' restMat[7,7]= 0
#' x2 <- id.cv(v1, SB = 59, restriction_matrix = restMat)
#' summary(x2)
#'
#' #Structural brake via Dates
#' # given that time series vector with dates is available
#' dateVector = seq(as.Date("1965/1/1"), as.Date("2008/7/1"), "quarter")
#' x3 <- id.cv(v1, SB = "1979-07-01", format = "%Y-%m-%d", dateVector = dateVector)
#' summary(x3)
#'
#' # or pass sequence arguments directly
#' x4 <- id.cv(v1, SB = "1979-07-01", format = "%Y-%m-%d", start = "1965-01-01", end = "2008-07-01",
#' frequency = "quarter")
#' summary(x4)
#'
#' # or provide ts date format (For quarterly, monthly, weekly and daily frequencies only)
#' x5 <- id.cv(v1, SB = c(1979, 3))
#' summary(x5)
#'
#' @importFrom steadyICA steadyICA
#' @export


#--------------------------------------------#
## Identification via changes in volatility ##
#--------------------------------------------#

# x  : object of class VAR
# SB : structural break


id.cv <- function(x, SB, start = NULL, end = NULL, frequency = NULL,
                        format = NULL, dateVector = NULL, max.iter = 50, crit = 0.001,
                  restriction_matrix = NULL){



u <- Tob <- p <- k <- residY <- coef_x <- yOut <- type <- y <-  NULL
get_var_objects(x)

rmOut = restriction_matrix
restriction_matrix <- get_restriction_matrix(restriction_matrix, k)

restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])

  if (is.numeric(SB)) {
    SBcharacter <- NULL
  }

if (!is.numeric(SB)) {
    SBcharacter <- SB
    SB <- getStructuralBreak(SB = SB, start = start, end = end,
                             frequency = frequency, format = format, dateVector = dateVector, Tob = Tob, p = p)
} else if(length(SB) != 1 & inherits(yOut, "ts") & length(SB) < 4){
        SBts = SB
        SB = dim(window(yOut, end = SB))[1]
      if(frequency(yOut) == 4){
        SBcharacter = paste(SBts[1], " Q", SBts[2], sep = "")
     }else if(frequency(yOut) == 12){
        SBcharacter = paste(SBts[1], " M", SBts[2], sep = "")
     }else if(frequency(yOut) == 52){
        SBcharacter = paste(SBts[1], " W", SBts[2], sep = "")
     }else if(frequency(yOut) == 365.25){
        SBcharacter = paste(SBts[1], "-", SBts[2], "-", SBts[3], sep = "")
     }else{
        SBcharacter = NULL
  }
}

if (length(SB) > 4 & length(SB) != Tob) {
  stop('Wrong length of SB')
}

if (length(SB) == Tob & length(SB) > 3) {
  SB_out <- SB
  TB <- Tob - sum(SB) + 1
  resid1 <- u[SB == 0,]
  resid2 <- u[SB,]
} else {
  SB_out <- SB
  TB <- SB - p
  SB <- rep(0, Tob)
  SB[TB:Tob] <- 1
  resid1 <- u[1:TB - 1, ]
  resid2 <- u[TB:Tob, ]
}

Sigma_hat1 <- (crossprod(resid1)) / (TB - 1)
Sigma_hat2 <- (crossprod(resid2)) / (Tob - TB + 1)

yl <- t(YLagCr(t(y), p))
yret <- y
y <- y[, -c(1:p)]

  if(type == 'const'){
    Z_t <- rbind(rep(1, ncol(yl)), yl)
  }else if(type == 'trend'){
    Z_t <- rbind(seq(p + 1, ncol(yret)), yl)
  }else if(type == 'both'){
    Z_t <- rbind(rep(1, ncol(yl)), seq(p + 1, ncol(yret)), yl)
  }else{
    Z_t <- yl
  }

  # if (!is.null(restriction_matrix)) {
  #   restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
  # } else {
  #   restrictions <- 0
  #   restriction_matrix <- matrix(NA, k, k)
  # }

  Regime1 <- which(SB == 0) - 1
  Regime2 <- which(SB == 1) - 1

  best_estimation <- IdentifyVolatility(crit = crit, u = u, TB = TB, p = p, k = k, type = type,
                                       Regime1 = Regime1, Regime2 = Regime2,
                                       RestrictionMatrix = restriction_matrix, restrictions = restrictions,
                                       Tob = Tob, SigmaHat1 = Sigma_hat1, SigmaHat2 = Sigma_hat2, Zt = Z_t, y = y,
                                       maxIter = max.iter)
  # Adding normalizing constant
  best_estimation$Lik <- -(Tob * (k / 2) * log(2 * pi) + best_estimation$Lik)

  if(restrictions > 0 ){


    unrestricted_estimation <- IdentifyVolatility(crit = crit, u = u, TB = TB, p = p, k = k, type = type,
                                                  Regime1 = Regime1, Regime2 = Regime2,
                                                  RestrictionMatrix = matrix(NA, k, k), restrictions = 0,
                                                  Tob = Tob, SigmaHat1 = Sigma_hat1, SigmaHat2 = Sigma_hat2, Zt = Z_t, y = y,
                                                  maxIter = max.iter)
    # Adding normalizing constant
    unrestricted_estimation$Lik <- -(Tob*(k/2)*log(2*pi) + unrestricted_estimation$Lik)

    lRatioTestStatistic = 2 * (unrestricted_estimation$Lik - best_estimation$Lik)
    restrictions <- length(restriction_matrix[!is.na(restriction_matrix)])
    pValue = round(1 - pchisq(lRatioTestStatistic, restrictions), 4)
    lRatioTest <- data.frame(testStatistic = lRatioTestStatistic, p.value = pValue)
    rownames(lRatioTest) <- ""
    colnames(lRatioTest) <- c("Test statistic", "p-value")
  }else{
    lRatioTest <- NULL
  }


  #result$restriction_matrix = rmOut

  if(is.null(best_estimation$A_hat)){
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
    }
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

  wald <- wald.test(best_estimation$Lambda, best_estimation$Fish, restrictions)
  rownames(best_estimation$B) <- colnames(u)
  rownames(best_estimation$Lambda) <- colnames(u)
  rownames(best_estimation$Lambda_SE) <- colnames(u)
  rownames(best_estimation$B_SE) <- colnames(u)

  result <- list(
    Lambda = best_estimation$Lambda,    # estimated Lambda matrix (unconditional heteroscedasticity)
    Lambda_SE = best_estimation$Lambda_SE,  # standard errors of Lambda matrix
    B = best_estimation$B,              # estimated B matrix (unique decomposition of the covariance matrix)
    B_SE = best_estimation$B_SE,            # standard errors of B matrix
    n = Tob,                # number of observations
    Fish = best_estimation$Fish,            # observerd fisher information matrix
    Lik = best_estimation$Lik,             # function value of likelihood
    wald_statistic = wald,  # results of wald test
    iteration = best_estimation$iteration,     # number of gls estimations
    method = "Changes in Volatility",
    SB = SB_out,                # Structural Break in number format
    A_hat = best_estimation$A_hat,            # VAR parameter estimated with gls
    type = type,          # type of the VAR model e.g 'const'
    SBcharacter = SBcharacter,             # Structural Break in input character format
    restrictions = restrictions, # number of restrictions
    restriction_matrix = rmOut,
    y = yOut,                # Data
    p = unname(p),                # number of lags
    K = k,# number of time series
    lRatioTest = lRatioTest
  )
  class(result) <- "svars"
 return(result)
}
