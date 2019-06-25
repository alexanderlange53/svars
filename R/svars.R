#' svars: Data-driven identification of structural VAR models
#'
#' @docType package
#' @name svars
#' @author \itemize{
#' \item Alexander Lange \email{alexander.lange@uni-goettingen.de}
#' \item Bernhard Dalheimer \email{bernhard.dalheimer@uni-goettingen.de}
#' \item Helmut Herwartz \email{hherwartz@uni-goettingen.de}
#' \item Simone Maxand   \email{simone.maxand@helsinki.fi}
#' }
#' @description
#' This package implements data-driven identification methods for structural vector autoregressive (SVAR) models.
#' Based on an existing VAR model object, the structural impact matrix B may be obtained
#' via different forms of heteroskedasticity or independent components.\cr
#' @details
#' The main functions to retrieve structural impact matrices are:
#' \itemize{
#' \item \tabular{ll}{ \code{\link{id.cv}} \tab Identification via changes in volatility,}
#' \item \tabular{ll}{\code{\link{id.cvm}} \tab Independence-based identification of SVAR models
#' based on Cramer-von Mises distance,}
#' \item \tabular{ll}{\code{\link{id.dc}} \tab Independence-based identification of SVAR models
#' based on distance covariances,}
#' \item \tabular{ll}{\code{\link{id.garch}} \tab Identification through patterns of conditional heteroskedasticity,}
#' \item \tabular{ll}{\code{\link{id.ngml}} \tab Identification via Non-Gaussian maximum likelihood,}
#' \item \tabular{ll}{\code{\link{id.st}} \tab Identification by means of smooth transition in covariance.}
#' }
#'  All of these functions require an estimated var object. Currently the classes 'vars' and 'vec2var' from the \code{vars} package,
#'  'nlVar', which includes both VAR and VECM, from the \code{tsDyn} package as well as the list from \code{MTS} package are supported.
#'  Besides these core functions, additional tools to calculate confidence bands for impulse response functions using
#'  bootstrap techniques as well as the Chow-Test for structural changes are implemented. The \code{USA} dataset is used to showcase the
#'  functionalities in examples throughout the package.
#' @useDynLib svars
#' @importFrom Rcpp sourceCpp
NULL
