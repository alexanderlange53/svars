#' @export

summary.svarIdent <- function(svarIdentObject, ...){

  cat(paste("\n", "Identification Results", "\n", sep = ""))
  underScore <- paste(rep("-", nchar("Identification Results")), collapse = "")
  cat(underScore, "\n")
  cat("\nMethod: " )
  cat(svarIdentObject$method)
  cat("\nSample size: ")
  cat(svarIdentObject$n)
  if(svarIdentObject$method ==  "Changes in Volatility"){
  cat("\nStructural Break: At Observation Number ")
 cat(svarIdentObject$SB)
  if(!is.null(svarIdentObject$SBcharacter)){
    cat(" during ")
    cat(svarIdentObject$SBcharacter)
  }
  }
  cat("\nNumber of GLS estimations : ")
  cat(svarIdentObject$iteration)
  cat("\nLikelihood: ")
  cat(svarIdentObject$Lik)
  cat("\nWald Test Statistics: ")
  cat(svarIdentObject$wald_statistic)
  cat("\n")
  cat("\nEstimated unconditional Heteroscedasticity Matrix (Lambda):\n")
  print(svarIdentObject$Lambda_hat)
  cat("\nStandard Errors of Lambda:\n")
  print(svarIdentObject$tLambda_SE)
  cat("\nEstimated B Matrix (unique decomposition of the covariance matrix): \n")
  print(svarIdentObject$B)
  cat("\nStandard Errors of B:\n")
  print(svarIdentObject$B_SE)
  #cat("\nObserverd fisher information matrix:\n")
  #print(svarIdentObject$Fish)

}
