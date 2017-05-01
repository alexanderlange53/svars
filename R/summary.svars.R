#' @export

summary.svars <- function(svarsObject, ...){

  cat(paste("\n", "Identification Results", "\n", sep = ""))
  underScore <- paste(rep("-", nchar("Identification Results")), collapse = "")
  cat(underScore, "\n")
  cat("\nMethod: " )
  cat(svarsObject$method)
  cat("\nSample size: ")
  cat(svarsObject$n)
  if(svarsObject$method ==  "Changes in Volatility"){
  cat("\nStructural Break: At Observation Number ")
 cat(svarsObject$SB)
  if(!is.null(svarsObject$SBcharacter)){
    cat(" during ")
    cat(svarsObject$SBcharacter)
  }
  }
  cat("\nNumber of GLS estimations : ")
  cat(svarsObject$iteration)
  cat("\nNumber of Restrictions : ")
  cat(svarsObject$restrictions)
  cat("\nLikelihood: ")
  cat(svarsObject$Lik)
  cat("\nWald Test Statistics: ")
  cat(svarsObject$wald_statistic)
  cat("\n")
  cat("\nEstimated unconditional Heteroscedasticity Matrix (Lambda):\n")
  print(svarsObject$Lambda)
  cat("\nStandard Errors of Lambda:\n")
  print(svarsObject$Lambda_SE)
  cat("\nEstimated B Matrix (unique decomposition of the covariance matrix): \n")
  print(svarsObject$B)
  cat("\nStandard Errors of B:\n")
  print(svarsObject$B_SE)
  if(svarsObject$restrictions > 0){
  cat("\nLikelihood Ratio Test Statistic:")
  cat(svarsObject$lRatioTestStatistic)
  cat(", p-value:")
  cat(svarsObject$lRatioTestPValue)
  }
  #cat("\nObserverd fisher information matrix:\n")
  #print(svarsObject$Fish)

}
