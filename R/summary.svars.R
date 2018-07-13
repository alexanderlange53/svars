#' @S3method summary svars

summary.svars <- function(object, ...){

  svarsObject <- object
  cat(paste("\n", "Identification Results", "\n", sep = ""))
  underScore <- paste(rep("-", nchar("Identification Results")), collapse = "")
  cat(underScore, "\n")
  cat("\nMethod: " )
  cat(svarsObject$method)
  cat("\nSample size: ")
  cat(svarsObject$n)
  if(!svarsObject$method %in% c("Distance covariances", "Cramer-von Mises distance")){
  cat("\nLikelihood: ")
  cat(svarsObject$Lik)
}
  if(svarsObject$method ==  "Changes in Volatility"){
  cat("\nStructural Break: At Observation Number ")
 cat(svarsObject$SB)
  if(!is.null(svarsObject$SBcharacter)){
    cat(" during ")
    cat(svarsObject$SBcharacter)
  }
 cat("\nNumber of GLS estimations: ")
 cat(svarsObject$iteration)
 cat("\nNumber of Restrictions: ")
 cat(svarsObject$restrictions)
 cat("\n")
 cat("\nEstimated unconditional Heteroscedasticity Matrix (Lambda):\n")
 print(svarsObject$Lambda)
 cat("\nStandard Errors of Lambda:\n")
 print(svarsObject$Lambda_SE)
 cat("\nEstimated B Matrix (unique decomposition of the covariance matrix): \n")
 print(svarsObject$B)
 cat("\nStandard Errors of B:\n")
 print(svarsObject$B_SE)
 cat("\nPairwise Wald Test:\n")
 printCoefmat(svarsObject$wald_statistic, has.Pvalue = T)
   if(svarsObject$restrictions > 0){
     cat("\nLikelihood Ratio Test Statistic: ")
     cat(svarsObject$lRatioTestStatistic)
     cat(", p-value:")
     cat(svarsObject$lRatioTestPValue)

   }
  }else if(svarsObject$method == "Smooth transition"){
    cat("\nEstimated location of transition: ")
    cat(svarsObject$est_c)
    cat("\nNumber of GLS estimations: ")
    cat(svarsObject$iteration)
    cat("\nNumber of Restrictions: ")
    cat(svarsObject$restrictions)
    cat("\nEstimated transition coefficient: ")
    cat(svarsObject$est_g)
    cat("\nNumber of all grid combinations: ")
    cat(svarsObject$comb)
    cat("\n")
    cat("\nEstimated Heteroscedasticity Matrix (Lambda):\n")
    print(svarsObject$Lambda)
    cat("\nStandard Errors of Lambda:\n")
    print(svarsObject$Lambda_SE)
    cat("\nEstimated B Matrix (unique decomposition of the covariance matrix): \n")
    print(svarsObject$B)
    cat("\nStandard Errors of B:\n")
    print(svarsObject$B_SE)
    cat("\nPairwise Wald Test:\n")
    printCoefmat(svarsObject$wald_statistic, has.Pvalue = T)
    if(svarsObject$restrictions > 0){
      cat("\nLikelihood Ratio Test Statistic: ")
      cat(svarsObject$lRatioTestStatistic)
      cat(", p-value:")
      cat(svarsObject$lRatioTestPValue)

    }
 }else if(svarsObject$method == "Non-Gaussian maximum likelihood"){
   cat("\nStage3: ")
   cat(svarsObject$stage3)
   cat("\nEstimated degrees of freedom:                    ")
   cat(svarsObject$df)
   cat("\nStandard errors of estimated degrees of freedom: ")
   cat(svarsObject$df_SE)
   cat("\n")
   cat("\nEstimated B Matrix (unique decomposition of the covariance matrix): \n")
   print(svarsObject$B)
   cat("\nestimated standardized B matrix:\n")
   print(svarsObject$B_stand)
   cat("\nStandard errors of standardized B matrix:\n")
   print(svarsObject$B_stand_SE)
   cat("\nEstimated scale of the standardized B: ")
   cat(svarsObject$sigma)
   cat("\nStandard errors of the scale:          ")
   cat(svarsObject$sigma_SE)
   if(svarsObject$restrictions > 0){
     cat("\nLikelihood Ratio Test Statistic: ")
     cat(svarsObject$lRatioTestStatistic)
     cat(", p-value:")
     cat(svarsObject$lRatioTestPValue)

   }

 }else if(svarsObject$method == "Distance covariances"){
   cat("\n")
   cat("\nEstimated B Matrix (unique decomposition of the covariance matrix): \n")
   print(svarsObject$B)

 }else if(svarsObject$method == "Cramer-von Mises distance"){
   cat("\n")
   cat("\nEstimated B Matrix (unique decomposition of the covariance matrix): \n")
   printCoefmat(svarsObject$B)
   cat("\nRotation Angles: ")
   cat(svarsObject$rotation_angles, "\n")
   cat("Cramer-von Mises test statistic: ")
   cat(svarsObject$test.stats)
  }


  #cat("\nObserverd fisher information matrix:\n")
  #print(svarsObject$Fish)

}
