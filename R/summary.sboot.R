#' @export

summary.sboot <- function(object, ...){

  sbootObject <- object
  cat(paste("\n", "Bootstrap Results", "\n", sep = ""))
  underScore <- paste(rep("-", nchar("Bootstrap Results")), collapse = "")
  cat(underScore, "\n")
  cat("\nMethod: " )
  cat(sbootObject$method)
  cat("\nBootstrap iterations: ")
  cat(sbootObject$nboot)
  if(sbootObject$method == "Wild bootstrap"){
    cat("\nUsage of Rademacher distribution: ")
    cat(sbootObject$rademacher)
  }else{
    cat("\nChosen block length: ")
    cat(sbootObject$b_length)
  }
  cat("\n")
  cat("\nPoint estimates: \n")
  print(sbootObject$point_estimate)
  cat("\nBootstrap means: \n")
  print(sbootObject$boot_mean)
  if(!is.null(sbootObject$SE)){
    cat("\nBootstrap standard errors: \n")
    print(sbootObject$SE)
  }
  cat("\nIdentified sign patterns: \n")
  if(is.null(sbootObject$signrest)){
    cat("\nShare of Bootstrap estimates with same unique sign pattern as Point estimates: ")
    cat((sbootObject$sign_complete/sbootObject$nboot)*100,"%")
    cat("\nUnique occurrence of single shocks according to sign pattern: \n")
    for(i in 1:length(sbootObject$sign_part)){
      cat("shock",i,":", (sbootObject$sign_part[i]/sbootObject$nboot)*100,"% \n")
    }
  }else{
    cat("\nSpecified sign pattern: \n")
    cat("\n")
    print(as.matrix(as.data.frame(sbootObject$signrest)))
    cat("\nUnique occurrence of single shocks according to sign pattern: \n")
    for(i in 1:length(sbootObject$sign_part)){
      cat(names(sbootObject$sign_part)[i],":", (sbootObject$sign_part[[i]]/sbootObject$nboot)*100,"% \n")
    }
    cat("\nJoint occurrence of specified shocks: ")
    cat((sbootObject$sign_complete/sbootObject$nboot)*100,"%")
  }
}
