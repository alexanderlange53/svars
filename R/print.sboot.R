#' @S3method print sboot

print.sboot <- function(object, ...){
  sbootObject <- object
  cat("\nUnique occurrence of single shocks according to sign pattern: \n")
  for(i in 1:length(sbootObject$sign_part)){
    cat(names(sbootObject$sign_part)[i],":", (sbootObject$sign_part[[i]]/sbootObject$nboot)*100,"% \n")
  }
  cat("\nJoint occurrence of specified shocks: ")
  cat((sbootObject$sign_complete/sbootObject$nboot)*100,"%")
}
