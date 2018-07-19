#' @S3method print sboot

print.sboot <- function(x, ...){

  cat("\nUnique occurrence of single shocks according to sign pattern: \n")
  for(i in 1:length(x$sign_part)){
    cat(names(x$sign_part)[i],":", (x$sign_part[[i]]/x$nboot)*100,"% \n")
  }
  cat("\nJoint occurrence of specified shocks: ")
  cat((x$sign_complete/x$nboot)*100,"%")
}
