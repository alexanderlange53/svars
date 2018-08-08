#' @S3method print svarirf

print.svarirf <- function(x, ...){

  cat("\nImpulse response coefficients\n")
  print(x$irf)
}
