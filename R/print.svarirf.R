#' @export

print.svarirf <- function(x, ...){

  cat("\nImpulse response coefficients\n")
  print(x$irf)
}
