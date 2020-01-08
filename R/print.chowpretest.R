#' @export

print.chowpretest <- function(x,...){
  cat("\nEmpirical Fluctuation Process: Multivariate Chow test\n\n")
  cat("From observation: ", x$from, "\n")
  cat("To observation: ", x$to, "\n")
  cat("Maximum break point test statistic:", max(x$teststat_bp, na.rm = TRUE), "at observation", which.max(x$teststat_bp), "\n")
  cat("Maximum sample split test statistic:", max(x$teststat_sp, na.rm = TRUE), "at observation", which.max(x$teststat_sp), "\n")
  cat("\n")


}
