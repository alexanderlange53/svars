#' @S3method summary jstest
#' @export

summary.jstest <- function(object, ...){

  cat(paste("\n", "Joint Significance Test Results", "\n", sep = ""))
  underScore <- paste(rep("-", nchar("Joint Significance Test Results")), collapse = "")
  cat(underScore)
  TestMatrix <-  matrix(c(round(object$test_statistic, 2),
                          round(object$p_value, 2)),
                        ncol = 1, nrow = 2)
  rownames(TestMatrix) <- c("Test statistic", "p-value")
  cat("\n")
  printCoefmat(TestMatrix)
}

