#' @export

summary.jstest <- function(object, ...){

  cat(paste("\n", "Joint Significance Test Results", "\n", sep = ""))
  underScore <- paste(rep("-", nchar("Joint Significance Test Results")), collapse = "")
  cat(underScore)
  TestMatrix <-  matrix(c(object$test_statistic,
                          object$p_value),
                        ncol = 2, nrow = 1, byrow = T)
  colnames(TestMatrix) <- c("Test statistic", "p-value")
  rownames(TestMatrix) <- ""
  cat("\n")
  printCoefmat(TestMatrix, has.Pvalue = T)
}

