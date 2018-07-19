#' @S3method print jstest

print.jstest <- function(object, ...){

  TestMatrix <-  matrix(c(object$test_statistic,
                          object$p_value),
                        ncol = 2, nrow = 1, byrow = T)
  colnames(TestMatrix) <- c("Test statistic", "p-value")
  rownames(TestMatrix) <- ""
  cat("\n")
  printCoefmat(TestMatrix, has.Pvalue = T)
}
