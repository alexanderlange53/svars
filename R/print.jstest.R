#' @S3method print jstest

print.jstest <- function(object, ...){

  TestMatrix <-  matrix(c(round(object$test_statistic, 2),
                          round(object$p_value, 2)),
                        ncol = 1, nrow = 2)
  rownames(TestMatrix) <- c("Test statistic", "p-value")
  cat("\n")
  printCoefmat(TestMatrix)
}
