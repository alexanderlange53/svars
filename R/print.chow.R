#' @S3method print chow

print.chow <- function(object, ...){

  TestMatrix <-  matrix(c(round(object$lambda_bp, 2),
                          round(object$testcrit_bp, 2),
                          round(object$p.value_bp, 2),
                          round(object$lambda_sp, 2),
                          round(object$testcrit_sp, 2),
                          round(object$p.value_sp, 2)),
                        ncol = 2, nrow = 3)
  colnames(TestMatrix) <- c("Break-point Test:", "      Sample-split test:")
  rownames(TestMatrix) <- c("Test statistic", "95% critical value", "p-value")
  cat("\n")
  printCoefmat(TestMatrix)

}
