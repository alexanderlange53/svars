#' @export

print.chow <- function(x, ...){

  TestMatrix <-  matrix(c(round(x$lambda_bp, 2),
                          round(x$testcrit_bp, 2),
                          round(x$p.value_bp, 2),
                          round(x$lambda_sp, 2),
                          round(x$testcrit_sp, 2),
                          round(x$p.value_sp, 2)),
                        ncol = 2, nrow = 3)
  colnames(TestMatrix) <- c("Break-point Test:", "      Sample-split test:")
  rownames(TestMatrix) <- c("Test statistic", "95% critical value", "p-value")
  cat("\n")
  printCoefmat(TestMatrix)

}
