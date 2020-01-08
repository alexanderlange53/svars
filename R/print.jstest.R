#' @export

print.jstest <- function(x, ...){

  TestMatrix <-  matrix(c(x$test_statistic,
                          x$p_value),
                        ncol = 2, nrow = 1, byrow = T)
  colnames(TestMatrix) <- c("Test statistic", "p-value")
  rownames(TestMatrix) <- ""
  cat("\n")
  printCoefmat(TestMatrix, has.Pvalue = T)
}
