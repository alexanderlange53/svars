#' @export

summary.chow <- function(object, ...){

  cat(paste("\n", "Chow Test Results", "\n", sep = ""))
  underScore <- paste(rep("-", nchar("Chow Test Results")), collapse = "")
  cat(underScore)
  cat("\nMax Number of Lags: ")
  cat(object$p)
  cat("\nStructural Break: At Observation Number ")
  cat(object$SB)
  if(!is.null(object$SBcharacter)){
    cat(" during ")
    cat(object$SBcharacter)
  }
  cat("\nNull Hypothesis: No breaks at particular time point\n")
  hLine <- paste(rep("=", nchar("Null Hypothesis: No breaks at particular time point")), collapse = "")
  cat(hLine, "\n")
  TestMatrix <-  matrix(c(object$lambda_bp,
                          object$testcrit_bp,
                          object$p.value_bp,
                          object$lambda_sp,
                          object$testcrit_sp,
                          object$p.value_sp),
                          ncol = 2, nrow = 3)
  colnames(TestMatrix) = c("Break-point Test:", "      Sample-split test:")
  rownames(TestMatrix) = c("Test statistic", "95% critical value", "p-value")
  printCoefmat(TestMatrix)

}

