#' @S3method summary chow

summary.chow <- function(object, ...){

  cat(paste("\n", "Chow Test Results", "\n", sep = ""))
  underScore <- paste(rep("-", nchar("Chow Test Results")), collapse = "")
  cat(underScore)
  cat("\nMaximum Number of Lags: ")
  cat(object$p)
  cat("\nStructural Break: At Observation Number ")
  cat(object$SB)
  if(!is.null(object$SBcharacter)){
    cat(" during ")
    cat(object$SBcharacter)
  }
  cat("\nNull Hypothesis: No breaks at particular time point\n")
  if(is.null(object$SBcharacter)){
  hLine <- paste(rep("=", nchar("Null Hypothesis: No breaks at particular time point")), collapse = "")
  }else{
  hLine <- paste(rep("=", sum(nchar(paste(c("Structural Break: At Observation Number ", object$SB, " during ",
                                      object$SBcharacter), sep = "")))), collapse = "")
  }
  cat(hLine, "\n")
  # TestMatrix <-  matrix(c(round(object$lambda_bp, 2),
  #                         round(object$testcrit_bp, 2),
  #                         round(object$p.value_bp, 2),
  #                         round(object$lambda_sp, 2),
  #                         round(object$testcrit_sp, 2),
  #                         round(object$p.value_sp, 2)),
  #                         ncol = 3, nrow = 2, byrow = T)

  TestMatrix <-  matrix(c(object$lambda_bp,
                          object$testcrit_bp,
                          object$p.value_bp,
                          object$lambda_sp,
                          object$testcrit_sp,
                          object$p.value_sp),
                        ncol = 3, nrow = 2, byrow = T)
   rownames(TestMatrix) <- c("Break-point Test:", "Sample-split test:")
   colnames(TestMatrix) <- c("Test statistic", "95% critical value", "p-value")
  cat("\n")
  printCoefmat(TestMatrix, has.Pvalue = T, signif.legend = T, signif.stars = T,
               digits = 4)

}

