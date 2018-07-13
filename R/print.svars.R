#' @S3method print svars

print.svars <- function(object, ...){

cat("\nEstimated B Matrix (unique decomposition of the covariance matrix): \n")
print(object$B)
}
