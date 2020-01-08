#' @export

print.svars <- function(x, ...){

cat("\nEstimated B Matrix (unique decomposition of the covariance matrix): \n")
print(x$B)
}
