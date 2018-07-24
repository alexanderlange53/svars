# Testing if all elements on the main daigonal of Lambda are different to each other
# Sigma: HESS => Fish


wald.test <- function(Lambda, Sigma, restrictions){

  k <- length(diag(Lambda))

  kList <- matrix(combn(1:k, 2), nrow = 2)

  betas <- matrix(combn(diag(Lambda), 2), nrow = 2)

  sigmas <- apply(kList, 2, function(x){
    diag(Sigma[c(k * k + x[1] - restrictions, k * k + x[2] - restrictions),
               c(k * k + x[1] - restrictions, k * k + x[2] - restrictions)])
    })

  covS <- apply(kList, 2, function(x){
    Sigma[c(k * k + x[1] - restrictions, k * k + x[2] - restrictions),
          c(k * k + x[1] - restrictions, k * k + x[2] - restrictions)][1,2]
    })

  p.value <- lapply(1:(k*(k-1)/2), function(x){
     round(1 - pchisq((betas[, x]%*%c(1, -1))^2 / (sum(sigmas[,x]) - 2 * covS[x]), 1), 2)
    })

  test.stat <- lapply(1:(k*(k-1)/2), function(x){
    round((betas[, x]%*%c(1, -1))^2 / (sum(sigmas[,x]) - 2 * covS[x]),2)
  })

  combinations <- matrix(combn(1:k,2), nrow = 2)

  H.null <- apply(combinations, 2, FUN = function(x){
    paste("lambda_", x[1], "=", "lambda_", x[2], sep = "")
    })


  #waldTest = cbind(H.null, test.stat, p.value)
  waldTest <- data.frame(test.stat = unlist(test.stat), p.value = unlist(p.value))
  colnames(waldTest) = c("Test statistic", "p-value")
  rownames(waldTest) <- H.null
  return(waldTest)
}


