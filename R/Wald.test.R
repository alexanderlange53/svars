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

  P.Value <- lapply(1:(k*(k-1)/2), function(x){
     round(1 - pchisq((betas[, x]%*%c(1, -1))^2 / (sum(sigmas[,x]) - 2 * covS[x]), 1), 2)
    })

  Test.Stat <- lapply(1:(k*(k-1)/2), function(x){
    round((betas[, x]%*%c(1, -1))^2 / (sum(sigmas[,x]) - 2 * covS[x]),2)
  })

  combinations = matrix(combn(1:k,2), nrow = 2)

  H.Null = apply(combinations, 2, FUN = function(x){
    paste("lambda_", x[1], "=", "lambda_", x[2], sep = "")
    })


  waldTest = cbind(H.Null,Test.Stat, P.Value)

  return(waldTest)
}


