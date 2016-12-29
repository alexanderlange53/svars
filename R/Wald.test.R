# testing if all the the elements on the main daigonal of Lambda are different to each other
# Sigma: HESS => Fish


wald.test <- function(Lambda, Sigma){

  k <- length(diag(Lambda))

  kList <- combn(1:k, 2)

  betas <- combn(diag(Lambda), 2)

  sigmas <- apply(kList, 2, function(x){
    diag(Sigma[c(k * k + x[1], k * k + x[2]), c(k * k + x[1], k * k + x[2])])
    })

  covS <- apply(kList, 2, function(x){
    Sigma[c(k * k + x[1], k * k + x[2]), c(k * k + x[1], k * k + x[2])][1,2]
    })

  testRes <- lapply(1:k, function(x){
    round(1 - pchisq((betas[, x]%*%c(1, -1))^2 / (sum(sigmas[,x]) - 2 * covS[x]), 1), 2)
    })

  return(unlist(testRes))
}


