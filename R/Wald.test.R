# testing if all the the elements on the main daigonal of Lambda are different to each other
# Sigma: HESS => InvFish

Wald.test <- function(Lambda, Sigma){

  beta <- diag(Lambda)[1:2]
  sigma <- diag(Sigma[10:11,10:11])
  covS <- Sigma[10:11, 10:11][1,2]
  l1_l2 <- round(1 - pchisq((beta%*%c(1, -1))^2/(sum(sigma) - 2*covS), 1), 2)

  beta <- diag(Lambda)[c(1,3)]
  sigma <- diag(Sigma[c(10,12),c(10,12)])
  covS <- Sigma[c(10,12),c(10,12)][1,2]
  l1_l3 <- round(1 - pchisq((beta%*%c(1, -1))^2/(sum(sigma) - 2*covS), 1), 2)

  beta <- diag(Lambda)[c(2,3)]
  sigma <- diag(Sigma[c(11,12),c(11,12)])
  covS <- Sigma[c(11,12),c(11,12)][1,2]
  l2_l3 <- round(1 - pchisq((beta%*%c(1, -1))^2/(sum(sigma) - 2*covS), 1), 2)

  return(c(l1_l2, l1_l3, l2_l3))
}


