#==========================================#
## Likelihood for smooth transition model ##
#==========================================#

likelihood_st <- function(parameter, u_t, G, k, Tob){

  if(any(parameter[(k * k + 1): (k * k + k)] < 0)){
    return(return(1e25))
  }

  B <- matrix(parameter[1: (k * k)], k, k)
  Lambda <- diag(parameter[(k * k + 1): (k * k + k)])
  Sigma_1 <- tcrossprod(B)
  Sigma_2 <- B %*% tcrossprod(Lambda, B)

  lik <- function(xx, B, Lambda){
    Omega <- (1 - G[xx]) * Sigma_1 + G[xx] * Sigma_2
    log(det(Omega)) + u_t[xx,] %*% solve(Omega) %*% u_t[xx,]
  }

  ll <- sapply(1:length(G), lik)
  ll <- sum(ll) * 0.5

  L <- - (- Tob * k / 2 * log(2 * pi) - ll)

  if(!is.na(L)){
    return(L)
  }else{
    return(1e25)
  }
}
