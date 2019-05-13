#==========================================#
## Likelihood for smooth transition model ##
#==========================================#

likelihood_st <- function(parameter, u, G, k, Tob, restriction_matrix, restrictions){

  if(!is.null(restriction_matrix)){
    if(!is.matrix(restriction_matrix)){
      stop("Please provide a valid input matrix")
    }
    naElements <- is.na(restriction_matrix)
    toFillMatrix <- restriction_matrix
    toFillMatrix[naElements] <- parameter[1:sum(naElements)]
    B <- toFillMatrix
  }else{
    B <- matrix(parameter[1: (k * k)], k, k)
    restrictions <- 0
  }

  Lambda <-  diag(parameter[((k*k+1) - restrictions):((k*k+k)-restrictions)])

  if(any(diag(Lambda) < 0)){
    return(1e25)
  }


  Sigma_1 <- tcrossprod(B)
  Sigma_2 <- B %*% tcrossprod(Lambda, B)

  lik <- function(xx, B, Lambda){
    Omega <- (1 - G[xx]) * Sigma_1 + G[xx] * Sigma_2
    log(det(Omega)) + u[xx,] %*% solve(Omega) %*% u[xx,]
  }

  ll <- sapply(1:length(G), lik)
  L <- ll <- sum(ll) * 0.5

  L <- - (- Tob * k / 2 * log(2 * pi) - ll)

  if(!is.na(L)){
    return(L)
  }else{
    return(1e25)
  }
}
