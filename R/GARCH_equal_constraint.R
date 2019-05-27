equal_constraint <- function(parameter, Sigma_e, Tob, k, u, restriction_matrix){

  B_inv <- diag(k)
  B_inv[col(B_inv) != row(B_inv)] <- parameter[1: (k^2 - k)]
  B_norm <-  solve(B_inv)

  diag_comp <- diag(parameter[(k^2 - k + 1): (k^2)])

  B <- solve(solve(diag_comp)%*%solve(B_norm))

  return(B[!is.na(restriction_matrix)])
}
