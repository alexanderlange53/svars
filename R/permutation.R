# computes all permutations of the columns of a matrix
# returns a list of matrices
permutation <- function(mat) {
  permutations <- permute_vector(1:ncol(mat))

  res_list <- list()
  for (i in 1:ncol(permutations)) {
    res_list[[i]] <- mat[, permutations[, i, drop = TRUE], drop = FALSE]
  }
  return(res_list)
}

# calculate all permutations of a vector x
# naiive recursive implementation.
# @param x a vector
# @return matrix; each column is one unqiue permutation of x
permute_vector <- function(x) {
  n <- length(x)
  num_permutation <- factorial(n)

  if (n == 1) { # base case
    return(as.matrix(x))
  } else {
    num_sub_permutation <- factorial(n - 1)
    res <- matrix(0.0, nrow = n, ncol = num_permutation)
    for (i in 1:n) {
      # swap first entry in x with i-th one
      y <- x
      tmp <- y[1]
      y[1] <- y[i]
      y[i] <- tmp

      # calculate all permutations of y[2:n]
      col_idx <- (i-1) * num_sub_permutation + seq(1:num_sub_permutation)
      res[1, col_idx] <- y[1]
      res[2:n, col_idx] <- permute_vector(y[2:n])
    }
    return(res)
  }
}
