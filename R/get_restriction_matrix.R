# internal function to convert the provided restricion fomat into the matrix used by the function

get_restriction_matrix <- function(restriction_matrix, k){

  if(!is.null(restriction_matrix)){

    restriction_matrix = as.matrix(restriction_matrix)

    if(dim(restriction_matrix)[1] == k & dim(restriction_matrix)[2] == k){

      restriction_matrix = restriction_matrix

    }else if(dim(restriction_matrix)[1] == k^2 | dim(restriction_matrix)[2] == k^2){
      ones = matrix(rep(1,k*k),k,k)
      restriction_matrix = c(ones) %*% restriction_matrix
      restriction_matrix = matrix(restriction_matrix, ncol = k)
      restriction_matrix[restriction_matrix == 1] <- NA
    }else{
      stop(paste0("Different restriction matrix dimension than B. Please use either of the two valid formats containing either kxk (", k,"x",k, ") or k^2xk^2 (", k^2,"x",k^2, ") dimensions."))
    }
  }else{
    restriction_matrix <- matrix(NA, k, k)
  }

  return(restriction_matrix)
}
