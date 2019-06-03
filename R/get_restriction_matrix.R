# internal function to convert the provided restricion fomat into the matrix used by the function

get_restriction_matrix <- function(restriction_matrix, k){

  if(!is.null(restriction_matrix)){

    restriction_matrix = as.matrix(restriction_matrix)

    if(dim(restriction_matrix)[1] == k & dim(restriction_matrix)[2] == k){

      restriction_matrix = restriction_matrix

    }else if(dim(restriction_matrix)[1] == k^2 | dim(restriction_matrix)[2] == k^2){
      if(dim(restriction_matrix)[1] == k^2){
        restriction_matrix <- t(restriction_matrix)
      }

      twoDims <- matrix(NA, k, k)
      for(i in 1:dim(restriction_matrix)[1]){
        twoDims[restriction_matrix[i, ] == 1] <- 0
      }
      #
      # twoDims = matrix(restriction_matrix, ncol = k)
      # twoDims[twoDims == 0] <- NA
      # twoDims[twoDims == 1] <- 0
       restriction_matrix = twoDims
    }else{
      stop(paste0("Different restriction matrix dimension than B. Please use either of the two valid formats containing kxk (", k,"x",k, ") dimensions."))
    }
  }else{
    restriction_matrix <- matrix(NA, k, k)
  }

  return(restriction_matrix)
}
