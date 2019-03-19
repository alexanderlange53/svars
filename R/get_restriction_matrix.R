# internal function to convert the provided restricion fomat into the matrix used by the function

get_restriction_matrix = function(restriction_matrix, k){

  if(!is.null(restriction_matrix)){

    restriction_matrix = as.matrix(restriction_matrix)

    if(dim(restriction_matrix)[2] == 1){
      if(dim(restriction_matrix)[1] != k^2){
        stop(paste0("Different restriction matrix dimension than B. Please use either of the two valid formats containing kxk (", k,"x",k, ") dimensions."))
      }
      twoDims = matrix(restriction_matrix, ncol = k)
      twoDims[twoDims == 0] <- NA
      twoDims[twoDims == 1] <- 0
      restriction_matrix = twoDims

    }else{
      if(dim(restriction_matrix)[1] == k & dim(restriction_matrix)[2] == k){

        restriction_matrix = restriction_matrix

      }else{
        stop(paste0("Different restriction matrix dimension than B. Please use either of the two valid formats containing kxk (", k,"x",k, ") dimensions."))
      }
    }
  }else{
    restriction_matrix <- NULL
  }

  restriction_matrix
}
