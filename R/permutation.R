permutation = function(mat){
  expP <- unique(expand.grid(1:ncol(mat), 1:ncol(mat), 1:ncol(mat)))
  combinations = expP[apply(expP, 1, function(a) length(unique(abs(a)))==ncol(mat))==TRUE ,]

  return(lapply(1:nrow(combinations), function(x){
    mat[, unlist(combinations[x,])]
  }))
}
