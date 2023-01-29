/*#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp17)]]


arma::mat sign_check(arma::cube Bs, int boot_length) {

    arma::mat sign_mat = arma::zeros(k, k);
    int sign_complete = 0;
    arma::vec sign_part = arma::zeros(k);

    for(int i = 0; i < boot_length; i++){

      pBs <- permutation(Bs.slice(i))
      sign.mat <-lapply(pBs, function(z){sapply(1:k, function(ii){all(z[,ii]/abs(z[,ii])  == x$B[,ii]/abs(x$B[,ii])) | all(z[,ii]/abs(z[,ii])  == x$B[,ii]/abs(x$B[,ii])*(-1))})})

      if(any(unlist(lapply(sign.mat, function(sign.mat)all(sign.mat == TRUE))))){
        sign.complete <- sign.complete + 1
      }

      for(j in 1:k){
        check <- rep(FALSE, k)
        for(l in 1:k){
          check[l] <- any(all(pBs[[1]][,l]/abs(pBs[[1]][,l]) == x$B[,j]/abs(x$B)[,j]) | all(pBs[[1]][,l]/abs(pBs[[1]][,l]) == x$B[,j]/abs(x$B)[,j]*(-1)))
        }
        if(sum(check) == 1){
          sign.part[[j]] <- sign.part[[j]] + 1
        }
      }
    }

}*/
