#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp17)]]


  // [[Rcpp::export]]

  arma::mat YLagCr(arma::mat y, int p ){

    arma::mat YLag = arma::zeros(y.n_rows, y.n_cols * p);

     for(int i = 0; i < p; ++i){
     YLag.submat(i, i * y.n_cols, y.n_rows-1, i * y.n_cols + (y.n_cols-1)) = y.submat(0, 0, y.n_rows - i - 1, y.n_cols-1 );
     }
      arma::mat YLagOut = YLag.submat(p-1,0, YLag.n_rows -2, YLag.n_cols - 1);

     return YLagOut;
// return YLag;

  }
