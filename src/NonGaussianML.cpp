#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
// Creating matrix with off diagonal elements
arma::mat BoffDiag(arma::vec& beta, int k, arma::mat RestrictionMatrix){

  arma::mat Boff = arma::eye(k, k);

  Boff.elem(find_nonfinite(RestrictionMatrix)) = beta;

  return Boff;
}


// Likelihood for Non-Gaussian ML Model
// [[Rcpp::export]]
arma::vec LikelihoodNGMLStage2(const arma::vec& theta, const arma::mat& u, const arma::mat& il, int Tob,
                            int& k, arma::vec& rows, arma::mat RestrictionMatrix, int& restrictions){

  // 'Restricting' the main diagonal elements of B to ones
  RestrictionMatrix.diag().ones();

  // Extracting values from parameter vector
  arma::vec beta = theta.subvec(0, k * k - k - 1 - restrictions);
  arma::vec sigma = theta.subvec(k * k - k - restrictions, k * k - 1 - restrictions);
  arma::vec lambda = theta.subvec(k * k - restrictions, theta.n_elem - 1);

  return lambda;

}
