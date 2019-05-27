#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

// Likelihood for Non-Gaussian ML Model (Structural parameter estimation)
// [[Rcpp::export]]
double LikelihoodNGMLStage2(const arma::vec& theta, const arma::mat& u, int Tob,
                            int& k, arma::mat& RestrictionMatrix, int& restrictions){

  // 'Restricting' the main diagonal elements of B to ones
  arma::mat RM = RestrictionMatrix;

  RM.diag().ones();

  arma::mat I = arma::eye(k, k);

  // Extracting values from parameter vector
  arma::vec beta = theta.subvec(0, k * k - k - 1 - restrictions);
  arma::vec sigma = theta.subvec(k * k - k - restrictions, k * k - 1 - restrictions);
  arma::vec lambda = theta.subvec(k * k - restrictions, theta.n_elem - 1);

  // Creating standardized B matrix
  arma::mat Boff = arma::eye(k, k);
  Boff.elem(find_nonfinite(RM)) = beta;

  if(arma::det(Boff) == 0.0 | any(sigma < 0.0) | any(lambda < 2.0)){
    return 1e25;
  }

  double logl = 0;

  arma::mat BoffI = Boff.i();
  arma::vec tdist(k);

  for (int i = 0; i < Tob; ++i) {
   arma::vec BoffIk = BoffI * u.row(i).t();

    for (int j = 0; j < k; j++) {
      tdist(j) = R::dt( (1/(sigma(j))) * sqrt(lambda(j) / (lambda(j) - 2)) * BoffIk(j), lambda(j), 0);
    }

    logl +=  arma::sum(log(tdist)) - log(arma::det(Boff)) - arma::sum(log(sigma % arma::sqrt((lambda - 2) / lambda)));
  }

 return logl * (-1);
}


// Likelihood for Non-Gaussian ML Model (AR parameter estimation)
// [[Rcpp::export]]
double LikelihoodNGMLStage3(arma::vec& A, arma::mat& Z_t, const arma::mat& Y, const arma::mat& B_stand_est, arma::vec& sigma_est,
                            arma::vec& d_freedom, int& k, int& Tob) {

  double logl = 0;
  arma::mat I = arma::eye(k, k);

  arma::mat term1(Y.n_rows, Y.n_cols, arma::fill::zeros);

  for (int i = 0; i < Z_t.n_cols; i++) {
    term1.col(i) = arma::kron(Z_t.col(i).t(), I) * A;
  }

  arma::mat ResidualsLS = Y.t() - term1.t();
  arma::vec tdist(k);
  arma::vec tx(k);

  for (int i = 0; i < ResidualsLS.n_rows; i++) {

    tx = (1 / sigma_est) % sqrt(d_freedom / (d_freedom - 2)) % ResidualsLS.row(i).t();

    for (int j = 0; j < k; j++) {
      tdist(j) = R::dt(tx(j), d_freedom(j), 0);
    }

    logl += arma::sum(log(tdist)) - log(arma::det(B_stand_est)) - arma::sum(log(sigma_est % sqrt((d_freedom - 2) / d_freedom)));
  }

  return logl * (-1);
}
