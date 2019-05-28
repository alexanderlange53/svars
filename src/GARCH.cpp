#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

// Likelihood for GARCH Model (univariate optimization) Sigma transpnieren!!
// [[Rcpp::export]]
double LikelihoodGARCHu(arma::vec& parameter, arma::vec& est, double& Sigma1, int Tob, int k){

  double gamma  =  parameter(0);
  double g      =  parameter(1);

  // Checking for input validity
  if (gamma > 0.001 & g >= 0.001 & gamma + g < 0.999) {

    // Likelihood function
    double  L = 0;
    arma::vec Sigma2(Tob);
    Sigma2(0) = Sigma1;

    for (int i = 1; i < Tob; i++) {
      Sigma2(i) =  (1 - g - gamma) + gamma * pow(est(i - 1), 2) + g * Sigma2(i - 1);
      L += 0.5 * (log(Sigma2(i - 1)) + pow(est(i - 1), 2) / Sigma2(i - 1));
    }

    return L;

  } else {
    return 1e25;
  }
}

// Likelihood for GARCH Model (multivariate optimization)
// [[Rcpp::export]]
double likelihoodGARCHm(arma::vec& parameter, arma::mat& SigmaE, int& Tob, int& k, arma::mat& u,
                        arma::mat& RestrictionMatrix, int& restrictions){

  arma::mat B(size(RestrictionMatrix), arma::fill::zeros);
  B.elem(find_nonfinite(RestrictionMatrix)) = parameter.subvec(0, (k * k - 1) - restrictions);

  double L = 0;

  for (int i = 0; i < Tob; i++) {
    L += (-0.5 * log(arma::det(B * arma::diagmat(SigmaE.row(i)) * B.t())) -
      0.5 * arma::as_scalar(u.row(i) * arma::inv(B * arma::diagmat(SigmaE.row(i)) * B.t()) *  u.row(i).t()));
  }

  return L * (-1);
}
