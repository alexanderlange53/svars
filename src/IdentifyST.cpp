#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

// Likelihood for smooth transition model
// [[Rcpp::export]]
double LikelihoodST(arma::vec& parameter, double& Tob, arma::mat& u, int& k,
                    arma::vec& G, arma::mat& RestrictionMatrix, int& restrictions){

  arma::mat B(size(RestrictionMatrix), arma::fill::zeros);
  B.elem(find_nonfinite(RestrictionMatrix)) = parameter.subvec(0, (k * k - 1) - restrictions);
  arma::mat Lambda =  arma::diagmat(parameter.subvec((k * k - restrictions), (k * k + (k - 1) - restrictions)));

 if (any(Lambda.diag() < 0.0)) {
    return 1e25;
  }

  arma::mat Sigma_1 = B * B.t();
  arma::mat Sigma_2 = B * Lambda * B.t();


  arma::vec ll(G.n_elem);
  ll.fill(0.0);

  for (int i = 0; i < G.n_elem; ++i) {
    arma::mat Omega = (1 - G(i)) * Sigma_1 + G(i) * Sigma_2;
    ll(i) = log(arma::det(Omega)) +  arma::as_scalar(u.row(i) * arma::inv(Omega) * u.row(i).t());
  }

  return (-1) * (- Tob * k / 2 * log(2 * M_PI) - arma::sum(ll) * 0.5);

}

// optimization of likelihood via nlm and exporting a list
// [[Rcpp::export]]
Rcpp::List nlmST(const arma::vec& S, double& Tob, const arma::mat& u, int& k,
                     const arma::vec& G, const arma::mat& RestrictionMatrix, int& restrictions){

  Rcpp::Environment stats("package:stats");
  Rcpp::Function nlm = stats["nlm"];

  Rcpp::List MLE = nlm(Rcpp::_["f"] = Rcpp::InternalFunction(LikelihoodST),
                          Rcpp::_["p"] = S,
                          Rcpp::_["hessian"] = "T",
                          Rcpp::_["iterlim"] = 150,
                          Rcpp::_["Tob"] = Tob,
                          Rcpp::_["u"] = u,
                          Rcpp::_["k"] = k,
                          Rcpp::_["G"] = G,
                          Rcpp::_["RestrictionMatrix"] = RestrictionMatrix,
                          Rcpp::_["restrictions"] = restrictions);

  return MLE;
}

// Creating Regressor matrix (used in multiple )
/*
arma::mat = crZ(const arma::mat& y, int& p){
  arma::mat Z = arma::zeros(y.nrows - p, y.cols * p);

  for (int i = 0; i < p; ++i) {
    Z.col(i * Z.ncol, (i + 1) * Z.ncol - 1) = y(0, span(y.nrows - p), span());
  }
}
*/

// Multivariate GLS estimator for smooth transition model
// [[Rcpp::export]]
arma::vec mGLSst(const arma::vec& transition, const arma::mat& B, const arma::mat& Lambda,
                 const arma::mat& Z_t, int& k, const arma::mat& Y){

  arma::mat W = arma::zeros(k * transition.n_elem, k * transition.n_elem);
  arma::mat I = arma::eye(k, k);

  for (int i = 0; i < transition.n_elem; ++i) {
    W(arma::span(i*k, (i + 1) * k - 1), arma::span(i*k, (i + 1) * k - 1)) = arma::inv((1 - transition(i)) * B * B.t() + transition(i) * B * Lambda * B.t());
  }

  arma::vec b_gls = arma::inv(arma::kron(Z_t, I) * W * arma::kron(Z_t.t(), I)) * arma::kron(Z_t, I) * W * arma::vectorise(Y);

  return b_gls;
}

