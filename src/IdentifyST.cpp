#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
double LikelihoodST(arma::vec& parameter, double& Tob, arma::mat& u, int& k,
                    arma::vec& G, arma::mat& RestrictionMatrix, int& restrictions){


  RestrictionMatrix.elem(find_nonfinite(RestrictionMatrix)) = parameter.elem(find_nonfinite(RestrictionMatrix));
  arma::mat B = RestrictionMatrix;

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

// [[Rcpp::export]]
Rcpp::List nlmST(const arma::vec& S, double& Tob, const arma::mat& u, int& k,
                     const arma::vec& G, const arma::mat& RestrictionMatrix, int& restrictions){

  Rcpp::Environment stats("package:stats");
  Rcpp::Function nlm = stats["nlm"];

  Rcpp::List MLEgls = nlm(Rcpp::_["f"] = Rcpp::InternalFunction(LikelihoodST),
                          Rcpp::_["p"] = S,
                          Rcpp::_["hessian"] = "T",
                          Rcpp::_["iterlim"] = 150,
                          Rcpp::_["Tob"] = Tob,
                          Rcpp::_["u"] = u,
                          Rcpp::_["k"] = k,
                          Rcpp::_["G"] = G,
                          Rcpp::_["RestrictionMatrix"] = RestrictionMatrix,
                          Rcpp::_["restrictions"] = restrictions);

  return MLEgls;
}
