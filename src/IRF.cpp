#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]

using namespace Rcpp;

// matrix exponentials
// [[Rcpp::export]]
arma::mat matexp(arma::mat X, int n)
{
  if (n == 0)
  {
    return arma::eye(X.n_cols, X.n_rows);
  } else if (n == 1)
  {
    return X;
  } else
  {
    return X * matexp(X, n-1);
  }
}


// IRFs
// [[Rcpp::export]]
List IRF(arma::mat &A_hat, arma::mat &B_hat, int &horizon)
{
  int K = A_hat.n_rows;
  int p = A_hat.n_cols / K;
  List Out(horizon);
  Out[0] = B_hat;
  if (p == 1)
  {
    for(int i = 1; i < horizon; i++)
    {
      Out[i] = matexp(A_hat, i) * B_hat;
    }
    return Out;
  } else
  {
    arma::mat Mm(K * p, K * p, arma::fill::zeros);
    Mm.submat(0, 0, (K - 1), (K * p - 1)) = A_hat;
    Mm.submat(K, 0, (K * p - 1), ((p - 1) * K - 1)) = arma::eye(K * (p - 1), K * (p - 1));
    arma::mat Mm1(K * p, K * p, arma::fill::eye);
    for (int i = 0; i < (horizon - 1); i++)
    {
      Mm1 = Mm1 * Mm;
      Out[i + 1] = Mm1.submat(0, 0, (K - 1), (K - 1)) * B_hat;
    }
    return Out;
  }

}
