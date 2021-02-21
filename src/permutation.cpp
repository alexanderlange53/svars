#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
int fact_it_c(int M){
  // Computes factorial for M
  int out = 1;
  if (M > 1){
    for (int i = 1; i <= M; ++i){
      out *= i;
    }
  }
  return out;
}

// [[Rcpp::export]]
arma::mat permute_vector(arma::vec x) {
  // calculate all permutations of a vector x
  // naiive recursive implementation.
  // @param x a vector
  // @return matrix; each column is one unqiue permutation of x

  int n = x.n_elem;
  int num_permutation = fact_it_c(n);
  arma::mat res = arma::zeros(n, num_permutation);

  if (n == 1) { // base case
    return arma::conv_to<arma::mat>::from(x);
  } else {
    int num_sub_permutation = fact_it_c(n-1);
    for (int i = 0; i < n; i++) {
      // swap first entry in x with i-th one
      arma::vec y = x;
      double tmp = y(0);
      y(0) = y(i);
      y(i) = tmp;

      // calculate all permutations of y[2:n]
      arma::vec col_idx = (i) * num_sub_permutation + arma::regspace(0, num_sub_permutation-1);
      for(int j = 0; j < col_idx.n_elem; j++) {
        res(0, col_idx(j)) = y(0);
      };
      res(arma::span(1, n-1), arma::span(col_idx(0), col_idx(col_idx.n_elem-1))) = permute_vector(y.subvec(1, n-1));
    }
    return res;
  }
}

// [[Rcpp::export]]
Rcpp::List permutation(arma::mat mat) {
  // computes all permutations of the columns of a matrix
  // returns a list of matrices

  arma::mat permutations = permute_vector(arma::regspace(1, mat.n_cols));

  Rcpp::List res_list = Rcpp::List::create(arma::ones(mat.n_rows, mat.n_cols));

  for (int i = 0; i < permutations.n_cols; i++) {
    arma::vec sel_col = permutations.col(i);
    arma::mat temp_mat = arma::zeros(mat.n_rows, mat.n_cols);

    for (int j = 0; j < sel_col.n_elem; j++) {
      temp_mat.col(j) = mat.col(sel_col(j)-1);
    }
    if (i > 0) {
      res_list.push_back(temp_mat);
    } else {
      res_list = Rcpp::List::create(temp_mat);
    }

  }
  return res_list;
}
