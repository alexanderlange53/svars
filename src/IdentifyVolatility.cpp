#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
double LikelihoodCV(arma::vec& S, double& Tob, double& TB,  arma::mat& Sigma_hat1, int& k,
                    arma::mat& Sigma_hat2, arma::mat& RestrictionMatrix, int& restrictions){


  RestrictionMatrix.elem(find_nonfinite(RestrictionMatrix)) = S.elem(find_nonfinite(RestrictionMatrix));
  arma::mat W = RestrictionMatrix;

  arma::mat Psi = diagmat(S.subvec(((k * k) - restrictions), (k * k + (k - 1) - restrictions)));

  arma::mat MMM = W * arma::trans(W);
  arma::mat MMM2 = W * Psi * arma::trans(W);
  double MW = arma::det(MMM);
  double MW2 = arma::det(MMM2);


  if (any(vectorise(Psi) < 0.0) || MW < 0.01 ||  MW2 < 0.01) {
    return 1e25;
  }

  double L = -(((TB - 1) / 2) * (log(MW) + arma::sum(diagvec((Sigma_hat1 * arma::inv(MMM)))))) -
    (((Tob - TB + 1) / 2) * (log(MW2) + arma::sum(diagvec((Sigma_hat2 * arma::inv(MMM2))))));

  return -L;

}

// [[Rcpp::export]]
Rcpp::List IdentifyVolatilityNew(int& counter, arma::mat& Bhat, arma::mat& LambdaHat,
                    int& Exit, int& crit, int& MaxIter, arma::mat& Zt, double& TB,
                    arma::mat& y, int& p, int& k, arma::mat& RestrictionMatrix,
                    int& restrictions, arma::vec& S, double& Tob){

  arma::mat GLS21(k*k*p+k, TB-1);
  arma::mat GLS22(k*k*p+k, y.n_cols);
  GLS21.zeros();
  GLS22.zeros();
  arma::mat term1(k, Zt.n_cols);
  term1.zeros();
  arma::mat kMat(k,k);
  kMat.eye();
  Rcpp::List BhatList = Rcpp::List::create(Bhat);
  Rcpp::List LambdaHatList = Rcpp::List::create(LambdaHat);
  arma::mat BhatInd = BhatList[0];
  arma::mat LambdaInd = LambdaHatList[0];
  arma::mat Sig1 = arma::inv(BhatInd * arma::trans(BhatInd));
  arma::mat Sig2 = arma::inv(BhatInd * (LambdaInd * arma::trans(BhatInd)));
  arma::mat GLS11 = arma::kron((Zt.cols(0, TB-2) * arma::trans(Zt.cols(0, TB-2))), Sig1);
  arma::mat GLS12 = arma::kron(Zt.cols(TB-1, Zt.n_cols-1) * arma::trans(Zt.cols(TB-1, Zt.n_cols-1)), Sig2);
  arma::mat GLS1 = arma::inv(GLS11 + GLS12);

  for(int i = 0; i < TB-1; ++i){
    GLS21.col(i) = arma::kron(Zt.col(i), Sig1) * y.col(i);
  }
  for(int i = TB-1; i < Zt.n_cols; ++i){
    GLS22.col(i) = arma::kron(Zt.col(i), Sig2) * y.col(i);
  }


  arma::mat GLS21sums = arma::sum(GLS21, 1);
  arma::mat GLS22sums = arma::sum(GLS22, 1);
  arma::mat GLS2 = GLS21sums + GLS22sums;
  arma::mat GLSHat = GLS1 * GLS2;
  term1 = reshape(kron(arma::trans(Zt), kMat) * GLSHat, k, Zt.n_cols);
  arma::mat ugls = arma::trans(y) - arma::trans(term1);
  arma::mat resid1gls = ugls.rows(0, TB-2);
  arma::mat resid2gls = ugls.rows(TB-1, Zt.n_cols-1);
  arma::mat Sigma_hat1gls =  (arma::trans(resid1gls) * resid1gls) / (TB-1);
  arma::mat Sigma_hat2gls = (arma::trans(resid2gls) * resid2gls) / (Zt.n_cols-TB+1);



  Rcpp::Environment stats("package:stats");
  Rcpp::Function nlm = stats["nlm"];


  Rcpp::List MLEgls = nlm(Rcpp::_["f"] = Rcpp::InternalFunction(LikelihoodCV),
                            Rcpp::_["p"] = S,
                            //Rcpp::_["method"] = "Nelder-Mead",
                            Rcpp::_["hessian"] = "T",
                            Rcpp::_["iterlim"] = 150,
                            Rcpp::_["Tob"] = Tob,
                            Rcpp::_["TB"] = TB,
                            Rcpp::_["Sigma_hat1"] = Sigma_hat1gls,
                            Rcpp::_["k"] = k,
                            Rcpp::_["Sigma_hat2"] = Sigma_hat2gls,
                            Rcpp::_["RestrictionMatrix"] = RestrictionMatrix,
                            Rcpp::_["restrictions"] = restrictions);

  //  arma::vec MLEestimate = Rcpp::as<arma::vec>(MLEgls[0]);
  //   return bfgs(S, LikelihoodCV, );
  return MLEgls;
}




