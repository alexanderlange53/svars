#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
double LikelihoodCV(arma::vec& S, double& Tob, double& TB,  arma::mat& SigmaHat1, int& k,
                    arma::mat& SigmaHat2, arma::mat& RestrictionMatrix, int& restrictions){

  arma::mat W(size(RestrictionMatrix), arma::fill::zeros);
  W.elem(find_nonfinite(RestrictionMatrix)) = S.subvec(0, (k * k - 1) - restrictions);
  arma::mat Psi =  arma::diagmat(S.subvec((k * k - restrictions), (k * k + (k - 1) - restrictions)));

  arma::mat MMM = W * arma::trans(W);
  arma::mat MMM2 = W * Psi * arma::trans(W);
  double MW = arma::det(MMM);
  double MW2 = arma::det(MMM2);


  if (any(vectorise(Psi) < 0.0) || MW < 0.001 ||  MW2 < 0.001) {
    return 1e25;
  }

  double L = -(((TB - 1) / 2) * (log(MW) + arma::sum(diagvec((SigmaHat1 * arma::inv(MMM)))))) -
    (((Tob - TB + 1) / 2) * (log(MW2) + arma::sum(diagvec((SigmaHat2 * arma::inv(MMM2))))));

  return -L;

}


// optimization of likelihood via nlm and exporting
// [[Rcpp::export]]
Rcpp::List nlmCV(const arma::vec& S, double Tob, double TB, const arma::mat SigmaHat1, int k,
                    const arma::mat SigmaHat2, arma::mat RestrictionMatrix, int restrictions){

  Rcpp::Environment stats("package:stats");
  Rcpp::Function nlm = stats["nlm"];

  Rcpp::List MLE = nlm(Rcpp::_["f"] = Rcpp::InternalFunction(LikelihoodCV),
                          Rcpp::_["p"] = S,
                          Rcpp::_["hessian"] = "T",
                          Rcpp::_["iterlim"] = 150,
                          Rcpp::_["Tob"] = Tob,
                          Rcpp::_["TB"] = TB,
                          Rcpp::_["SigmaHat1"] = SigmaHat1,
                          Rcpp::_["k"] = k,
                          Rcpp::_["SigmaHat2"] = SigmaHat2,
                          Rcpp::_["RestrictionMatrix"] = RestrictionMatrix,
                          Rcpp::_["restrictions"] = restrictions);

  return MLE;
}



// [[Rcpp::export]]
Rcpp::List IdentifyVolatility(int crit, const arma::mat& u, double TB, arma::uvec& Regime1, arma::uvec& Regime2,
                              int p, int k, arma::mat RestrictionMatrix, std::string type,
                              int restrictions, double Tob, arma::mat SigmaHat1, arma::mat SigmaHat2,
                              arma::mat Zt, arma::mat y, int maxIter){

   arma::mat SigmaHat = u.t() * u / (Tob);


   arma::mat initB = arma::chol(SigmaHat, "lower");

   arma::mat initBvec = arma::ones(k * k);
   initBvec = initB.elem(find_nonfinite(RestrictionMatrix));

   arma::vec initLambda = arma::ones(k);
   arma::vec S = arma::join_vert(initBvec, initLambda);


   arma::vec likelihoods = {1e25}; // log likelihoods are directly stored in vector instead

   Rcpp::List hessian = Rcpp::List::create(arma::ones(k * k, k * k));
   Rcpp::List GLSE = Rcpp::List::create(arma::ones(p * k * k));

   Rcpp::List MLE = nlmCV(S, Tob, TB, SigmaHat1, k,  SigmaHat2, RestrictionMatrix, restrictions);
   arma::vec Lestimates = MLE[1];



  arma::mat BLoop = arma::zeros(k, k);
  BLoop.elem(find_nonfinite(RestrictionMatrix)) = Lestimates.subvec(0, k * k - 1 - restrictions);
  Rcpp::List BHat = Rcpp::List::create(BLoop);

  arma::mat LambdaFirst = arma::diagmat(Lestimates.subvec(k * k - restrictions, k * k + k - 1 - restrictions));
  Rcpp::List LambdaHat = Rcpp::List::create(LambdaFirst);

  int count = 0;
  double Exit = 1;

  while (Exit > crit && count < maxIter) {
     arma::mat BhatInd = BHat[count];
     arma::mat LambdaInd = LambdaHat[count];

     arma::mat Sig1 = arma::inv(BhatInd * arma::trans(BhatInd));
     arma::mat Sig2 = arma::inv(BhatInd * (LambdaInd * arma::trans(BhatInd)));

     arma::mat GLS11 = arma::kron((Zt.cols(Regime1) * arma::trans(Zt.cols(Regime1))), Sig1);
     arma::mat GLS12 = arma::kron(Zt.cols(Regime2) * arma::trans(Zt.cols(Regime2)), Sig2);

     arma::mat GLS1 = arma::inv(GLS11 + GLS12);

    // Differentiating between different cases of constant/trend/none
    arma::mat GLS21 = arma::zeros(k * k * p, Regime1.n_elem);
    arma::mat GLS22= arma::zeros(k * k * p, Regime2.n_elem);
    if (type == "const" || type == "trend") {
      GLS21 = arma::zeros(k * k * p + k, Regime1.n_elem);
      GLS22= arma::zeros(k * k * p + k, Regime2.n_elem);
    } else if (type == "both") {
      GLS21 = arma::zeros(k * k * p + 2 * k, Regime1.n_elem);
      GLS22= arma::zeros(k * k * p + 2 * k, Regime2.n_elem);
    }

    //return Rcpp::List::create(Rcpp::Named("GLS21") = any(2 == Regime1));

    int j1 = 0;
    int j2 = 0;
    for (int i = 0; i < Zt.n_cols; ++i) {
      if (any(i == Regime1)) {
        GLS21.col(j1) = arma::kron(Zt.col(i), Sig1) * y.col(i);
        j1 += 1;
      } else if (any(i == Regime2)) {
        GLS22.col(j2) = arma::kron(Zt.col(i), Sig2) * y.col(i);
        j2 += 1;
      }
     }

    arma::mat GLS21sums = arma::sum(GLS21, 1);
    arma::mat GLS22sums = arma::sum(GLS22, 1);
    arma::mat GLS2 = GLS21sums + GLS22sums;
    arma::mat GLSHat = GLS1 * GLS2;

    arma::mat term1(k, Zt.n_cols);
    term1.zeros();
    arma::mat kMat(k,k);
    kMat.eye();

    term1 = reshape(kron(arma::trans(Zt), kMat) * GLSHat, k, Zt.n_cols);
    arma::mat ugls = arma::trans(y) - arma::trans(term1);
    arma::mat resid1gls = ugls.rows(Regime1);
    arma::mat resid2gls = ugls.rows(Regime2);
    arma::mat Sigma_hat1gls =  (arma::trans(resid1gls) * resid1gls) / (TB - 1);
    arma::mat Sigma_hat2gls = (arma::trans(resid2gls) * resid2gls) / (Zt.n_cols - TB + 1);

    Rcpp::List MLEgls = nlmCV(S, Tob, TB, Sigma_hat1gls, k,  Sigma_hat2gls, RestrictionMatrix, restrictions);
    arma::mat GLSBLoop = arma::zeros(k, k);
    arma::vec GLSestimates = MLEgls[1];


    GLSBLoop.elem(find_nonfinite(RestrictionMatrix)) = GLSestimates.subvec(0, k * k - 1 - restrictions);
    arma::mat GLSLambdaLoop = arma::diagmat(GLSestimates.subvec(k * k - restrictions, k * k + k - 1 - restrictions));
    BHat.push_back(GLSBLoop);
    LambdaHat.push_back(GLSLambdaLoop);
    int sz = likelihoods.size();
    likelihoods.resize(sz + 1);
    likelihoods(sz) = MLEgls[0];
    hessian.push_back(MLEgls[3]);
    GLSE.push_back(GLSHat);
     count += 1;
     Exit = likelihoods(count - 1) - likelihoods(count);

  }

   arma::vec ll = likelihoods;

    int cc = ll.index_min();
    double llbest = ll.min();

    arma::mat BOpt = BHat[cc];
    arma::mat LambdaOpt = LambdaHat[cc];
    arma::mat GLSEOpt = GLSE[cc];
    GLSEOpt.reshape(k, GLSEOpt.size() / k);

   // Optaining standard errors
  arma::mat HESS = hessian[cc];
  HESS = HESS.i();

  for(int i = 0; i < HESS.n_rows; ++i){
    if (HESS(i, i) < 0.0) {
      HESS.col(i) = HESS.col(i) * (-1);
    }
  }

  arma::vec FishObs = arma::sqrt(HESS.diag());

  arma::mat BSE = arma::zeros(k, k);
  BSE.elem(find_nonfinite(RestrictionMatrix)) = FishObs.subvec(0, k * k - 1 - restrictions);
  arma::mat LambdaSE = arma::diagmat(FishObs.subvec(k * k - restrictions, k * k + k - 1 - restrictions));

  //Returning an R like list object with all results from optimization
  return Rcpp::List::create(Rcpp::Named("Lambda") = LambdaOpt,
                            Rcpp::Named("Lambda_SE") = LambdaSE,
                            Rcpp::Named("B") = BOpt,
                            Rcpp::Named("B_SE") = BSE,
                            Rcpp::Named("Fish") = HESS,
                            Rcpp::Named("Lik") = llbest,
                            Rcpp::Named("iteration") = count,
                            Rcpp::Named("A_hat") = GLSEOpt);

}



