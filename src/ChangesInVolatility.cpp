#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
double LikelihoodCV(arma::vec& S, double& Tob, double& TB,  arma::mat& SigmaHat1, int& k,
                    arma::mat& SigmaHat2, arma::mat& RestrictionMatrix, int& restrictions, arma::mat& RestrictionMatrixLambda, int& restrictionsLambda){

  arma::mat W(size(RestrictionMatrix), arma::fill::zeros);
  W.elem(find_nonfinite(RestrictionMatrix)) = S.subvec(0, (k * k - 1) - restrictions);

  arma::mat Psi1 = arma::ones(k);
  Psi1.elem(find_nonfinite(RestrictionMatrixLambda)) =  S.subvec((k * k - restrictions), (k * k + (k - 1) - restrictions - restrictionsLambda));
  Psi1.elem(find_finite(RestrictionMatrixLambda)) =  RestrictionMatrixLambda.elem(find_finite(RestrictionMatrixLambda));
  arma::mat Psi =  arma::diagmat(Psi1);

  arma::mat MMM = W * arma::trans(W);
  arma::mat MMM2 = W * Psi * arma::trans(W);
  double MW = arma::det(MMM);
  double MW2 = arma::det(MMM2);


  if (any(vectorise(Psi) < 0.0)) {
    return 1e25;
  }

  double L = -(((TB - 1) / 2) * (log(MW) + arma::sum(diagvec((SigmaHat1 * arma::inv(MMM)))))) -
    (((Tob - TB + 1) / 2) * (log(MW2) + arma::sum(diagvec((SigmaHat2 * arma::inv(MMM2))))));

  return -L;

}

// [[Rcpp::export]]
double LikelihoodCV3regimes(arma::vec& S, int& TB1, int& TB2, int& TB3, arma::mat& SigmaHat1, int& k,
                            arma::mat& SigmaHat2, arma::mat& SigmaHat3, arma::mat& RestrictionMatrix, int& restrictions, arma::mat& RestrictionMatrixLambda1,
                            arma::mat& RestrictionMatrixLambda2, int& restrictionsLambda){

  arma::mat W(size(RestrictionMatrix), arma::fill::zeros);
  W.elem(find_nonfinite(RestrictionMatrix)) = S.subvec(0, (k * k - 1) - restrictions);

  arma::mat Psi1 = arma::ones(k);

  arma::vec r1 = RestrictionMatrixLambda1.elem(find_finite(RestrictionMatrixLambda1));
  //arma::vec r2 = RestrictionMatrixLambda2.elem(find_finite(RestrictionMatrixLambda2));

  Psi1.elem(find_nonfinite(RestrictionMatrixLambda1)) =  S.subvec((k * k - restrictions), (k * k + (k - 1) - restrictions - r1.n_elem));
  Psi1.elem(find_finite(RestrictionMatrixLambda1)) =  RestrictionMatrixLambda1.elem(find_finite(RestrictionMatrixLambda1));
  arma::mat Psi =  arma::diagmat(Psi1);


  arma::mat Psi21 = arma::ones(k);
  Psi21.elem(find_nonfinite(RestrictionMatrixLambda2)) =  S.subvec((k * k + k - restrictions - r1.n_elem), (k * k + k + (k - 1) - restrictions - restrictionsLambda));
  Psi21.elem(find_finite(RestrictionMatrixLambda2)) =  RestrictionMatrixLambda2.elem(find_finite(RestrictionMatrixLambda2));
  arma::mat Psi2 =  arma::diagmat(Psi21);

  arma::mat MMM = W * arma::trans(W);
  arma::mat MMM2 = W * Psi * arma::trans(W);
  arma::mat MMM3 = W * Psi2 * arma::trans(W);
  double MW = arma::det(MMM);
  double MW2 = arma::det(MMM2);
  double MW3 = arma::det(MMM3);


  if (any(vectorise(Psi) < 0.0) | any(vectorise(Psi2) < 0.0)) {
    return 1e25;
  }

  double L = -((TB1 / 2) * (log(MW) + arma::sum(diagvec((SigmaHat1 * arma::inv(MMM)))))) -
    ((TB2 / 2) * (log(MW2) + arma::sum(diagvec((SigmaHat2 * arma::inv(MMM2)))))) -
    ((TB3 / 2) * (log(MW3) + arma::sum(diagvec((SigmaHat3 * arma::inv(MMM3))))));

  return -L;

}

// optimization of likelihood via nlm and exporting
// [[Rcpp::export]]
Rcpp::List nlmCV(const arma::vec& S, double Tob, double TB, const arma::mat SigmaHat1, int k,
                    const arma::mat SigmaHat2, arma::mat RestrictionMatrix, int restrictions, arma::vec RestrictionMatrixLambda, int restrictionsLambda){

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
                          Rcpp::_["restrictions"] = restrictions,
                          Rcpp::_["RestrictionMatrixLambda"] = RestrictionMatrixLambda,
                          Rcpp::_["restrictionsLambda"] = restrictionsLambda);

  return MLE;
}

// optimization of likelihood via nlm and exporting
// [[Rcpp::export]]
Rcpp::List nlmCV3(const arma::vec& S, double TB1, double TB2, double TB3, const arma::mat SigmaHat1, int k,
                 const arma::mat SigmaHat2, const arma::mat SigmaHat3, arma::mat RestrictionMatrix, int restrictions,
                 arma::mat& RestrictionMatrixLambda1, arma::mat& RestrictionMatrixLambda2, int restrictionsLambda){

  Rcpp::Environment stats("package:stats");
  Rcpp::Function nlm = stats["nlm"];

  Rcpp::List MLE = nlm(Rcpp::_["f"] = Rcpp::InternalFunction(LikelihoodCV3regimes),
                       Rcpp::_["p"] = S,
                       Rcpp::_["hessian"] = "T",
                       Rcpp::_["iterlim"] = 150,
                       Rcpp::_["TB1"] = TB1,
                       Rcpp::_["TB2"] = TB2,
                       Rcpp::_["TB3"] = TB3,
                       Rcpp::_["SigmaHat1"] = SigmaHat1,
                       Rcpp::_["k"] = k,
                       Rcpp::_["SigmaHat2"] = SigmaHat2,
                       Rcpp::_["SigmaHat3"] = SigmaHat3,
                       Rcpp::_["RestrictionMatrix"] = RestrictionMatrix,
                       Rcpp::_["restrictions"] = restrictions,
                       Rcpp::_["RestrictionMatrixLambda1"] = RestrictionMatrixLambda1,
                       Rcpp::_["RestrictionMatrixLambda2"] = RestrictionMatrixLambda2,
                       Rcpp::_["restrictionsLambda"] = restrictionsLambda);

  return MLE;
}


// [[Rcpp::export]]
Rcpp::List IdentifyVolatility(int crit, const arma::mat& u, double TB, arma::uvec& Regime1, arma::uvec& Regime2,
                              int p, int k, arma::mat RestrictionMatrix, std::string type, arma::vec RestrictionMatrixLambda, int restrictionsLambda,
                              int restrictions, double Tob, arma::mat SigmaHat1, arma::mat SigmaHat2,
                              arma::mat Zt, arma::mat y, int maxIter){

   arma::mat SigmaHat = u.t() * u / (Tob);


   arma::mat initB = arma::chol(SigmaHat, "lower");

   arma::mat initBvec = arma::ones(k * k);
   initBvec = initB.elem(find_nonfinite(RestrictionMatrix));

   arma::vec initLambda = arma::ones(k);
   initLambda = initLambda.elem(find_nonfinite(RestrictionMatrixLambda));

   arma::vec S = arma::join_vert(initBvec, initLambda);


   arma::vec likelihoods = {1e25}; // log likelihoods are directly stored in vector instead

   Rcpp::List hessian = Rcpp::List::create(arma::ones(k * k, k * k));
   Rcpp::List GLSE = Rcpp::List::create(arma::ones(p * k * k));

   Rcpp::List MLE = nlmCV(S, Tob, TB, SigmaHat1, k,  SigmaHat2, RestrictionMatrix, restrictions, RestrictionMatrixLambda, restrictionsLambda);
   arma::vec Lestimates = MLE[1];

  arma::mat BLoop = arma::zeros(k, k);
  BLoop.elem(find_nonfinite(RestrictionMatrix)) = Lestimates.subvec(0, k * k - 1 - restrictions);
  Rcpp::List BHat = Rcpp::List::create(BLoop);

  arma::mat LambdaFirstInit = arma::ones(k);
  LambdaFirstInit.elem(find_nonfinite(RestrictionMatrixLambda)) = Lestimates.subvec(k * k - restrictions, k * k + k - 1 - restrictions - restrictionsLambda);
  LambdaFirstInit.elem(find_finite(RestrictionMatrixLambda)) = RestrictionMatrixLambda.elem(find_finite(RestrictionMatrixLambda));
  arma::mat LambdaFirst = arma::diagmat(LambdaFirstInit);
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

    int j1 = 0;
    int j2 = 0;
    for (auto i = 0u; i < Zt.n_cols; ++i) {
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

    Rcpp::List MLEgls = nlmCV(S, Tob, TB, Sigma_hat1gls, k,  Sigma_hat2gls, RestrictionMatrix, restrictions, RestrictionMatrixLambda, restrictionsLambda);
    arma::mat GLSBLoop = arma::zeros(k, k);
    arma::vec GLSestimates = MLEgls[1];

    GLSBLoop.elem(find_nonfinite(RestrictionMatrix)) = GLSestimates.subvec(0, k * k - 1 - restrictions);
    BHat.push_back(GLSBLoop);

    arma::mat GLSLambdaLoopInit = arma::ones(k);
    GLSLambdaLoopInit.elem(find_nonfinite(RestrictionMatrixLambda)) = GLSestimates.subvec(k * k - restrictions, k * k + k - 1 - restrictions - restrictionsLambda);
    GLSLambdaLoopInit.elem(find_finite(RestrictionMatrixLambda)) = RestrictionMatrixLambda.elem(find_finite(RestrictionMatrixLambda));


    arma::mat GLSLambdaLoop = arma::diagmat(GLSLambdaLoopInit);
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

  for(auto i = 0u; i < HESS.n_rows; ++i){
    if (HESS(i, i) < 0.0) {
      HESS.col(i) = HESS.col(i) * (-1);
    }
  }

  arma::vec FishObs = arma::sqrt(HESS.diag());

  arma::mat BSE = arma::zeros(k, k);
  BSE.elem(find_nonfinite(RestrictionMatrix)) = FishObs.subvec(0, k * k - 1 - restrictions);

  arma::mat LambdaSE1 = arma::zeros(k);
  LambdaSE1.elem(find_nonfinite(RestrictionMatrixLambda)) = FishObs.subvec(k * k - restrictions, k * k + k - 1 - restrictions - restrictionsLambda);
  arma::mat LambdaSE = arma::diagmat(LambdaSE1);

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


// [[Rcpp::export]]
Rcpp::List IdentifyVolatility3(int crit, const arma::mat& u, double TB1, double TB2, double TB3, arma::uvec& Regime1, arma::uvec& Regime2,
                               arma::uvec& Regime3, int p, int k, arma::mat RestrictionMatrix, std::string type,  arma::vec RestrictionMatrixLambda1, arma::vec RestrictionMatrixLambda2, int restrictionsLambda,
                              int restrictions, double Tob, arma::mat SigmaHat1, arma::mat SigmaHat2, arma::mat SigmaHat3,
                              arma::mat Zt, arma::mat y, int maxIter){

  arma::mat SigmaHat = u.t() * u / (Tob);


  arma::mat initB = arma::chol(SigmaHat, "lower");

  arma::mat initBvec = arma::ones(k * k);
  initBvec = initB.elem(find_nonfinite(RestrictionMatrix));

  arma::vec initLambda1 = arma::ones(k);
  arma::vec Lambda1 = initLambda1.elem(find_nonfinite(RestrictionMatrixLambda1));


  arma::vec initLambda2 = arma::ones(k);
  arma::vec Lambda2 = initLambda2.elem(find_nonfinite(RestrictionMatrixLambda2));

  arma::vec S = arma::join_vert(initBvec, Lambda1, Lambda2);


  arma::vec likelihoods = {1e25}; // log likelihoods are directly stored in vector instead

  Rcpp::List hessian = Rcpp::List::create(arma::ones(k * k, k * k));
  Rcpp::List GLSE = Rcpp::List::create(arma::ones(p * k * k));

  Rcpp::List MLE = nlmCV3(S, TB1, TB2, TB3, SigmaHat1, k,  SigmaHat2, SigmaHat3, RestrictionMatrix, restrictions, RestrictionMatrixLambda1, RestrictionMatrixLambda2, restrictionsLambda);
  arma::vec Lestimates = MLE[1];

  arma::mat BLoop = arma::zeros(k, k);
  BLoop.elem(find_nonfinite(RestrictionMatrix)) = Lestimates.subvec(0, k * k - 1 - restrictions);
  Rcpp::List BHat = Rcpp::List::create(BLoop);

  arma::vec r1 = RestrictionMatrixLambda1.elem(find_finite(RestrictionMatrixLambda1));

  arma::mat LambdaFirstInit1 = arma::ones(k);
  LambdaFirstInit1.elem(find_nonfinite(RestrictionMatrixLambda1)) = Lestimates.subvec(k * k - restrictions, k * k + k - 1 - restrictions - r1.n_elem);
  LambdaFirstInit1.elem(find_finite(RestrictionMatrixLambda1)) = RestrictionMatrixLambda1.elem(find_finite(RestrictionMatrixLambda1));
  arma::mat LambdaFirst1 = arma::diagmat(LambdaFirstInit1);
  Rcpp::List Lambda1Hat = Rcpp::List::create(LambdaFirst1);

  arma::mat LambdaFirstInit2 = arma::ones(k);
  LambdaFirstInit2.elem(find_nonfinite(RestrictionMatrixLambda2)) = Lestimates.subvec(k * k + k - restrictions - r1.n_elem, k * k + k + k - 1 - restrictions - restrictionsLambda);
  LambdaFirstInit2.elem(find_finite(RestrictionMatrixLambda2)) = RestrictionMatrixLambda2.elem(find_finite(RestrictionMatrixLambda2));
  arma::mat LambdaFirst2 = arma::diagmat(LambdaFirstInit2);
  Rcpp::List Lambda2Hat = Rcpp::List::create(LambdaFirst2);

  int count = 0;
  double Exit = 1;

  while (Exit > crit && count < maxIter) {
    arma::mat BhatInd = BHat[count];
    arma::mat Lambda1Ind = Lambda1Hat[count];
    arma::mat Lambda2Ind = Lambda2Hat[count];

    arma::mat Sig1 = arma::inv(BhatInd * arma::trans(BhatInd));
    arma::mat Sig2 = arma::inv(BhatInd * (Lambda1Ind * arma::trans(BhatInd)));
    arma::mat Sig3 = arma::inv(BhatInd * (Lambda2Ind * arma::trans(BhatInd)));

    arma::mat GLS11 = arma::kron((Zt.cols(Regime1) * arma::trans(Zt.cols(Regime1))), Sig1);
    arma::mat GLS12 = arma::kron(Zt.cols(Regime2) * arma::trans(Zt.cols(Regime2)), Sig2);
    arma::mat GLS13 = arma::kron(Zt.cols(Regime3) * arma::trans(Zt.cols(Regime3)), Sig3);

    arma::mat GLS1 = arma::inv(GLS11 + GLS12 + GLS13);

    // Differentiating between different cases of constant/trend/none
    arma::mat GLS21 = arma::zeros(k * k * p, Regime1.n_elem);
    arma::mat GLS22 = arma::zeros(k * k * p, Regime2.n_elem);
    arma::mat GLS23 = arma::zeros(k * k * p, Regime3.n_elem);
    if (type == "const" || type == "trend") {
      GLS21 = arma::zeros(k * k * p + k, Regime1.n_elem);
      GLS22 = arma::zeros(k * k * p + k, Regime2.n_elem);
      GLS23 = arma::zeros(k * k * p + k, Regime3.n_elem);
    } else if (type == "both") {
      GLS21 = arma::zeros(k * k * p + 2 * k, Regime1.n_elem);
      GLS22 = arma::zeros(k * k * p + 2 * k, Regime2.n_elem);
      GLS23 = arma::zeros(k * k * p + 2 * k, Regime3.n_elem);
    }

    int j1 = 0;
    int j2 = 0;
    int j3 = 0;
    for (auto i = 0u; i < Zt.n_cols; ++i) {
      if (any(i == Regime1)) {
        GLS21.col(j1) = arma::kron(Zt.col(i), Sig1) * y.col(i);
        j1 += 1;
      } else if (any(i == Regime2)) {
        GLS22.col(j2) = arma::kron(Zt.col(i), Sig2) * y.col(i);
        j2 += 1;
      } else if (any(i == Regime3)) {
        GLS23.col(j3) = arma::kron(Zt.col(i), Sig3) * y.col(i);
        j3 += 1;
      }
    }

    arma::mat GLS21sums = arma::sum(GLS21, 1);
    arma::mat GLS22sums = arma::sum(GLS22, 1);
    arma::mat GLS23sums = arma::sum(GLS23, 1);
    arma::mat GLS2 = GLS21sums + GLS22sums + GLS23sums;
    arma::mat GLSHat = GLS1 * GLS2;

    arma::mat term1(k, Zt.n_cols);
    term1.zeros();
    arma::mat kMat(k,k);
    kMat.eye();

    term1 = reshape(kron(arma::trans(Zt), kMat) * GLSHat, k, Zt.n_cols);
    arma::mat ugls = arma::trans(y) - arma::trans(term1);
    arma::mat resid1gls = ugls.rows(Regime1);
    arma::mat resid2gls = ugls.rows(Regime2);
    arma::mat resid3gls = ugls.rows(Regime3);
    arma::mat Sigma_hat1gls =  (arma::trans(resid1gls) * resid1gls) / TB1;
    arma::mat Sigma_hat2gls = (arma::trans(resid2gls) * resid2gls) / TB2;
    arma::mat Sigma_hat3gls = (arma::trans(resid3gls) * resid3gls) / TB3;

    Rcpp::List MLEgls = nlmCV3(S,TB1, TB2, TB3, Sigma_hat1gls, k,  Sigma_hat2gls, Sigma_hat3gls, RestrictionMatrix, restrictions, RestrictionMatrixLambda1, RestrictionMatrixLambda2, restrictionsLambda);
    arma::mat GLSBLoop = arma::zeros(k, k);
    arma::vec GLSestimates = MLEgls[1];

    GLSBLoop.elem(find_nonfinite(RestrictionMatrix)) = GLSestimates.subvec(0, k * k - 1 - restrictions);

    BHat.push_back(GLSBLoop);

    arma::mat GLSLambda1LoopInit = arma::ones(k);
    GLSLambda1LoopInit.elem(arma::find_nonfinite(RestrictionMatrixLambda1)) = GLSestimates.subvec(k * k - restrictions, k * k + k - 1 - restrictions - r1.n_elem);
    GLSLambda1LoopInit.elem(arma::find_finite(RestrictionMatrixLambda1)) = RestrictionMatrixLambda1.elem(find_finite(RestrictionMatrixLambda1));


    arma::mat GLSLambda1Loop = arma::diagmat(GLSLambda1LoopInit);
    Lambda1Hat.push_back(GLSLambda1Loop);

    arma::mat GLSLambda2LoopInit = arma::ones(k);
    GLSLambda2LoopInit.elem(find_nonfinite(RestrictionMatrixLambda2)) = GLSestimates.subvec(k * k + k - restrictions - r1.n_elem, k * k + k + k - 1 - restrictions - restrictionsLambda);
    GLSLambda2LoopInit.elem(find_finite(RestrictionMatrixLambda2)) = RestrictionMatrixLambda2.elem(find_finite(RestrictionMatrixLambda2));


    arma::mat GLSLambda2Loop = arma::diagmat(GLSLambda2LoopInit);
    Lambda2Hat.push_back(GLSLambda2Loop);

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
  arma::mat Lambda1Opt = Lambda1Hat[cc];
  arma::mat Lambda2Opt = Lambda2Hat[cc];
  arma::mat GLSEOpt = GLSE[cc];
  GLSEOpt.reshape(k, GLSEOpt.size() / k);

  // Optaining standard errors
  arma::mat HESS = hessian[cc];
  HESS = HESS.i();

  for(auto i = 0u; i < HESS.n_rows; ++i){
    if (HESS(i, i) < 0.0) {
      HESS.col(i) = HESS.col(i) * (-1);
    }
  }

  arma::vec FishObs = arma::sqrt(HESS.diag());

  arma::mat BSE = arma::zeros(k, k);
  BSE.elem(find_nonfinite(RestrictionMatrix)) = FishObs.subvec(0, k * k - 1 - restrictions);

  arma::mat Lambda1SE1 = arma::zeros(k);
  arma::mat Lambda2SE1 = arma::zeros(k);
  Lambda1SE1.elem(find_nonfinite(RestrictionMatrixLambda1)) = FishObs.subvec(k * k - restrictions, k * k + k - 1 - restrictions - r1.n_elem);
  Lambda2SE1.elem(find_nonfinite(RestrictionMatrixLambda2)) = FishObs.subvec(k * k + k - restrictions - r1.n_elem, k * k + k + k - 1 - restrictions - restrictionsLambda);

  arma::mat Lambda1SE = arma::diagmat(Lambda1SE1);
  arma::mat Lambda2SE = arma::diagmat(Lambda2SE1);

  //Returning an R like list object with all results from optimization
  return Rcpp::List::create(Rcpp::Named("Lambda") = Lambda1Opt,
                            Rcpp::Named("Lambda2") = Lambda2Opt,
                            Rcpp::Named("Lambda_SE") = Lambda1SE,
                            Rcpp::Named("Lambda2_SE") = Lambda2SE,
                            Rcpp::Named("B") = BOpt,
                            Rcpp::Named("B_SE") = BSE,
                            Rcpp::Named("Fish") = HESS,
                            Rcpp::Named("Lik") = llbest,
                            Rcpp::Named("iteration") = count,
                            Rcpp::Named("A_hat") = GLSEOpt);

}
