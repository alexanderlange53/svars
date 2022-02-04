#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

// Likelihood for GARCH Model (univariate optimization) ---------------------------------------------------------------
// [[Rcpp::export]]
double LikelihoodGARCHu(const arma::vec& parameter, arma::vec& est, double& Sigma1, int& Tob){

  double gamma  =  parameter(0);
  double g      =  parameter(1);

  // Checking for input validity
  if (gamma > 0.01 && g >= 0.01 && gamma + g < 0.98) {

    // Likelihood function
    double  L = 0;
    arma::vec Sigma2(Tob);
    Sigma2(0) = Sigma1;

    for (int i = 1; i < Tob; i++) {
      Sigma2(i) =  (1 - gamma - g) + gamma * pow(est(i - 1), 2) + g * Sigma2(i - 1);
      L += 0.5 * (Tob - 1) *(log(2 * M_PI) + log(Sigma2(i - 1)) + pow(est(i - 1), 2) / Sigma2(i - 1));
    }

    return L;

  } else {
    return 1000000000000;
  }
}

// optimization of univariate likelihood via nlm and exporting a list ----------------------------------------------
// [[Rcpp::export]]
Rcpp::List nlmGARCHu(const arma::vec parameter, const arma::vec est, double Sigma1, int Tob){

  Rcpp::Environment stats("package:stats");
  Rcpp::Function nlm = stats["nlm"];

  Rcpp::List MLE = nlm(Rcpp::_["f"] = Rcpp::InternalFunction(LikelihoodGARCHu),
                       Rcpp::_["p"] = parameter,
                       Rcpp::_["hessian"] = "T",
                       Rcpp::_["iterlim"] = 150,
                       Rcpp::_["steptol"] = 1e-5,
                       Rcpp::_["est"] = est,
                       Rcpp::_["Sigma1"] = Sigma1,
                       Rcpp::_["Tob"] = Tob);

  return MLE;
}

// Univariate GARCH(1, 1) variances -------------------------------------------------------------------------------
// [[Rcpp::export]]
arma::vec SigmaGARCHuniv(const arma::vec& param, int Tob, double& SigmaE, const arma::vec est){

  arma::vec SigmaOut(Tob);
  SigmaOut(0) = SigmaE;

  for (int i = 1; i < Tob; i++) {
    SigmaOut(i) =  param(0) + param(1) * pow(est(i-1), 2) + param(2) * SigmaOut(i-1);
  }

  return SigmaOut;
}

// Finding start values -------------------------------------------------------------------------------------------
// [[Rcpp::export]]
Rcpp::List GarchStart(int& k, arma::mat& ste, int& Tob, int start_iter){

  Rcpp::List PiterBlind = Rcpp::List::create(Rcpp::Named("ParameterE") = arma::zeros(k, 3),
                                              Rcpp::Named("Likelihoods") = 1e25,
                                              Rcpp::Named("ConVariance") = arma::zeros(Tob, k));
  Rcpp::List parameterConsider = Rcpp::List::create(PiterBlind);

  //int StartIter = 100;

  for (int i = 0; i < start_iter; i++) {
    // Stage 1: Univariate optimization of GARCH(1, 1) parameter
    // Initial values as in Luetkepohl + Schlaak (2018)
    arma::vec initGamma = Rcpp::runif(k, 0.01, 0.3);
    arma::vec initG(k, arma::fill::zeros);
    double testG =  0;
    for (int j = 0; j < k; j++) {
      testG = R::runif(0.6, 0.95);
      if (initGamma(j) + testG < 0.991) {
        initG(j) = testG;
      } else {
        int count = 0;
        while ((initGamma(j) + testG >= 0.991) | (count >= 10)) {
          testG = R::runif(0.6, 0.95);
          count += 1;
        }
        if (count >= 10) {
          testG = 0.991 - initGamma(j);
        }
        initG(j) = testG;
      }
    }

    arma::mat parameterIni = arma::join_horiz(initGamma, initG);

    // first observstion of strucutral variance is the estimated sample variance
    arma::mat SigmaE0m = arma::cov(ste.t());
    arma::vec SigmaE0 = SigmaE0m.diag();

    // optimizing the univariate likelihood functions
    arma::vec gammaUniv(k);
    arma::vec gUniv(k);
    arma::mat paramUniv = arma::zeros(3, k);
    arma::mat SigmaUniv = arma::zeros(Tob, k);

    arma::vec likvalues(k);

    //return Rcpp::List::create(parameterIni, SigmaE0);

    for (int l = 0; l < k; l++) {
      Rcpp::List maxL = nlmGARCHu(parameterIni.row(l).t(), ste.row(l).t(), SigmaE0(l), Tob);

      /*if (l > 1) {
        return maxL;
      }*/

      // Likelihood values
      likvalues(l) = maxL[0];

      // Optimized GARCH parameter
      arma::vec estimatedP = maxL[1];
      gammaUniv(l) = estimatedP(0);
      gUniv(l) = estimatedP(1);

      // Including a constant
      arma::vec tt = {(1- gammaUniv(l) - gUniv(l)), gammaUniv(l), gUniv(l)};
      paramUniv.col(l) = tt;

      // estimated conditional heteroskedasticity
      SigmaUniv.col(l) = SigmaGARCHuniv(paramUniv.col(l), Tob, SigmaE0(l), ste.row(l).t());
    }

    // Returning an R like list object with all results from optimization
    Rcpp::List Piter = Rcpp::List::create(Rcpp::Named("ParameterE") = arma::join_horiz(gammaUniv, gUniv),
                                          Rcpp::Named("Likelihoods") = arma::mean(likvalues),
                                          Rcpp::Named("ConVariance") = SigmaUniv);


    parameterConsider.push_back(Piter);

  }
  return parameterConsider;
}

// Likelihood for GARCH Model (multivariate optimization) ------------------------------------------------------------
// [[Rcpp::export]]
double LikelihoodGARCHm(arma::vec& parameter, arma::mat& SigmaE, int& Tob, int& k, arma::mat& u,
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

// optimization of univariate likelihood via nlm and exporting a list ---------------------------------------------
// [[Rcpp::export]]
Rcpp::List nlmGARCHm(const arma::vec parameter, const arma::mat& SigmaE, int Tob, int k, const arma::mat u,
                     const arma::mat RestrictionMatrix, int restrictions){

  Rcpp::Environment stats("package:stats");
  Rcpp::Function nlm = stats["nlm"];

  Rcpp::List MLE = nlm(Rcpp::_["f"] = Rcpp::InternalFunction(LikelihoodGARCHm),
                       Rcpp::_["p"] = parameter,
                       Rcpp::_["hessian"] = "T",
                       Rcpp::_["iterlim"] = 150,
                       Rcpp::_["SigmaE"] = SigmaE,
                       Rcpp::_["Tob"] = Tob,
                       Rcpp::_["k"] = k,
                       Rcpp::_["u"] = u,
                       Rcpp::_["RestrictionMatrix"] = RestrictionMatrix,
                       Rcpp::_["restrictions"] = restrictions);

  return MLE;
}

// Iterative procedure for GARCH identification --------------------------------------------------------------------
// [[Rcpp::export]]
Rcpp::List GARCHiterativeP(arma::vec& parameter, arma::mat& SigmaUniv, int& k, arma::mat& parameterIniu,
                           arma::mat& u, arma::mat& RestrictionMatrix, int& restrictions,
                           int& maxIter, int& Tob, double& crit){


  // create empty vectors and lists for results
  arma::vec gamma(k);
  arma::vec g(k);
  arma::vec param(k);

  arma::mat Best(size(RestrictionMatrix), arma::fill::zeros);

  Rcpp::List resultsB = Rcpp::List::create(arma::zeros(k));
  Rcpp::List resultsParam = Rcpp::List::create(arma::zeros(k, 2));

  int round = 0;
  double Exit = 1000.0;

  arma::vec likelihoods = {1e25}; // log likelihoods are directly stored in vector instead

  Rcpp::List multiHessianL = Rcpp::List::create(arma::zeros(k * k));
  Rcpp::List uniML = Rcpp::List::create(1e25);
  Rcpp::List uniHessianL = Rcpp::List::create(arma::zeros(2, 2));
  Rcpp::List SigmaUnivL = Rcpp::List::create(arma::zeros(Tob, k));
  arma::vec ini = parameter;


  while (fabs(Exit) > crit && round < maxIter){

    Rcpp::List maxML = nlmGARCHm(ini, SigmaUniv, Tob, k, u,
                                  RestrictionMatrix, restrictions);

    multiHessianL.push_back(maxML[3]);
    // initials for next round of univariate estimation
    arma::vec iniL = maxML[1];
    ini = iniL;

    Best.elem(find_nonfinite(RestrictionMatrix)) = ini;

    arma::mat BestI = Best.i();

    // save individual B matrices for each round
    resultsB.push_back(Best);

    // calculate new structural residuals for update of the GARCH parameters
    arma::mat EstResiduals = BestI * u.t();

    // Evaluating exit criterion
    int sz = likelihoods.size(); // Dynamicly increase the size of the likelihood storing vector
    likelihoods.resize(sz + 1);
    likelihoods(sz) = maxML[0];

    if (round > 0) {
        Exit =  likelihoods(sz - 1) - likelihoods(sz);
    }

    // re-estimate GARCH part, based on update of estimate of B
    // optimizing the univariate likelihood functions
    arma::vec gammaUniv(k);
    arma::vec gUniv(k);
    arma::mat paramUniv = arma::zeros(3, k);
    //arma::mat SigmaUniv = arma::zeros(Tob, k);
    Rcpp::List uniHessian = Rcpp::List::create(arma::zeros(2, 2));

    // first observstion of strucutral variance is the estimated sample variance
    arma::mat SigmaE0m = arma::cov(EstResiduals.t());
    arma::vec SigmaE0 = SigmaE0m.diag();

    for (int i = 0; i < k; i++) {
      Rcpp::List maxMLu = nlmGARCHu(parameterIniu.row(i).t(), EstResiduals.row(i).t(),
                                    SigmaE0(i), Tob);

      if (i == 0.0) {
        uniHessian[0] =  maxMLu[3];
      } else {
        uniHessian.push_back(maxMLu[3]);
      }

      arma::vec uniParam = maxMLu[1];

      // Optimized GARCH parameter
      gammaUniv(i) = uniParam(0);
      gUniv(i) = uniParam(1);

      // Including a constant
      arma::vec tt = {(1- gammaUniv(i) - gUniv(i)), gammaUniv(i), gUniv(i)};
      paramUniv.col(i) = tt;

      // estimated conditional heteroskedasticity
      SigmaUniv.col(i) = SigmaGARCHuniv(paramUniv.col(i), Tob, SigmaE0(i), EstResiduals.row(i).t());
    }

    SigmaUnivL.push_back(SigmaUniv);
    uniHessianL.push_back(uniHessian);
    parameterIniu = arma::join_horiz(gammaUniv, gUniv);

    resultsParam.push_back(parameterIniu);

    round += 1;

  } // end of while loop

  // extracting the best estimates
  arma::vec ll = likelihoods;
  int cc = ll.index_min();
  double llbest = ll.min();

  // Calculate log likelihood with normalizing constant
  double llf = log(sqrt(1 / pow((2 * M_PI), k) )) * Tob - llbest;

  arma::mat BHat = resultsB[cc];

  arma::mat GARCHparamHat = resultsParam[cc];
  arma::mat SigmaUnivHat = SigmaUnivL[cc];

  // Standard errors
  arma::mat FishObs = multiHessianL[cc];
  Rcpp::List FishObsU = uniHessianL[cc];

  // Returning an R like list object with all results from optimization
  return Rcpp::List::create(Rcpp::Named("B_hat") = BHat,
                            Rcpp::Named("GARCH_param_hat") = GARCHparamHat,
                            Rcpp::Named("Sigma_e_univ") = SigmaUnivHat,
                            Rcpp::Named("FishObs") = FishObs,
                            Rcpp::Named("Lik") = llf,
                            Rcpp::Named("FishObsU") = FishObsU,
                            Rcpp::Named("iteration") = round);
}
