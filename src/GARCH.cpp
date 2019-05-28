#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

// Likelihood for GARCH Model (univariate optimization)
// [[Rcpp::export]]
double LikelihoodGARCHu(const arma::vec& parameter, arma::vec& est, double& Sigma1, int& Tob){

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

// optimization of univariate likelihood via nlm and exporting a list
// [[Rcpp::export]]
Rcpp::List nlmGARCHu(const arma::vec parameter, const arma::vec est, double Sigma1, int Tob){

  Rcpp::Environment stats("package:stats");
  Rcpp::Function nlm = stats["nlm"];

  Rcpp::List MLE = nlm(Rcpp::_["f"] = Rcpp::InternalFunction(LikelihoodGARCHu),
                       Rcpp::_["p"] = parameter,
                       Rcpp::_["hessian"] = "T",
                       Rcpp::_["iterlim"] = 150,
                       Rcpp::_["est"] = est,
                       Rcpp::_["Sigma1"] = Sigma1,
                       Rcpp::_["Tob"] = Tob);

  return MLE;
}

// Univariate GARCH(1, 1) variances
// [[Rcpp::export]]
arma::vec SigmaGARCHuniv(const arma::vec& param, int Tob, double& SigmaE, const arma::vec est){

  arma::vec SigmaOut(Tob);
  SigmaOut(0) = SigmaE;

  for (int i = 1; i < Tob; i++) {
    SigmaOut(i) =  param(0) + param(1) * pow(est(i-1), 2) + param(2) * SigmaOut(i-1);
  }

  return SigmaOut;
}

// Finding start values
// [[Rcpp::export]]
Rcpp::List GarchStart(int& StartIter, int& k, arma::mat& ste, int& Tob){

  Rcpp::List PiterBlind = Rcpp::List::create(Rcpp::Named("ParameterE") = arma::zeros(k, 3),
                                              Rcpp::Named("Likelihoods") = 1e25,
                                              Rcpp::Named("ConVariance") = arma::zeros(Tob, k));
  Rcpp::List parameterConsider = Rcpp::List::create(PiterBlind);

  for (int i = 0; i < StartIter; i++) {
    // Stage 1: Univariate optimization of GARCH(1, 1) parameter
    // Initial values as in Luetkepohl + Schlaak (2018)
    arma::vec initGamma = Rcpp::runif(k);
    arma::vec initG(k, arma::fill::zeros);
    double testG =  0;
    for (int j = 0; j < k; j++) {
      testG = R::runif(0, 1);
      if (initGamma(j) + testG < 1) {
        initG(j) = testG;
      } else {
        while (initGamma(j) + testG > 1) {
          testG = R::runif(0, 1);
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

    for (int l = 0; l < k; l++) {
      Rcpp::List maxL = nlmGARCHu(parameterIni.row(l).t(), ste.row(l).t(), SigmaE0(l), Tob);

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
