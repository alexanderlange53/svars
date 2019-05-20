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

  arma::mat Sigma1 = B * B.t();
  arma::mat Sigma2 = B * Lambda * B.t();


  arma::vec ll(G.n_elem);
  ll.fill(0.0);

  for (int i = 0; i < G.n_elem; ++i) {
    arma::mat Omega = (1 - G(i)) * Sigma1 + G(i) * Sigma2;
    ll(i) = log(arma::det(Omega)) +  arma::as_scalar(u.row(i) * arma::inv(Omega) * u.row(i).t());
  }

  return (-1) * (- Tob * k / 2 * log(2 * M_PI) - arma::sum(ll) * 0.5);

}

// optimization of likelihood via nlm and exporting a list
// [[Rcpp::export]]
Rcpp::List nlmST(const arma::vec& S, double Tob, const arma::mat u, int k,
                     const arma::vec transition, const arma::mat RestrictionMatrix, int restrictions){

  Rcpp::Environment stats("package:stats");
  Rcpp::Function nlm = stats["nlm"];

  Rcpp::List MLE = nlm(Rcpp::_["f"] = Rcpp::InternalFunction(LikelihoodST),
                          Rcpp::_["p"] = S,
                          Rcpp::_["hessian"] = "T",
                          Rcpp::_["iterlim"] = 150,
                          Rcpp::_["Tob"] = Tob,
                          Rcpp::_["u"] = u,
                          Rcpp::_["k"] = k,
                          Rcpp::_["G"] = transition,
                          Rcpp::_["RestrictionMatrix"] = RestrictionMatrix,
                          Rcpp::_["restrictions"] = restrictions);

  // Extract and coerce from list.
  //arma::vec out = Rcpp::as<arma::vec>(MLE[0]);

  return MLE;
}

// Creating Regressor matrix (used in multiple models)
/*
arma::mat = crZ(const arma::mat& y, int& p){
  arma::mat Z = arma::zeros(y.cols - p, y.rows * p);

  for (int i = 0; i < p; ++i) {
    Z.col(i * Z.ncol, (i + 1) * Z.ncol - 1) = y(0, span(y.nrows - p), span());
  }
}
*/

// Multivariate GLS estimator for smooth transition model
// [[Rcpp::export]]
arma::vec mGLSst(const arma::vec transition, const arma::mat& B, const arma::mat& Lambda,
                 const arma::mat Z_t, int k, const arma::mat Y){

  arma::mat W = arma::zeros(k * transition.n_elem, k * transition.n_elem);
  arma::mat I = arma::eye(k, k);

  for (int i = 0; i < transition.n_elem; ++i) {
    W(arma::span(i*k, (i + 1) * k - 1), arma::span(i*k, (i + 1) * k - 1)) = arma::inv((1 - transition(i)) * B * B.t() + transition(i) * B * Lambda * B.t());
  }

  return arma::inv(arma::kron(Z_t, I) * W * arma::kron(Z_t.t(), I)) * arma::kron(Z_t, I) * W * arma::vectorise(Y);
}

// Algorithm from GLS estimation and likelihood optimization for St model
// [[Rcpp::export]]
Rcpp::List IterativeSmoothTransition(const arma::vec& transition, const arma::mat& u, arma::mat& Y, int& Tob, int& k, int& p,
                                     double& crit, int& maxIter, arma::mat& Z_t, arma::mat& Yloop, arma::mat& RestrictionMatrix,
                                     int&  restrictions){
  int count = 0;
  double Exit =  1000;
  arma::mat I = arma::eye(k, k);
  arma::mat Ugls = u;

  // Creating initial values for structural parameter
  arma::mat SigmaHat = u.t() * u / (Tob - 1 - k * p);

  arma::mat initB = arma::chol(SigmaHat, "lower");
  Rcpp::List BHat = Rcpp::List::create(initB);
  arma::mat initBvec = arma::ones(k * k);

  initBvec = initB.elem(find_nonfinite(RestrictionMatrix)); // selecting elements according to restriction matrix

  // Creating lists which stores the results from every loop iteration
  arma::mat initLambda = arma::eye(k, k);
  Rcpp::List LambdaHat = Rcpp::List::create(initLambda);

  arma::vec S = arma::join_vert(initBvec, initLambda.diag());

  arma::vec likelihoods = {1e25}; // log likelihoods are directly stored in vector instead

  Rcpp::List hessian = Rcpp::List::create(arma::ones(k^2, k^2));
  Rcpp::List GLSE = Rcpp::List::create(arma::ones(p * k^2));

  // iterative procedure
  while (Exit > crit && count < maxIter) {

    // Extracting previously generated/estimated parameter as starting values of optimization
    arma::mat initBloop = BHat[count];
    initBvec = initBloop.elem(find_nonfinite(RestrictionMatrix));

    arma::mat initLambdaloop = LambdaHat[count];

    S = arma::join_vert(initBvec, initLambdaloop.diag()); // vectorising

    // Step 1: Likleihood optimization
    Rcpp::List mle = nlmST(S, Tob, Ugls, k,
                          transition, RestrictionMatrix, restrictions);

    // Storing optimized Parameter and liklihood
    arma::mat BLoop = arma::zeros(k, k);
    arma::vec Lestimates = mle[1];

    BLoop.elem(find_nonfinite(RestrictionMatrix)) = Lestimates.subvec(0, k * k - 1 - restrictions);
    arma::mat LambdaLoop = arma::diagmat(Lestimates.subvec(k * k - restrictions, k * k + k - 1 - restrictions));

    BHat.push_back(BLoop);
    LambdaHat.push_back(LambdaLoop);

    int sz = likelihoods.size(); // Dynamicly increase the size of the likelihood storing vector
    likelihoods.resize(sz + 1);
    likelihoods(sz) = mle[0];

    hessian.push_back(mle[3]);

    // Step 2: Reestimation of VAR parameter with GLS
    arma::vec BetaGLS = mGLSst(transition, BHat[count + 1], LambdaHat[count + 1], Z_t, k, Yloop);

    GLSE.push_back(BetaGLS);

    // Generating residuals which schould be free of unconditional heteroskedasticity
    Ugls = arma::vectorise(Yloop) - arma::kron(Z_t.t(), I) * BetaGLS;
    Ugls.reshape(k, Tob);
    Ugls = Ugls.t();

    count += 1;

    Exit =  likelihoods(count - 1) - likelihoods(count);
  }

 // extracting the best estimates
 arma::vec ll = likelihoods;
 int cc = ll.index_min();
 double llbest = ll.min();

 arma::mat BOpt = BHat[cc];
 arma::mat LambdaOpt = LambdaHat[cc];
 arma::mat GLSEOpt = GLSE[cc];
 GLSEOpt.reshape(k, GLSEOpt.size() / k); // Unknown column dimension, depending on deterministic parts

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

 // Returning an R like list object with all results from optimization
 return Rcpp::List::create(Rcpp::Named("Lambda") = LambdaOpt,
                           Rcpp::Named("Lambda_SE") = LambdaSE,
                           Rcpp::Named("B") = BOpt,
                           Rcpp::Named("B_SE") = BSE,
                           Rcpp::Named("Fish") = HESS,
                           Rcpp::Named("Lik") = llbest * (-1),
                           Rcpp::Named("iteration") = count,
                           Rcpp::Named("A_hat") = GLSEOpt);
}
