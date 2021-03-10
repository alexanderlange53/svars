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

  for (auto i = 0u; i < G.n_elem; ++i) {
    arma::mat Omega = (1 - G(i)) * Sigma1 + G(i) * Sigma2;
    ll(i) = log(arma::det(Omega)) +  arma::as_scalar(u.row(i) * arma::inv(Omega) * u.row(i).t());
  }

  return (-1) * (- Tob * k / 2 * log(2 * M_PI) - arma::sum(ll) * 0.5);

}

// Likelihood for smooth transition model
// [[Rcpp::export]]
double LikelihoodST2(arma::vec& parameter, double& Tob, arma::mat& u, int& k,
                    arma::vec& G1, arma::vec& G3, arma::mat& RestrictionMatrix, int& restrictions){

  arma::mat B(size(RestrictionMatrix), arma::fill::zeros);
  B.elem(find_nonfinite(RestrictionMatrix)) = parameter.subvec(0, (k * k - 1) - restrictions);
  arma::mat Lambda1 =  arma::diagmat(parameter.subvec((k * k - restrictions), (k * k + (k - 1) - restrictions)));
  arma::mat Lambda2 =  arma::diagmat(parameter.subvec((k * k + k - restrictions), (k * k + 2*k - 1 - restrictions)));

  if (any(Lambda1.diag() < 0.0)) {
    return 1e25;
  }
  if (any(Lambda2.diag() < 0.0)) {
    return 1e25;
  }

  arma::mat Sigma1 = B * B.t();
  arma::mat Sigma2 = B * Lambda1 * B.t();
  arma::mat Sigma3 = B * Lambda2 * B.t();


  arma::vec ll(G1.n_elem);
  ll.fill(0.0);

  for (auto i = 0u; i < G1.n_elem; ++i) {
    arma::mat Omega = G1(i) * Sigma1 + (1 - G1(i) - G3(i)) * Sigma2 + G3(i) * Sigma3;
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

  return MLE;
}

// optimization of likelihood via nlm and exporting a list for 3 regimes
// [[Rcpp::export]]
Rcpp::List nlmST2(const arma::vec& S, double Tob, const arma::mat u, int k,
                 const arma::vec transition1, const arma::vec transition2, const arma::mat RestrictionMatrix, int restrictions){

  Rcpp::Environment stats("package:stats");
  Rcpp::Function nlm = stats["nlm"];

  Rcpp::List MLE = nlm(Rcpp::_["f"] = Rcpp::InternalFunction(LikelihoodST2),
                       Rcpp::_["p"] = S,
                       Rcpp::_["hessian"] = "T",
                       Rcpp::_["iterlim"] = 150,
                       Rcpp::_["Tob"] = Tob,
                       Rcpp::_["u"] = u,
                       Rcpp::_["k"] = k,
                       Rcpp::_["G1"] = transition1,
                       Rcpp::_["G2"] = transition2,
                       Rcpp::_["RestrictionMatrix"] = RestrictionMatrix,
                       Rcpp::_["restrictions"] = restrictions);

  return MLE;
}

// Multivariate GLS estimator for smooth transition model
// [[Rcpp::export]]
arma::vec mGLSst(const arma::vec transition, const arma::mat& B, const arma::mat& Lambda,
                 const arma::mat Z_t, int k, const arma::mat Y){

  arma::mat W = arma::zeros(k * transition.n_elem, k * transition.n_elem);
  arma::mat I = arma::eye(k, k);

  for (auto i = 0u; i < Z_t.n_cols; ++i) {
    W(arma::span(i*k, (i + 1) * k - 1), arma::span(i*k, (i + 1) * k - 1)) = arma::inv((1 - transition(i)) * B * B.t() + transition(i) * B * Lambda * B.t());
  }

  return arma::inv(arma::kron(Z_t, I) * W * arma::kron(Z_t.t(), I)) * arma::kron(Z_t, I) * W * arma::vectorise(Y);
}

// Multivariate GLS estimator for smooth transition model with 3 regimes
// [[Rcpp::export]]
arma::vec mGLSst2(const arma::vec transition1, const arma::vec transition2, const arma::mat& B, const arma::mat& Lambda1, const arma::mat& Lambda2,
                 const arma::mat Z_t, int k, const arma::mat Y){

  arma::mat W = arma::zeros(k * Z_t.n_cols, k * Z_t.n_cols);
  arma::mat I = arma::eye(k, k);

  for (auto i = 0u; i < Z_t.n_cols; ++i) {
    W(arma::span(i*k, (i + 1) * k - 1), arma::span(i*k, (i + 1) * k - 1)) = arma::inv(transition1(i) * B * B.t() + (1 - transition1(i) - transition2(i)) * B * Lambda1 * B.t() + transition2(i) * B * Lambda2 * B.t());
  }

  return arma::inv(arma::kron(Z_t, I) * W * arma::kron(Z_t.t(), I)) * arma::kron(Z_t, I) * W * arma::vectorise(Y);
}

// Multivariate GLS estimator for smooth transition model with 3 regimes and tvar
// [[Rcpp::export]]
arma::vec mGLSst2_tvar(const arma::vec transition1, const arma::vec transition2, const arma::mat& B, const arma::mat& Lambda1, const arma::mat& Lambda2,
                  const arma::mat Z_t, int Tob_beg, int k, const arma::mat Y){

  arma::mat W = arma::zeros(k * Z_t.n_cols, k * Z_t.n_cols);
  arma::mat I = arma::eye(k, k);

  int end = Tob_beg + Z_t.n_cols;

  int j = 0;

  for (auto i = Tob_beg; i < end; ++i) {
    W(arma::span(j*k, (j + 1) * k - 1), arma::span(j*k, (j + 1) * k - 1)) = arma::inv(transition1(i) * B * B.t() + (1 - transition1(i) - transition2(i)) * B * Lambda1 * B.t() + transition2(i) * B * Lambda2 * B.t());
    j +=1;
  }

  return arma::inv(arma::kron(Z_t, I) * W * arma::kron(Z_t.t(), I)) * arma::kron(Z_t, I) * W * arma::vectorise(Y);
}

// Algorithm from GLS estimation and likelihood optimization for ST model
// [[Rcpp::export]]
Rcpp::List IterativeSmoothTransition(const arma::vec& transition, const arma::mat& u, int& Tob, int& k, int& p,
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

 for(auto i = 0u; i < HESS.n_rows; ++i){
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

// Algorithm from GLS estimation and likelihood optimization for ST model with 3 Regimes
// [[Rcpp::export]]
Rcpp::List IterativeSmoothTransition2(const arma::vec& transition1, const arma::vec& transition2, const arma::mat& u, int& Tob, int& k, int& p,
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
  arma::mat initLambda1 = arma::eye(k, k);
  arma::mat initLambda2 = arma::eye(k, k);
  Rcpp::List LambdaHat1 = Rcpp::List::create(initLambda1);
  Rcpp::List LambdaHat2 = Rcpp::List::create(initLambda2);

  arma::vec S = arma::join_vert(initBvec, initLambda1.diag(), initLambda2.diag());

  arma::vec likelihoods = {1e25}; // log likelihoods are directly stored in vector instead

  Rcpp::List hessian = Rcpp::List::create(arma::ones(k^2, k^2));
  Rcpp::List GLSE = Rcpp::List::create(arma::ones(p * k^2));

  // iterative procedure
  while (Exit > crit && count < maxIter) {

    // Extracting previously generated/estimated parameter as starting values of optimization
    arma::mat initBloop = BHat[count];
    initBvec = initBloop.elem(find_nonfinite(RestrictionMatrix));

    arma::mat initLambdaloop1 = LambdaHat1[count];
    arma::mat initLambdaloop2 = LambdaHat2[count];

    S = arma::join_vert(initBvec, initLambdaloop1.diag(), initLambdaloop2.diag()); // vectorising

    // Step 1: Likleihood optimization
    Rcpp::List mle = nlmST2(S, Tob, Ugls, k,
                           transition1, transition2, RestrictionMatrix, restrictions);

    // Storing optimized Parameter and liklihood
    arma::mat BLoop = arma::zeros(k, k);
    arma::vec Lestimates = mle[1];

    BLoop.elem(find_nonfinite(RestrictionMatrix)) = Lestimates.subvec(0, k * k - 1 - restrictions);
    arma::mat LambdaLoop1 = arma::diagmat(Lestimates.subvec(k * k - restrictions, k * k + k - 1 - restrictions));
    arma::mat LambdaLoop2 = arma::diagmat(Lestimates.subvec(k * k + k - restrictions, k * k + 2*k - 1 - restrictions));

    BHat.push_back(BLoop);
    LambdaHat1.push_back(LambdaLoop1);
    LambdaHat2.push_back(LambdaLoop2);

    int sz = likelihoods.size(); // Dynamicly increase the size of the likelihood storing vector
    likelihoods.resize(sz + 1);
    likelihoods(sz) = mle[0];

    hessian.push_back(mle[3]);

    // Step 2: Reestimation of VAR parameter with GLS
    arma::vec BetaGLS = mGLSst2(transition1, transition2, BHat[count + 1], LambdaHat1[count + 1], LambdaHat2[count + 1], Z_t, k, Yloop);

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
  arma::mat LambdaOpt1 = LambdaHat1[cc];
  arma::mat LambdaOpt2 = LambdaHat2[cc];
  arma::mat GLSEOpt = GLSE[cc];
  GLSEOpt.reshape(k, GLSEOpt.size() / k); // Unknown column dimension, depending on deterministic parts

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
  arma::mat LambdaSE1 = arma::diagmat(FishObs.subvec(k * k - restrictions, k * k + k - 1 - restrictions));
  arma::mat LambdaSE2 = arma::diagmat(FishObs.subvec(k * k + k - restrictions, k * k + 2*k - 1 - restrictions));

  // Returning an R like list object with all results from optimization
  return Rcpp::List::create(Rcpp::Named("Lambda1") = LambdaOpt1,
                            Rcpp::Named("Lambda2") = LambdaOpt2,
                            Rcpp::Named("Lambda1_SE") = LambdaSE1,
                            Rcpp::Named("Lambda2_SE") = LambdaSE2,
                            Rcpp::Named("B") = BOpt,
                            Rcpp::Named("B_SE") = BSE,
                            Rcpp::Named("Fish") = HESS,
                            Rcpp::Named("Lik") = llbest * (-1),
                            Rcpp::Named("iteration") = count,
                            Rcpp::Named("A_hat") = GLSEOpt);
}

// Algorithm from GLS estimation and likelihood optimization for ST model with 3 Regimes
// [[Rcpp::export]]
Rcpp::List IterativeSmoothTransition2_TVAR(const arma::vec& transition1, const arma::vec& transition2, const arma::mat& u, int& Tob, int Tob1, int Tob2, int Tob3,
                                           int& k, int& p,
                                          double& crit, int& maxIter, arma::mat& Z_t1, arma::mat& Z_t2, arma::mat& Z_t3,
                                          arma::mat& Yloop1,  arma::mat& Yloop2,  arma::mat& Yloop3,
                                          arma::mat& RestrictionMatrix,
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
  arma::mat initLambda1 = arma::eye(k, k);
  arma::mat initLambda2 = arma::eye(k, k);
  Rcpp::List LambdaHat1 = Rcpp::List::create(initLambda1);
  Rcpp::List LambdaHat2 = Rcpp::List::create(initLambda2);

  arma::vec S = arma::join_vert(initBvec, initLambda1.diag(), initLambda2.diag());

  arma::vec likelihoods = {1e25}; // log likelihoods are directly stored in vector instead

  Rcpp::List hessian = Rcpp::List::create(arma::ones(k^2, k^2));
  Rcpp::List GLSE1 = Rcpp::List::create(arma::ones(p * k^2));
  Rcpp::List GLSE2 = Rcpp::List::create(arma::ones(p * k^2));
  Rcpp::List GLSE3 = Rcpp::List::create(arma::ones(p * k^2));


  // iterative procedure
  while (Exit > crit && count < maxIter) {

    // Extracting previously generated/estimated parameter as starting values of optimization
    arma::mat initBloop = BHat[count];
    initBvec = initBloop.elem(find_nonfinite(RestrictionMatrix));

    arma::mat initLambdaloop1 = LambdaHat1[count];
    arma::mat initLambdaloop2 = LambdaHat2[count];

    S = arma::join_vert(initBvec, initLambdaloop1.diag(), initLambdaloop2.diag()); // vectorising

    // Step 1: Likleihood optimization
    Rcpp::List mle = nlmST2(S, Tob, Ugls, k,
                            transition1, transition2, RestrictionMatrix, restrictions);

    // Storing optimized Parameter and liklihood
    arma::mat BLoop = arma::zeros(k, k);
    arma::vec Lestimates = mle[1];

    BLoop.elem(find_nonfinite(RestrictionMatrix)) = Lestimates.subvec(0, k * k - 1 - restrictions);
    arma::mat LambdaLoop1 = arma::diagmat(Lestimates.subvec(k * k - restrictions, k * k + k - 1 - restrictions));
    arma::mat LambdaLoop2 = arma::diagmat(Lestimates.subvec(k * k + k - restrictions, k * k + 2*k - 1 - restrictions));

    BHat.push_back(BLoop);
    LambdaHat1.push_back(LambdaLoop1);
    LambdaHat2.push_back(LambdaLoop2);

    int sz = likelihoods.size(); // Dynamicly increase the size of the likelihood storing vector
    likelihoods.resize(sz + 1);
    likelihoods(sz) = mle[0];

    hessian.push_back(mle[3]);

    // Step 2: Reestimation of VAR parameter with GLS
    arma::vec BetaGLS1 = mGLSst2_tvar(transition1, transition2, BHat[count + 1], LambdaHat1[count + 1], LambdaHat2[count + 1], Z_t1, 0, k, Yloop1);
    arma::vec BetaGLS2 = mGLSst2_tvar(transition1, transition2, BHat[count + 1], LambdaHat1[count + 1], LambdaHat2[count + 1], Z_t2, Z_t1.n_cols-1, k, Yloop2);
    arma::vec BetaGLS3 = mGLSst2_tvar(transition1, transition2, BHat[count + 1], LambdaHat1[count + 1], LambdaHat2[count + 1], Z_t3, Z_t2.n_cols-1, k, Yloop3);



    GLSE1.push_back(BetaGLS1);
    GLSE2.push_back(BetaGLS2);
    GLSE3.push_back(BetaGLS3);

    // Generating residuals which schould be free of unconditional heteroskedasticity
    arma::mat Ugls1 = arma::vectorise(Yloop1) - arma::kron(Z_t1.t(), I) * BetaGLS1;
    Ugls1.reshape(k, Tob1);
    Ugls1 = Ugls1.t();
    arma::mat Ugls2 = arma::vectorise(Yloop2) - arma::kron(Z_t2.t(), I) * BetaGLS2;
    Ugls2.reshape(k, Tob2);
    Ugls2 = Ugls2.t();
    arma::mat Ugls3 = arma::vectorise(Yloop3) - arma::kron(Z_t3.t(), I) * BetaGLS3;
    Ugls3.reshape(k, Tob3);
    Ugls3 = Ugls3.t();

    Ugls = arma::join_vert(Ugls1, Ugls2, Ugls3);

    count += 1;

    Exit =  likelihoods(count - 1) - likelihoods(count);
  }

  // extracting the best estimates
  arma::vec ll = likelihoods;
  int cc = ll.index_min();
  double llbest = ll.min();

  arma::mat BOpt = BHat[cc];
  arma::mat LambdaOpt1 = LambdaHat1[cc];
  arma::mat LambdaOpt2 = LambdaHat2[cc];
  arma::mat GLSEOpt1 = GLSE1[cc];
  arma::mat GLSEOpt2 = GLSE2[cc];
  arma::mat GLSEOpt3 = GLSE3[cc];
  GLSEOpt1.reshape(k, GLSEOpt1.size() / k); // Unknown column dimension, depending on deterministic parts
  GLSEOpt2.reshape(k, GLSEOpt2.size() / k);
  GLSEOpt3.reshape(k, GLSEOpt3.size() / k);

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
  arma::mat LambdaSE1 = arma::diagmat(FishObs.subvec(k * k - restrictions, k * k + k - 1 - restrictions));
  arma::mat LambdaSE2 = arma::diagmat(FishObs.subvec(k * k + k - restrictions, k * k + 2*k - 1 - restrictions));

  // Returning an R like list object with all results from optimization
  return Rcpp::List::create(Rcpp::Named("Lambda1") = LambdaOpt1,
                            Rcpp::Named("Lambda2") = LambdaOpt2,
                            Rcpp::Named("Lambda1_SE") = LambdaSE1,
                            Rcpp::Named("Lambda2_SE") = LambdaSE2,
                            Rcpp::Named("B") = BOpt,
                            Rcpp::Named("B_SE") = BSE,
                            Rcpp::Named("Fish") = HESS,
                            Rcpp::Named("Lik") = llbest * (-1),
                            Rcpp::Named("iteration") = count,
                            Rcpp::Named("A_hat1") = GLSEOpt1,
                            Rcpp::Named("A_hat2") = GLSEOpt2,
                            Rcpp::Named("A_hat3") = GLSEOpt3);
}
