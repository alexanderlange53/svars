#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _svars_IdentifyVolatilityNew(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _svars_LikelihoodCV(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _svars_LikelihoodST(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _svars_nlm_rcpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _svars_nlmST(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_svars_IdentifyVolatilityNew", (DL_FUNC) &_svars_IdentifyVolatilityNew, 15},
    {"_svars_LikelihoodCV",          (DL_FUNC) &_svars_LikelihoodCV,           8},
    {"_svars_LikelihoodST",          (DL_FUNC) &_svars_LikelihoodST,           7},
    {"_svars_nlm_rcpp",              (DL_FUNC) &_svars_nlm_rcpp,               8},
    {"_svars_nlmST",                 (DL_FUNC) &_svars_nlmST,                  7},
    {NULL, NULL, 0}
};

void R_init_svars(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
