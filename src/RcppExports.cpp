// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// duplicatedInt
LogicalVector duplicatedInt(IntegerVector x);
RcppExport SEXP _SpaDES_tools_duplicatedInt(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(duplicatedInt(x));
    return rcpp_result_gen;
END_RCPP
}
// runifC
NumericVector runifC(const int N);
RcppExport SEXP _SpaDES_tools_runifC(SEXP NSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type N(NSEXP);
    rcpp_result_gen = Rcpp::wrap(runifC(N));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_SpaDES_tools_duplicatedInt", (DL_FUNC) &_SpaDES_tools_duplicatedInt, 1},
    {"_SpaDES_tools_runifC", (DL_FUNC) &_SpaDES_tools_runifC, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_SpaDES_tools(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
