// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

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
// pointDistance2
NumericMatrix pointDistance2(NumericMatrix to, NumericMatrix from);
RcppExport SEXP _SpaDES_tools_pointDistance2(SEXP toSEXP, SEXP fromSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type to(toSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type from(fromSEXP);
    rcpp_result_gen = Rcpp::wrap(pointDistance2(to, from));
    return rcpp_result_gen;
END_RCPP
}
// pointDistance3
NumericMatrix pointDistance3(NumericVector fromX, NumericVector toX, NumericVector fromY, NumericVector toY, double maxDistance);
RcppExport SEXP _SpaDES_tools_pointDistance3(SEXP fromXSEXP, SEXP toXSEXP, SEXP fromYSEXP, SEXP toYSEXP, SEXP maxDistanceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type fromX(fromXSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type toX(toXSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type fromY(fromYSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type toY(toYSEXP);
    Rcpp::traits::input_parameter< double >::type maxDistance(maxDistanceSEXP);
    rcpp_result_gen = Rcpp::wrap(pointDistance3(fromX, toX, fromY, toY, maxDistance));
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
    {"_SpaDES_tools_pointDistance2", (DL_FUNC) &_SpaDES_tools_pointDistance2, 2},
    {"_SpaDES_tools_pointDistance3", (DL_FUNC) &_SpaDES_tools_pointDistance3, 5},
    {"_SpaDES_tools_runifC", (DL_FUNC) &_SpaDES_tools_runifC, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_SpaDES_tools(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
