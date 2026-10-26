//Includes/namespaces
#include <Rcpp.h>
using namespace Rcpp;

//' Are all values in `[0, 1]`, ignoring `NA`?
//'
//' @description
//' The question `spread()` asks of `spreadProb` before it starts. Expressed in R
//' as `all(inRange(na.omit(x)))` it costs three passes and two copies of the
//' whole vector -- `na.omit()` allocates one, the comparison another -- which is
//' work proportional to the size of the landscape on every call, however little
//' of it burns. This is one pass, no allocation, and it stops at the first value
//' that fails.
//'
//' `NA` and `NaN` are ignored, so an all-`NA` vector and a zero-length vector are
//' both `TRUE`: there is nothing in them that is out of range. That matches
//' `all(inRange(na.omit(x)))`, which is `all(logical(0))`.
//'
//' @param x Numeric vector.
//' @return A single logical.
//' @export
//' @rdname inRange01
// [[Rcpp::export]]
bool allInRange01(NumericVector x) {
  R_xlen_t n = x.size();
  const double *p = REAL(x);        // no bounds-checked proxy in the hot loop
  for (R_xlen_t i = 0; i < n; ++i) {
    const double v = p[i];
    // In-range is the common case, and every comparison against NaN is false,
    // so this reaches the NaN test only for values that already failed.
    if (v >= 0.0 && v <= 1.0) continue;
    if (ISNAN(v)) continue;         // NA and NaN alike
    return false;
  }
  return true;
}
