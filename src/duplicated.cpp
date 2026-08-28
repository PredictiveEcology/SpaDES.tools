//Includes/namespaces
#include <Rcpp.h>
using namespace Rcpp;

//' @title
//' Rcpp duplicated on integers using Rcpp Sugar
//' @description
//' `.duplicatedInt` does same as `duplicated` in R, but only on integers, and faster.
//' It uses Rcpp sugar
//'
//' @param x Integer Vector
//' @return
//' A logical vector, as per `duplicated`
//'
//' @rdname duplicated
// [[Rcpp::export]]
LogicalVector duplicatedInt(IntegerVector x) {
  return duplicated(x);
}

