// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>

//' @title Trapezoid Rule Numeric Integration
//'
//' @description Compute the integral of y with respect to x via trapezoid rule.
//'
//' @param x,y numeric vectors of equal length
//'
//' @return a numeric value, the estimated integral
//'
//' @example examples/traprule.R
//'
//' @export
// [[Rcpp::export]]
double traprule(arma::vec x, arma::vec y) { 
  return arma::sum((x.subvec(1, x.n_elem - 1) - x.subvec(0, x.n_elem - 2)) % 
                   (y.subvec(1, y.n_elem - 1) + y.subvec(0, y.n_elem - 2))) * 0.5;
}

