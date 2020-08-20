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
//' @examples
//' xvec <- seq(-2 * pi, 3 * pi, length = 560)
//' foo  <- function(x) { sin(x) + x * cos(x) + 12 }
//' yvec <- foo(xvec)
//' plot(xvec, yvec, type = "l")
//'
//' integrate(f = foo, lower = -2 * pi, upper = 3 * pi)
//' traprule(xvec, yvec)
//'
//' @export
// [[Rcpp::export]]
double traprule(arma::vec x, arma::vec y) {
  return arma::sum((x.subvec(1, x.n_elem - 1) - x.subvec(0, x.n_elem - 2)) %
                   (y.subvec(1, y.n_elem - 1) + y.subvec(0, y.n_elem - 2))) * 0.5;
}

