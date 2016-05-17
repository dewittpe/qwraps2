#include <Rcpp.h>
#include <math.h>

//' @title logit and inverse logit functions
//' 
//' @description transform \code{x} either via the logit, or inverse logit.
//'
//' @details Why are these functions are not part of base R?  I don't have the
//' answer to that question.  So here are functions for easy of use.
//'
//' @param x a numeric vector
//' @export
//' @rdname logit
// [[Rcpp::export]]
Rcpp::NumericVector logit(Rcpp::NumericVector x) {
  //arma::vec result(x.n_elem);
  int n = x.size();
  Rcpp::NumericVector result(n);

  for(int i = 0; i < n; ++i) { 
    result[i] = log( x[i] / (1.0 - x[i]) );
  }

  return result;
}

//' @export
//' @rdname logit
// [[Rcpp::export]]
Rcpp::NumericVector invlogit(Rcpp::NumericVector x) { 
  //arma::vec result(x.n_elem);
  int n = x.size();
  Rcpp::NumericVector result(n);

  for (int i=0; i < n; ++i) { 
    result[i] = 1.0 / (1.0 + exp (-1.0 * x[i]));
  }
  return result;
}

