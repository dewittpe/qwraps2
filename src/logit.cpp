#include <Rcpp.h>
#include <math.h>

//' @title logit and inverse logit functions
//'
//' @description
//' transform \code{x} either via the logit, or inverse logit.
//'
//' @details
//' The logit and inverse logit functions are part of R via the
//' logistic distribution functions in the stats package.
//' Quoting from the documentation for the logistic distribution
//'
//' "\code{qlogis(p)} is the same as the \code{logit} function, \code{logit(p) =
//' log(p/1-p)}, and \code{plogis(x)} has consequently been called the 'inverse
//' logit'."
//'
//' See the examples for benchmarking these functions.  The \code{logit} and
//' \code{invlogit} functions are faster than the \code{qlogis} and \code{plogis}
//' functions.
//'
//' @seealso \code{\link[stats:Logistic]{qlogis}}
//'
//' @examples
//' library(rbenchmark)
//'
//' # compare logit to qlogis
//' p <- runif(1e5)
//' identical(logit(p), qlogis(p))
//'
//' \dontrun{
//' rbenchmark::benchmark(logit(p), qlogis(p))
//' }
//'
//' # compare invlogit to plogis
//' x <- runif(1e5, -1000, 1000)
//' identical(invlogit(x), plogis(x))
//'
//' \dontrun{
//' rbenchmark::benchmark(invlogit(x), plogis(x))
//' }
//'
//' @param x a numeric vector
//' @export
//' @rdname logit
// [[Rcpp::export]]
Rcpp::NumericVector logit(Rcpp::NumericVector x) {
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
  int n = x.size();
  Rcpp::NumericVector result(n);

  for (int i=0; i < n; ++i) {
    result[i] = 1.0 / (1.0 + exp (-1.0 * x[i]));
  }
  return result;
}

