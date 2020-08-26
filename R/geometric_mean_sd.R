#' Geometric Mean, Variance, and Standard Deviation
#'
#' Return the geometric mean, variance, and standard deviation,
#'
#' @param x a numeric vector
#' @param na_rm a logical value indicating whether \code{NA} values should be
#'   stripped before the computation proceeds.
#' @return a numeric value
#'
#' @seealso \code{\link{gmean_sd}} for easy formatting of the geometric mean and
#' standard deviation.  \code{vignette("summary-statistics", package =
#' "qwraps2")}.
#'
#' @examples
#'
#' set.seed(42)
#' x <- runif(14, min = 4, max = 70)
#'
#' # geometric mean - four equivalent ways to get the same result
#' prod(x) ^ (1 / length(x))
#' exp(mean(log(x)))
#' 1.2 ^ mean(log(x, base = 1.2))
#' gmean(x)
#'
#' # geometric variance
#' gvar(x)
#'
#' # geometric sd
#' exp(sd(log(x)))                                     ## This is wrong (incorrect sample size)
#' exp(sqrt((length(x) - 1) / length(x)) * sd(log(x))) ## Correct calculation
#' gsd(x)
#'
#' # Missing data will result in and NA being returned
#' x[c(2, 4, 7)] <- NA
#' gmean(x)
#' gmean(x, na_rm = TRUE)
#' gvar(x, na_rm = TRUE)
#' gsd(x, na_rm = TRUE)
#'
#' @name geometric_mean_var_sd
NULL

#' @rdname geometric_mean_var_sd
#' @export
gmean <- function(x, na_rm = FALSE) {
  if (na_rm) {
    x <- stats::na.omit(x)
  }
  exp(mean(log(x)))
}

#' @rdname geometric_mean_var_sd
#' @export
gvar <- function(x, na_rm = FALSE) {
  if (na_rm) {
    x <- stats::na.omit(x)
  }
  exp((length(x) - 1) / length(x) * stats::var(log(x)))
}

#' @rdname geometric_mean_var_sd
#' @export
gsd <- function(x, na_rm = FALSE) {
  if (na_rm) {
    x <- stats::na.omit(x)
  }
  exp(sqrt((length(x) - 1) / length(x) * stats::var(log(x))))
}
