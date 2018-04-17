#' Geometric Mean, Variance, and Standard Deviation
#'
#' Return the geometric mean, variance, and standard deviation,
#'
#' @param x a numeric vector
#' @param na_rm a logical value indicating whether \code{NA} values should be
#'   stripped before the computation proceeds.
#' @return a numeric value
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
