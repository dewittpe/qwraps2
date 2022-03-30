#' @title Means and Confidence Intervals
#'
#' @description A function for calculating and formatting means and
#' confidence interval.
#'
#' @details
#' Given a numeric vector, \code{mean_ci} will return a vector with the mean,
#' LCL, and UCL.  Using \code{frmtci} will be helpful for reporting the results
#' in print.
#'
#' @param x a numeric vector
#' @param na_rm if true, omit NA values
#' @param alpha defaults to \code{getOption('qwraps2_alpha', 0.05)}.  The
#' symmetric 100(1-alpha)\% CI will be determined.
#' @param qdist defaults to \code{qnorm}.  use \code{qt} for a Student t
#' intervals.
#' @param qdist.args list of arguments passed to \code{qdist}
#' @param ... not currently used
#'
#' @return a vector with the mean, lower confidence limit (LCL), and the upper
#' confidence limit (UCL).
#'
#' @seealso \code{\link{frmtci}}
#'
#' @examples
#' # using the standard normal for the CI
#' mean_ci(mtcars$mpg)
#'
#' # print it nicely
#' qwraps2::frmtci(mean_ci(mtcars$mpg))
#' qwraps2::frmtci(mean_ci(mtcars$mpg), show_level = TRUE)
#' qwraps2::frmtci(mean_ci(mtcars$mpg, alpha = 0.01), show_level = TRUE)
#'
#' # Compare to the ci that comes form t.test
#' t.test(mtcars$mpg)
#' t.test(mtcars$mpg)$conf.int
#' mean_ci(mtcars$mpg, qdist = stats::qt, qdist.args = list(df = 31))
#'
#' @export
mean_ci <- function(x,
                    na_rm = FALSE,
                    alpha = getOption("qwraps2_alpha", 0.05),
                    qdist = stats::qnorm,
                    qdist.args = list(),
                    ...
                    ) {
  cl <- as.list(match.call())
  if ("transform" %in% names(cl)) {
    stop("the transform option has been deprecated.  Its existence made it too easy to return incorrect values.  Think about the delta method.")
  }

  if (!("na_rm" %in% names(cl)) & ("na.rm" %in% names(cl))) {
    na_rm <- cl$na.rm
    warning("qwraps2::mean_ci uses the argument `na_rm`, not `na.rm`.")
  }

  stopifnot(inherits(na_rm, "logical"))

  m <- mean(x, na.rm = na_rm)
  s <- stats::sd(x,   na.rm = na_rm)
  n <- if (na_rm) { sum(!is.na(x)) } else { length(x) }

  qd <- match.fun(qdist)

  scores <- do.call(qd, c(list(p = c(alpha/2, 1 - alpha/2)), qdist.args))

  out <- c("mean" = m, "lcl"  = m + scores[1] * s / sqrt(n),  "ucl"  = m + scores[2] * s / sqrt(n))

  attr(out, 'alpha') <- alpha
  class(out) <- c("qwraps2_mean_ci", class(out))
  out
}

#' @export
#' @param ... arguments passed to \code{frmtci}.
#' @rdname mean_ci
print.qwraps2_mean_ci <- function(x, ...) {
  print(frmtci(x, ...))
}

