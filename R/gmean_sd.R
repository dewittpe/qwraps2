#' @title Geometric Mean and Standard deviation
#'
#' @description A function for calculating and formatting geometric means and
#' standard deviations.
#'
#' @details
#' Given a numeric vector, \code{gmean_sd} will return a character string with
#' the geometric mean and standard deviation.  Formatting of the output will be
#' extended in future versions.
#'
#' @param x a numeric vector
#' @param digits digits to the right of the decimal point to return in the
#' percentage estimate.
#' @param na_rm if true, omit NA values
#' @param show_n defaults to \dQuote{ifNA}.  Other options are \dQuote{always} or
#' \dQuote{never}.
#' @param denote_sd a character string set to either "pm" or "paren" for reporting 'mean
#' \eqn{\pm} sd' or 'mean (sd)'
#' @param markup character string with value \dQuote{latex} or \dQuote{markdown}
#' @param ... pass through
#'
#' @seealso \code{\link{mean_sd}}, \code{\link{gmean}}, \code{\link{gsd}}
#'
#' @return a character vector of the formatted values
#'
#' @examples
#'
#' gmean_sd(mtcars$mpg, markup = "latex")
#' gmean_sd(mtcars$mpg, markup = "markdown")
#'
#' @export
gmean_sd <- function(x,
                     digits = getOption("qwraps2_frmt_digits", 2),
                     na_rm = FALSE,
                     show_n = "ifNA",
                     denote_sd = "pm",
                     markup = getOption("qwraps2_markup", "latex"),
                     ...) {

  cl <- as.list(match.call())
  if ("na.rm" %in% names(cl)) {
    if ("na_rm" %in% names(cl)) {
      warning("qwraps2::gmean_sd uses the argument `na_rm`, not `na.rm`. `na_rm` is used preferentially.")
    } else {
      na_rm <- cl$na.rm
      warning("qwraps2::gmean_sd uses the argument `na_rm`, not `na.rm`.")
    }
  }
  stopifnot(inherits(na_rm, "logical"))
  stopifnot(length(markup) == 1L)
  stopifnot(markup %in% c("latex", "markdown"))
  stopifnot(show_n %in% c("ifNA", "always", "never"))

  n <- sum(!is.na(x))

  m <- gmean(x, na_rm)
  s <- gsd(x, na_rm)

  if (show_n == "always" | (show_n == "ifNA" & any(is.na(x)))) {
    rtn <- paste0(frmt(as.integer(n), digits), "; ", frmt(m, digits), " $\\pm$ ", frmt(s, digits))
  } else {
    rtn <- paste0(frmt(m, digits), " $\\pm$ ", frmt(s, digits))
  }

  if (denote_sd == "paren") {
    rtn <- gsub("\\$\\\\pm\\$\\s(.*)", "\\(\\1\\)", rtn)
  }

  if (markup == "markdown") {
    rtn <- gsub("\\$\\\\pm\\$", "&plusmn;", rtn)
  }

  return(rtn)
}

