#' @title Mean and Standard Error (of the mean)
#'
#' @description A function for calculating and formatting means and
#' standard deviations.
#'
#' @details
#' Given a numeric vector, \code{mean_se} will return a character string with
#' the mean and standard error of the mean.  Formatting of the output will be
#' extended in future versions.
#'
#' @param x a numeric vector
#' @param digits digits to the right of the decimal point to return in the
#' percentage estimate.
#' @param na_rm if true, omit NA values
#' @param show_n defaults to "ifNA".  Other options are "always" or "never".
#' @param denote_sd a character string set to either "pm" or "paren" for reporting 'mean
#' \eqn{\pm} sd' or 'mean (sd)'
#' @param markup latex or markdown
#' @param ... pass through
#'
#' @return a character vector of the formatted values
#'
#' @examples
#' set.seed(42)
#' x <- rnorm(1000, 3, 4)
#' mean(x)
#' sd(x) / sqrt(length(x)) # standard error
#' mean_se(x)
#' mean_se(x, show_n = "always")
#' mean_se(x, show_n = "always", denote_sd = "paren")
#'
#' x[187] <- NA
#' mean_se(x, na_rm = TRUE)
#'
#' @export
mean_se <- function(x,
                    digits = getOption("qwraps2_frmt_digits", 2),
                    na_rm = FALSE,
                    show_n = "ifNA",
                    denote_sd = "pm",
                    markup = getOption("qwraps2_markup", "latex"),
                    ...) {

  cl <- as.list(match.call())
  if (!("na_rm" %in% names(cl)) & ("na.rm" %in% names(cl))) {
    na_rm <- cl$na.rm
    warning("qwraps2::mean_sd uses the argument `na_rm`, not `na.rm`.")
  }
  stopifnot(inherits(na_rm, "logical"))
  stopifnot(length(markup) == 1L)
  stopifnot(markup %in% c("latex", "markdown"))
  stopifnot(show_n %in% c("ifNA", "always", "never"))
  stopifnot(denote_sd %in% c("pm", "paren"))

  n <- sum(!is.na(x))
  m <- mean(x, na.rm = na_rm)
  s <- stats::sd(x, na.rm = na_rm) / sqrt(n)

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

