#' A collection of wrapper functions for aiding the authoring of reproducible
#' reports.
#'
#' \pkg{qwraps2} is a collection of functions I have found very useful when
#' working on a varied collection of different analysis reports.  These
#' functions are commonly used wrappers which I have been reusing and modifying
#' over several years.
#'
#' Several wrappers for \pkg{ggplot2} style graphics, such as ROC, AUC, and KM
#' plots are provided.  Named as \code{\link{qroc}}, \code{\link{qauc}}, and
#' \code{\link{qkmplot}} to pay homage to \code{qplot} form \pkg{ggplot2} and
#' the standard names for such plots.
#'
#' Other functions are used to quickly generate meaningful character strings for
#' outputting results in .Rnw, .Rmd, or other similar functions.
#'
#' @section Options: 
#' There are several options which can be set via \code{options} and will be
#' used via \code{getOption}.  The following lists, in alphabetical order the
#' different options which are available and what they control.
#'
#' \itemize{
#'   \item \code{getOptions("qwraps2_alpha", 0.05)} significance level, used for
#'   generating \code{(1 - getOptions("qwraps2_alpha", 0.05)) * 100}\% confidence
#'   intervals, and determining significance for p-value <
#'   \code{getOptions("qwraps2_alpha", 0.05)}.
#'   \item \code{getOptions("qwraps2_frmtp_digits", 2)}  Number of digits to the
#'     right of the decimal point to report p-values too.  If 
#'     \code{log10(p-value) < getOptions("qwraps2_frmtp_digits", 2)} then the
#'     output will be "P < 0.01", to however many digits are correct.  Other
#'     options control other parts of the output p-value format.
#'
#'   \item \code{getOptions("qwraps2_style", "default")} By setting this option
#'     to a specific journal, p-values and other output, will be formatted to
#'     meet journal requirements.
#' }
#' @docType package
#' @name qwraps2
NULL
