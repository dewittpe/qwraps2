#' mtcars2
#'
#' An extended version of \code{\link[datasets]{mtcars}} data set.
#'
#' @seealso \code{vignette("qwraps2-data-sets", package = "qwraps2")} for
#' details on the construction of the data set.
"mtcars2"

#' pefr
#'
#' Peak expiratory flow rate data
#'
#' Peak expiratory flow rate (pefr) data is used for examples within the qwraps2
#' package.  The data has been transcribed from Bland (1986).
#'
#' "The sample comprised colleagues and family of J.M.B. chosen to give a wide
#' range of PEFR but in no way representative of any defined population. Two
#' measurements were made with a Wright peak flow meter and two with a mini
#' Wright meter, in random order. All measurements were taken by J.M.B., using
#' the same two instruments. (These data were collected to demonstrate the
#' statistical method and provide no evidence on the comparability of these two
#' instruments.) We did not repeat suspect readings and took a single reading as
#' our measurement of PEFR. Only the first measurement by each method is used to
#' illustrate the comparison of methods, the second measurements being used in
#' the study of repeatability."
#'
#' @format a data frame with four columns
#'
#' [, 1] subject id number
#'
#' [, 2] measurement first or second
#'
#' [, 3] meter "Wright peak flow meter" or "Mini Write peak flow meter"
#'
#' [, 4] pefr peak expiratory flow rate (liters / min)
#'
#' @seealso \code{vignette("qwraps2-data-sets", package = "qwraps2")} for
#' details on the construction of the data set.
#'
#' @references
#'
#' Bland, J. Martin, and Douglas G Altman. "Statistical methods for assessing
#' agreement between two methods of clinical measurement." The lancet 327, no. 8476
#' (1986): 307-310.
#'
"pefr"
