#' mtcars2
#'
#' An extended version of \code{\link[datasets]{mtcars}} data set.
#'
#' @format a data.frame with 32 rows and 19 columns
#'
#' \tabular{rlll}{
#' [,  1] \tab make          \tab Manufacturer name                        \tab parted out from \code{rownames(mtcars)} \cr
#' [,  2] \tab model         \tab                                          \tab parted out from \code{rownames(mtcars)} \cr
#' [,  3] \tab mpg           \tab miles per (US) gallon                    \tab identical to mtcars$mpg \cr
#' [,  4] \tab disp          \tab Displacement (cu.in.)                    \tab identical to mtcars$disp \cr
#' [,  5] \tab hp            \tab Gross horsepower                         \tab identical to mtcars$hp \cr
#' [,  6] \tab drat          \tab Rear axle ratio                          \tab identical to mtcars$drat \cr
#' [,  7] \tab wt            \tab weight (1000 lbs)                        \tab identical to mtcars$wt \cr
#' [,  8] \tab qsec          \tab 1/4 mile time                            \tab identical to mtcars$qsec \cr
#' [,  9] \tab cyl           \tab number of cylinders                      \tab identical to mtcars$cyl \cr
#' [, 10] \tab cyl_character \tab                                          \tab \cr
#' [, 11] \tab cyl_factor    \tab                                          \tab \cr
#' [, 12] \tab vs            \tab Engine (0 = V-shaped, 1 = straight)      \tab identical to mtcars$vs \cr
#' [, 13] \tab engine        \tab                                          \tab \cr
#' [, 14] \tab am            \tab Transmission (0 = automatic, 1 = manual) \tab identical to mtcars$am \cr
#' [, 15] \tab transmission  \tab                                          \tab \cr
#' [, 16] \tab gear          \tab Number of forward gears                  \tab identical to mtcars$gear \cr
#' [, 17] \tab gear_factor   \tab                                          \tab \cr
#' [, 18] \tab carb          \tab Number of carburetors                    \tab identical to mtcars$carb \cr
#' [, 19] \tab test_date     \tab fictitious testing date                  \tab \cr
#' }
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
#' \dQuote{The sample comprised colleagues and family of J.M.B. chosen to give a
#' wide range of PEFR but in no way representative of any defined population.
#' Two measurements were made with a Wright peak flow meter and two with a mini
#' Wright meter, in random order. All measurements were taken by J.M.B., using
#' the same two instruments. (These data were collected to demonstrate the
#' statistical method and provide no evidence on the comparability of these two
#' instruments.) We did not repeat suspect readings and took a single reading as
#' our measurement of PEFR. Only the first measurement by each method is used to
#' illustrate the comparison of methods, the second measurements being used in
#' the study of repeatability.}
#'
#' @format a data frame with four columns
#' \tabular{rll}{
#' [, 1] \tab subject     \tab id number \cr
#' [, 2] \tab measurement \tab first or second \cr
#' [, 3] \tab meter       \tab \dQuote{Wright peak flow meter} or \dQuote{Mini Write peak flow meter} \cr
#' [, 4] \tab pefr        \tab peak expiratory flow rate (liters / min) \cr
#' }
#'
#' @seealso \code{vignette('qwraps2-data-sets', package = 'qwraps2')} for
#' details on the construction of the data set.
#'
#' @references
#'
#' Bland, J. Martin, and Douglas G Altman. "Statistical methods for assessing
#' agreement between two methods of clinical measurement." The lancet 327, no. 8476
#' (1986): 307-310.
#'
"pefr"

#' Spambase
#'
#' Classifying Email as Spam or Non-Spam
#'
#' @format a data.frame with 4601 rows, 58 columns; 57 features and 0/1
#' indicator for spam
#'
#' Used under CC BY 4.0 license.
#'
#' @seealso \code{vignette("qwraps2-data-sets", package = "qwraps2")} for
#' details on the construction of the data set.
#'
#' @references
#' Hopkins,Mark, Reeber,Erik, Forman,George, and Suermondt,Jaap. (1999). Spambase. UCI Machine Learning Repository. https://doi.org/10.24432/C53G6X.
#'
"spambase"

