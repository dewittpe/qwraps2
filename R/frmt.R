#' @title Format 
#'
#' @description A function for formating Numeric values for consistent display
#' in reports.
#'
#' @details
#' This function is really just a wrapper for the \code{formatC}.
#'
#' @seealso
#' \code{\link{frmtp}} \code{\link{formatC}}
#'
#' @param x a vector of numbers to format
#' @param digits number of digits, including trailing zeros, to the right of the
#' decimal point.  This option is ignored if \code{is.integer(x) == TRUE)}.
#'
#' @return a character vector of the formatted numbers
#'
#' @examples
#' 
#' integers <- c(1234L, 9861230L)
#' numbers  <- c(1234,  9861230)
#' frmt(integers)  # no decimal point
#' frmt(numbers)   # decimal point and zeros to the right
#' 
#' numbers <- c(0.1234, 0.1, 1234.4321, 0.365, 0.375)
#' frmt(numbers)
#'
#' @export   
frmt <- function(x, digits = getOption("qwraps2_frmt_digits", 2)) { 
  sapply(x, 
         function(xx) { 
           if (is.integer(xx)) { 
             formatC(xx, format = "d", big.mark = ",")
           } else { 
             formatC(xx, digits = digits, format = "f", big.mark = ",")
           }
         })
}

