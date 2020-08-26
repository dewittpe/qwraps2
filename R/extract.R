#' @title Extract Summary stats from regression objects
#'
#' @description A collection of functions for extracting summary statistics and
#' reporting regression results from \code{lm}, \code{glm} and other regression
#' objects.
#'
#' @seealso
#' \code{\link{lm}} 
#'
#' @param x a \code{lm} object
#'
#' @return a character vector of the formatted numbers
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp + drat, data = mtcars)
#' summary(fit)
#' extract_fstat(fit)
#' extract_fpvalue(fit)
#'
#' @export   
#' @rdname extract
extract_fstat <- function(x) {
  UseMethod("extract_fstat")
}

#' @export
extract_fstat.lm <- function(x) {
  fstat <- summary(x)$fstatistic
  paste0("$F_{", 
         frmt(as.integer(fstat[2L])), ", ", 
         frmt(as.integer(fstat[3L])), "} = ",
         frmt(fstat[1L]), "$")
}

#' @export
#' @return formatted p-value from the F-test
#' @rdname extract
extract_fpvalue <- function(x) { 
  UseMethod("extract_fpvalue")
}

#' @export
#' @rdname extract
extract_fpvalue.lm <- function(x) { 
  fstat <- summary(x)$fstatistic
  frmtp(stats::pf(fstat[1L], fstat[2L], fstat[3L], lower.tail = FALSE))
}

