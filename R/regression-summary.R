#' @title Extract Summary stats from regression objects
#'
#' @description A collectin of functions for extracting summary statistics and
#' reporting regression results from \code{lm}, \code{glm} and other regression
#' objects.
#'
#' @details
#' TO DO
#'
#' @seealso
#' \code{\link{lm}} 
#'
#' @param x a \code{lm} object
#'
#' @return a character vector of the formatted numbers
#'
#' @examples
#' # TODO
#'
#' @export   
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
extract_fpvalue <- function(x) { 
  UseMethod("extract_fpvalue")
}

#' @export
extract_fpvalue.lm <- function(x) { 
  fstat <- summary(x)$fstatistic
  frmtp(pf(fstat[1L], fstat[2L], fstat[3L], lower.tail = FALSE))
}
extract_fpvalue.lm(fit) 

