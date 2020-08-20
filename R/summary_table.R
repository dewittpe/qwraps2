#' Data Summary Tables
#'
#' Tools useful for building data summary tables.
#'
#' \code{summary_table} can be used to generate good looking, simple tabels in
#' LaTeX or markdown.  Functions like xtables::print.xtable and Hmisc::latex
#' provide many more tools for formating tables.  The purpose of
#' \code{summary_table} is to generate good looking tables quickly within
#' workflow for summarizing a data set.
#'
#' Creating a list-of-lists of summary functions to apply to a data set will
#' allow the exploration of the whole data set and grouped data sets.  In the
#' example provided on this page we see a set of summary measures for the
#' \code{\link[datasets]{mtcars}} data set and the construction of a table for
#' the whole data set and for a grouped data set.
#'
#'
#' The list-of-lists should be thought of as follows:  the outer list defines
#' row groups, the inner lists define the rows within each row group.
#'
#' More detailed use of these functions can be found the "summary-statistics"
#' vignette.
#'
#' The \code{print} method for the \code{qwraps2_summary_table} objects is just
#' a simple wrapper for \code{\link{qable}}.
#'
#' @param x a \code{data.frame} or \code{grouped_df}.
#' @param summaries a list of lists of formulea for summarizing the data set.
#' See Details and examples.
#'
#' @seealso \code{\link{qable}} for marking up \code{qwraps2_data_summary}
#' objects.  \code{\link[dplyr]{group_by}} for \code{\link[dplyr]{grouped_df}}
#' objects.  The \code{vignette("summary-statistics", package = "qwraps2")} for
#' detailed use of these functions and cavets.
#'
#' @return a \code{qwraps2_summary_table} object.
#'
#'
#' @export
#' @rdname summary_table
summary_table <- function(x, summaries = qsummary(x)) {
  UseMethod("summary_table")
}

#' @export
summary_table.default <- function(x, summaries = qsummary(x)) {

  if (!missing(summaries)) {
    if ( any(grepl("\\.data\\$", parse(text = summaries))) ) {
      warning("Use of the data pronoun is no longer required/encouraged.  The
              ability to use it has been deprecated.  See the documentation for
              summary_table, qsummary, and the vignettes for more detail.  The
              use of the data pronoun will be supported in version 0.5.0 of
              qwraps2 with this warning.  Eventually an error will be thrown
              before support is removed from the package completely.")
      summary_table_042(x, summaries = summaries)
    }
  }

  return("I'm the king!")

}

#' @rdname summary_table
#' @export
qsummary <- function(x, ...) {
  qsummary_042(x, ...)
}


#' @rdname summary_table
#' @export
#' @param ... \code{qwraps2_summary_table} objects to bind together
#' @param deparse.level integer controlling the construction of labels in the
#' case of non-matrix-like arguments (for the default method): \code{deparse.level =
#' 0} constructs no labels; the default, \code{deparse.level = 1} or
#' \code{deparse.level = 2} constructs labels from the argument names.
#' @seealso \code{cbind}
cbind.qwraps2_summary_table <- function(..., deparse.level = 1) {
  tabs <- list(...)

  for(i in seq_along(tabs)[-1]) {
    if (inherits(tabs[[i-1]], "qwraps2_summary_table") & inherits(tabs[[i]], "qwraps2_summary_table")) {
      if (!identical(attr(tabs[[i-1]], "rgroups"), attr(tabs[[i]], "rgroups") ) ) {
        stop("Not all row groups are identical.")
      }

      if (!identical(rownames(tabs[[i-1]]), rownames(tabs[[i]]))) {
        stop("Not all rownames are identical.")
      }
    }
  }

  out <- do.call(cbind, args = c(lapply(tabs, unclass), list(deparse.level = deparse.level)))

  attr(out, "rgroups") <- attr(tabs[[1]], "rgroups")
  class(out) <- class(tabs[[1]])

  out
}

#' @seealso \code{rbind}
#' @rdname summary_table
#' @export
rbind.qwraps2_summary_table <- function(..., deparse.level = 1) {
  tabs <- list(...)

  for(i in seq_along(tabs)[-1]) {
    if (inherits(tabs[[i-1]], "qwraps2_summary_table") & inherits(tabs[[i]], "qwraps2_summary_table")) {
      if (!identical(colnames(tabs[[i-1]]), colnames(tabs[[i]]))) {
        stop("Not all colnames are identical.")
      }
    }
  }

  out <- do.call(rbind, args = c(lapply(tabs, unclass), list(deparse.level = deparse.level)))

  attr(out, "rgroups") <- do.call(c, lapply(tabs, attr, "rgroups"))
  class(out) <- class(tabs[[1]])

  out
}
