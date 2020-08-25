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
#' @param by a character vector of variable names to generate the summary by,
#' that is one column for each unique values of the variables specified.
#'
#' @seealso \code{\link{qsummary}} for generating the summaries,
#' \code{\link{qable}} for marking up \code{qwraps2_data_summary} objects.
#' \code{\link[dplyr]{group_by}} for \code{\link[dplyr]{grouped_df}} objects.
#' The \code{vignette("summary-statistics", package = "qwraps2")} for detailed
#' use of these functions and cavets.
#'
#' @return a \code{qwraps2_summary_table} object.
#'
#'
#' @export
#' @rdname summary_table
summary_table <- function(x, summaries = qsummary(x), by = NULL) {
  UseMethod("summary_table")
}

#' @export
summary_table.grouped_df <- function(x, summaries = qsummary(x), by = NULL) {
  if (!is.null(by)) {
    warning("You've passed a grouped_df to summary_table and specified the by argument.  The by argument will be ignored.")
  }

  # this assumes dplyr version 0.8.0 or newer
  lbs <- names(attr(x, "groups"))
  lbs <- lbs[-length(lbs)]
  NextMethod(object = x, by = lbs)
}
#' @export
summary_table.data.frame <- function(x, summaries = qsummary(x), by = NULL) {

  if (!missing(summaries)) {
    if ( any(grepl("\\.data\\$", parse(text = summaries))) ) {
      warning("Use of the data pronoun is no longer required/encouraged.  The
              ability to use it has been deprecated.  See the documentation for
              summary_table, qsummary, and the vignettes for more detail.  The
              use of the data pronoun will be supported in version 0.5.0 of
              qwraps2 with this warning.  Eventually an error will be thrown
              before support is removed from the package completely.")
      return(summary_table_042(x, summaries = summaries))
    }
  }

  if (!is.null(by)) {
    subsets <- split(x, interaction(x[, by]))
  } else {
    subsets <- list(x)
  }

  rtn <- lapply(subsets, apply_summaries, summaries = summaries)
  if (length(rtn) > 1L) {
    cn <- paste0(names(rtn), " (N = ", sapply(rtn, attr, "n"), ")")
    rtn <- do.call(cbind, rtn)
    colnames(rtn) <- cn
  } else {
    rtn <- rtn[[1]]
    colnames(rtn) <- paste0(deparse(substitute(x), nlines = 1L, backtick = TRUE), " (N = ", frmt(nrow(x)), ")")
  }

  rtn
}

apply_summaries <- function(summaries, x) {
  rtn <- lapply(summaries, lapply, stats::model.frame, x)
  rtn <- lapply(rtn, lapply, function(y) {attr(y, "terms") <- NULL; y})
  rtn <- lapply(rtn, lapply, unlist)
  rtn <- lapply(rtn, lapply, function(y) {attr(y, "names") <- NULL; y})
  rtn <- lapply(rtn, unlist)
  rtn <- lapply(rtn, as.matrix, ncol = 1)

  rgroups <- sapply(rtn, nrow)

  rtn <- do.call(rbind, rtn)
  attr(rtn, "rgroups") <- rgroups
  attr(rtn, "n") <- nrow(x)
  class(rtn) <- c("qwraps2_summary_table", class(rtn))
  rtn
}


#' @param numeric_summaries a list of functions to use for summarizing numeric
#' variables.  The functions need to be provided as character strings with the
#' single argument defined by the \code{\%s} symbol.
#' @param n_perc_args a list of arguments to pass to
#' \code{\link[qwraps2]{n_perc}} to be used with \code{character} or
#' \code{factor} variables in \code{.data}.
#' @param env environment to assign to the resulting formulae

#' @rdname summary_table
#' @export
qsummary <- function(x, numeric_summaries, n_perc_args, env = parent.frame()) {
  UseMethod("qsummary")
}

#' @export
qsummary.grouped_df <- function(x, numeric_summaries, n_perc_args, env = parent.frame()) {
  NextMethod()
}

#' @export
qsummary.data.frame <- function(x,
                                numeric_summaries =
                                  list("minimum"      = "~ qwraps2::frmt(min(%s))",
                                       "median (IQR)" = "~ qwraps2::median_iqr(%s)",
                                       "mean (sd)"    = "~ qwraps2::mean_sd(%s)",
                                       "maximum"      = "~ qwraps2::frmt(max(%s))"),
                                n_perc_args = list(digits = 0, show_symbol = FALSE)
                                ,
                                env = parent.frame()) {

  npa <- lapply(n_perc_args, deparse)
  npa <- paste(paste(names(npa), npa, sep = " = "), collapse = ", ")

  rtn <-
    lapply(names(x),
           function(variable) {
             if (is.numeric(x[[variable]])) {
               summaries <- numeric_summaries
             } else if (is.character(x[[variable]]) | is.factor(x[[variable]])) {
               lvls <- levels(as.factor(x[[variable]]))
               summaries <- as.list(paste0("~ qwraps2::n_perc(%s == '", lvls, "', ", npa, ")"))
               summaries <- stats::setNames(summaries, lvls)
             } else if (is.logical(x[[variable]])) {
               summaries <- as.list(paste0("~ qwraps2::n_perc(%s, ", npa, ")"))
             } else if (inherits(x[[variable]], "Date")) {
               summaries <- list("first" = " ~ as.character(min(%s))", "last" = "~ as.character(max(%s))")
             } else {
               warning(sprintf("no default summary method for class '%s'", class(x[[variable]])))
               return(NULL)
             }

             if (any(is.na(x[[variable]]))) {
               summaries <- lapply(summaries, sprintf, "na.omit(%s)")
               summaries <- c(summaries, list("Unknown/Missing" = "~ qwraps2::n_perc(is.na(%s))"))
             }

             if (make.names(variable) == variable) {
               lapply(summaries, sprintf, variable)
             } else {
               lapply(summaries, sprintf, paste0("`", variable, "`"))
             }

           })
  rtn <- stats::setNames(rtn, names(x))

  rtn <- lapply(rtn, lapply, stats::as.formula, env = env)

  # if there is a label for the column use that as the name for the summary
  labs <- lapply(x, attr, "label")
  if (!all(sapply(labs, is.null))) {
    labs_i <- which(!sapply(labs, is.null))
    names(rtn)[labs_i] <- labs[labs_i]
  }

  rtn
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

#' @export
print.qwraps2_summary_table <- function(x, rgroup = attr(x, "rgroups"), rnames = rownames(x), cnames = colnames(x), ...) {
  print(qable(x, rgroup = rgroup, rnames = rnames, cnames = cnames, ...))
}
