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
#' the whole data set and for a grouped data set.  When working through the
#' example pay attention to the use of \code{\link[dplyr]{group_by}} and
#' \code{\link[dplyr]{ungroup}} from \code{dplyr}.
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
#' @example examples/summary_table.R
#'
#' @export
#' @rdname summary_table
summary_table <- function(x, summaries) {
  UseMethod("summary_table")
}

#' @method summary_table data.frame
#' @export
summary_table.data.frame <- function(x, summaries) {

  out <- 
    lapply(summaries, function(s) { lapply(s, function(y) { rlang::f_rhs(y) }) }) %>%
    lapply(function(dots) { dplyr::summarize(x, rlang::UQS(dots)) }) %>%
    lapply(t) %>%
    do.call(rbind, .) 

  colnames(out) <- paste0(deparse(substitute(x)), " (N = ", nrow(x), ")")
  attr(out, "rgroups") <- sapply(summaries, length)
  class(out) <- c("qwraps2_summary_table", class(out))
  out
}

#' @export
summary_table.grouped_df <- function(x, summaries) {
  ngrps <- length(attr(x, "vars"))

  lbs <- attr(x, "labels")
  grpsz <- attr(x, "group_sizes")
    
  lbs <- apply(cbind(matrix(paste(rep(names(lbs), each = nrow(lbs)), as.matrix(lbs), sep= ": "), nrow = nrow(lbs)), paste0("(N = ", grpsz, ")")), 1, paste, collapse = " ")
                       
  out <- 
    lapply(summaries, function(s) { lapply(s, function(y) { rlang::f_rhs(y) }) }) %>%
    lapply(function(dots) { dplyr::summarize(x, rlang::UQS(dots)) }) %>%
    lapply(t) %>%
    lapply(function(y) `[`(y, -seq(1, ngrps, by = 1), )) %>%
    do.call(rbind, .)

  colnames(out) <- lbs
  rownames(out) <- unlist(lapply(summaries, names), use.names = FALSE)

  attr(out, "rgroups") <- sapply(summaries, length)
  class(out) <- c("qwraps2_summary_table", class(out))

  out 
}

#' @export
print.qwraps2_summary_table <- function(x, rgroup = attr(x, "rgroups"), rnames = rownames(x), cnames = colnames(x), ...) { 
  print(qable(x, rgroup = rgroup, rnames = rnames, cnames = cnames, ...))
}

#' @export
#' @rdname summary_table
#' @param ... \code{qwraps2_summary_table} objects to bind together
#' @param deparse.level integer controlling the construction of labels in the
#' case of non-matrix-like arguments (for the default method): \code{deparse.level =
#' 0} constructs no labels; the default, \code{deparse.level = 1} or
#' \code{deparse.level = 2} constructs labels from the argument names.
#' @seealso \code{cbind} 
cbind.qwraps2_summary_table <- function(..., deparse.level = 1) {
  tabs <- list(...)
  
  out <- do.call(cbind, args = c(lapply(tabs, unclass), list(deparse.level = deparse.level)))

  attr(out, "rgroups") <- attr(tabs[[1]], "rgroups")
  class(out) <- class(tabs[[1]])

  out
}





#' Tabular Summaries
#' 
#' Tool to quickly generate the code for summarizing a variable.  To be used
#' with summary_table.
#' 
#' @param x a variable to summarize
#' @param n_perc_args a list of arguments to pass to \code{n_perc}
#' @param envir the environment to attach to the resulting formulea
#' @export
tab_summary <- function(x, n_perc_args = list(digits = 0, show_symbol = FALSE), envir = parent.frame()) {
  UseMethod("tab_summary")
}

#' @export
tab_summary.numeric <- function(x, n_perc_args = list(digits = 0, show_symbol = FALSE), envir = parent.frame()) {
  v <- deparse(substitute(x))

  if (length(n_perc_args)) { 
    n_args <- paste(", ", paste(paste(names(n_perc_args), lapply(n_perc_args, function(x) if (is.character(x)) paste0("'", x, "'") else x), sep = " = "), collapse = ", "))
  } else {
    n_args <- ""
  }

  if (any(is.na(x))) {
    s <- list("min"          = paste("~ min(", v, ", na.rm = TRUE)"),
              "median (IQR)" = paste("~ qwraps2::median_iqr(", v, ", na_rm = TRUE)"),
              "mean (sd)"    = paste("~ qwraps2::mean_sd(", v, ", na_rm = TRUE)"),
              "max"          = paste("~ max(", v, ", na.rm = TRUE)"))

    s <- c(s, Unknown = paste(" ~ qwraps2::n_perc(is.na(", v, ")", n_args, ")"))

  } else {
    s <- list("min"          = paste("~ min(", v, ")"),
              "median (IQR)" = paste("~ qwraps2::median_iqr(", v, ")"),
              "mean (sd)"    = paste("~ qwraps2::mean_sd(", v, ")"),
              "max"          = paste("~ max(", v, ")"))
  } 
  lapply(s, stats::as.formula, env = envir)
}

#' @export
tab_summary.character <- function(x, n_perc_args = list(digits = 0, show_symbol = FALSE), envir = parent.frame()) {
  v <- deparse(substitute(x)) 

  if (length(n_perc_args)) { 
    n_args <- paste(", ", paste(paste(names(n_perc_args), lapply(n_perc_args, function(x) if (is.character(x)) paste0("'", x, "'") else x), sep = " = "), collapse = ", "))
  } else {
    n_args <- ""
  }

  if (any(is.na(x))) {
    x <- stats::na.omit(x)
    s <- lapply(sort(unique(x)), 
                function(xx) {
                  paste0("~ qwraps2::n_perc(", v, " == '", xx, "'", n_args, ", na_rm = TRUE)")
                })
    s <- c(s, paste(" ~ qwraps2::n_perc(is.na(", v, ")", n_args, ")"))
    s <- stats::setNames(s, c(sort(unique(x)), "Unknown"))
  } else {
    s <- lapply(sort(unique(x)), 
                function(xx) {
                  paste0("~ qwraps2::n_perc(", v, " == '", xx, "'", n_args, ")")
                })
    s <- stats::setNames(s, sort(unique(x)))
  } 
  lapply(s, stats::as.formula, env = envir)
}

#' @export
tab_summary.factor <- function(x, n_perc_args = list(digits = 0, show_symbol = FALSE), envir = parent.frame()) {
  v <- deparse(substitute(x))

  if (length(n_perc_args)) { 
    n_args <- paste(", ", paste(paste(names(n_perc_args), lapply(n_perc_args, function(x) if (is.character(x)) paste0("'", x, "'") else x), sep = " = "), collapse = ", "))
  } else {
    n_args <- ""
  }

  if (any(is.na(x))) {
    s <- lapply(levels(x),
                function(xx) {
                  paste0("~ qwraps2::n_perc(", v, " == '", xx, "'", n_args, ", na_rm = TRUE)")
                })
    s <- c(s, paste(" ~ qwraps2::n_perc(is.na(", v, ")", n_args, ")"))
    s <- stats::setNames(s, c(as.character(sort(unique(x))), "Unknown"))
  } else {
    s <- lapply(levels(x),
                function(xx) {
                  paste0("~ qwraps2::n_perc(", v, " == '", xx, "'", n_args, ")")
                })
    s <- stats::setNames(s, sort(unique(x)))
  } 
  lapply(s, stats::as.formula, env = envir)
}

