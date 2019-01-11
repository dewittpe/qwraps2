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
summary_table <- function(x, summaries = qsummary(x)) {
  UseMethod("summary_table")
}

#' @method summary_table data.frame
#' @export
summary_table.data.frame <- function(x, summaries = qsummary(x)) {

  out <-
    lapply(summaries, function(s) { lapply(s, function(y) { rlang::f_rhs(y) }) }) %>%
    lapply(function(dots) { dplyr::summarize(x, !!!(dots)) }) %>%
    lapply(t) %>%
    do.call(rbind, .)

  colnames(out) <- paste0(deparse(substitute(x), backtick = TRUE), " (N = ", frmt(nrow(x)), ")")
  attr(out, "rgroups") <- sapply(summaries, length)
  class(out) <- c("qwraps2_summary_table", class(out))
  out
}

#' @export
summary_table.grouped_df <- function(x, summaries = qsummary(x)) {

  # A workaround needs to be made while dplyr transition form version 0.7.8 to
  # 0.8.0, see issue #67

  if (!is.null(attr(x, "vars"))) {
    ngrps <- length(attr(x, "vars"))  # for dplyr version 0.7.8
    lbs <- attr(x, "labels")
    grpsz <- frmt(attr(x, "group_sizes"))
    lbs <- apply(cbind(matrix(paste(rep(names(lbs), each = nrow(lbs)), as.matrix(lbs), sep= ": "), nrow = nrow(lbs)), paste0("(N = ", grpsz, ")")), 1, paste, collapse = " ")
  } else {
    ngrps <- nrow(attr(x, "groups"))  # for dplyr version 0.7.99.9000 and beyond
    lbs <- attr(x, "groups")
    lbs <- lbs[-length(lbs)]
    grpsz <- frmt(sapply(attr(x, "groups")[[".rows"]], length))
    lbs <- apply(cbind(matrix(paste(rep(names(lbs), each = nrow(lbs)), as.matrix(lbs), sep= ": "), nrow = nrow(lbs)), paste0("(N = ", grpsz, ")")), 1, paste, collapse = " ")
  }

  out <-
    lapply(summaries, function(s) { lapply(s, function(y) { rlang::f_rhs(y) }) }) %>%
    lapply(function(dots) { dplyr::summarize(x, !!!(dots)) }) %>%
    lapply(t) %>%
    lapply(function(y) `[`(y, -1, )) %>%
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

#' @export
#' @rdname summary_table
#' @seealso \code{rbind}
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


#' Quick Variable Summaries
#'
#' Tool to quickly generate the code for summarizing the variables of a
#' \code{data.frame}.
#'
#' @param .data a \code{data.frame}
#' @param numeric_summaries a list of functions to use for summarizing numeric
#' variables.  The functions need to be provided as character strings with the
#' single argument defined by the \code{\%s} symbol.
#' @param n_perc_args a list of arguments to pass to
#' \code{\link[qwraps2]{n_perc}} to be used with \code{character} or
#' \code{factor} variables in \code{.data}.
#' @param env environment to assign to the resulting formulae
#'
#' @export
#' @rdname summary_table
qsummary <- function(.data, numeric_summaries, n_perc_args, env) {
  UseMethod("qsummary")
}

#' @export
qsummary.data.frame <- function(.data,
                                numeric_summaries =
                                  list("minimum"      = "~ qwraps2::frmt(min(%s))",
                                       "median (IQR)" = "~ qwraps2::median_iqr(%s)",
                                       "mean (sd)"    = "~ qwraps2::mean_sd(%s)",
                                       "maximum"      = "~ qwraps2::frmt(max(%s))")
                                ,
                                n_perc_args = list(digits = 0, show_symbol = FALSE)
                                ,
                                env = parent.frame()) {

  numeric_summaries <-
    lapply(numeric_summaries, function(x) as.character(x)[length(x)])
  n_perc_args_show_denom <- n_perc_args
  n_perc_args_show_denom$show_denom = "always"

  out <-
    sapply(names(.data),
           function(var) {
             if (is.numeric(.data[[var]])) {
               if (any(is.na(.data[[var]]))) {
                 rtn <- lapply(numeric_summaries, sprintf, sprintf("na.omit(.data[['%s']])", var))
                 cl <- list(quote(qwraps2::n_perc))
                 cl[[2]] <- substitute(is.na(.data[[vv]]), list(vv = var))
                 cl <- c(cl, n_perc_args_show_denom)
                 rtn <- c(rtn, Unknown = paste("~", paste(deparse(as.call(cl)), collapse = "")))
               } else {
                 rtn <- lapply(numeric_summaries, sprintf, sprintf(".data[['%s']]", var))
               }
             } else if (is.character(.data[[var]]) | is.factor(.data[[var]])) {
               .data[[var]] <- as.factor(.data[[var]])

               if (any(is.na(.data[[var]]))) {
                 rtn <-
                   sapply(levels(.data[[var]]),
                          function(l) {
                            cl <- list(quote(qwraps2::n_perc))
                            cl[[2]] <- substitute(na.omit(.data[[vv]]) == ll, list(vv = var, ll = l))
                            cl <- c(cl, n_perc_args)
                            paste("~", paste(deparse(as.call(cl)), collapse = ""))
                          },
                          simplify = FALSE)
                 cl <- list(quote(qwraps2::n_perc))
                 cl[[2]] <- substitute(is.na(.data[[vv]]), list(vv = var))
                 cl <- c(cl, n_perc_args_show_denom)
                 rtn <- c(rtn, Unknown = paste("~", paste(deparse(as.call(cl)), collapse = "")))
               } else {
                 rtn <-
                   sapply(levels(.data[[var]]),
                          function(l) {
                            cl <- list(quote(qwraps2::n_perc))
                            cl[[2]] <- substitute(.data[[vv]] == ll, list(vv = var, ll = l))
                            cl <- c(cl, n_perc_args)
                            paste("~", paste(deparse(as.call(cl)), collapse = ""))
                          },
                          simplify = FALSE)
               }

             } else if (is.logical(.data[[var]])) {

               if (any(is.na(.data[[var]]))) {
                 cl <- list(quote(qwraps2::n_perc))
                 cl[[2]] <- substitute(na.omit(.data[[vv]]), list(vv = var))
                 cl <- c(cl, n_perc_args)
                 rtn <- paste("~", paste(deparse(as.call(cl)), collapse = ""))
                 names(rtn) <- "True"

                 cl <- list(quote(qwraps2::n_perc))
                 cl[[2]] <- substitute(is.na(.data[[vv]]), list(vv = var))
                 cl <- c(cl, n_perc_args_show_denom)
                 rtn <- c(rtn, Unknown = paste("~", paste(deparse(as.call(cl)), collapse = "")))

                 rtn
               } else {
                 cl <- list(quote(qwraps2::n_perc))
                 cl[[2]] <- substitute(.data[[vv]], list(vv = var))
                 cl <- c(cl, n_perc_args)
                 rtn <- paste("~", paste(deparse(as.call(cl)), collapse = ""))
                 names(rtn) <- var
                 rtn
               }
             } else if (inherits(.data[[var]], "Date")) {
                 rtn <- lapply(list("first" = " ~ min(%s)", "last"  = " ~ max(%s)"),
                               sprintf, sprintf(".data[['%s']]", var))

             } else {
               warning(sprintf("no default method for class '%s' found in .data[['%s']]", class(.data[[var]]), var),
                       call. = FALSE)
               rtn <- NA
             }
             rtn
           },
           simplify = FALSE)
  lapply(out[!is.na(out)], function(x) lapply(x, FUN = stats::as.formula, env = env))
}

#' @export
qsummary.grouped_df <- function(.data, ...) {
  qsummary(dplyr::ungroup(.data), ...)
}


#' Tabular Summaries
#'
#' Tool to quickly generate the code for summarizing a variable.  To be used
#' with summary_table.  This function has been deprecated, see
#' \code{\link{qsummary}} instead.
#'
#' @param x a variable to summarize
#' @param n_perc_args a list of arguments to pass to \code{n_perc}
#' @param envir the environment to attach to the resulting formulea
#' @export
tab_summary <- function(x, n_perc_args = list(digits = 0, show_symbol = FALSE), envir = parent.frame()) {
  .Deprecated(new = "qsummary")
  UseMethod("tab_summary")
}

#' @export
tab_summary.numeric <- function(x, n_perc_args = list(digits = 0, show_symbol = FALSE), envir = parent.frame()) {
  v <- deparse(substitute(x), backtick = TRUE)

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
  v <- deparse(substitute(x), backtick = TRUE)

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
  v <- deparse(substitute(x), backtick = TRUE)

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

