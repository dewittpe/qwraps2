#' Deprecated functions
#'
#' Functions listed here are deprecated.  Replacement methods have been
#' developed to replace these methods.
#'
#' \code{summary_table_042} and \code{qsummary_042} have been redesigned with some
#' changes to the api for version 0.5.0 of qwraps2.  The version released
#' up through version 0.4.2 have been placed here with the appended _042 on the
#' function names.  This will will for soft deprecation in 0.5.0 (warning), hard
#' deprecation in later (error), and eventual removal from the package.
#'
#' @param x a \code{data.frame} or \code{grouped_df}.
#' @param summaries a list of lists of formulea for summarizing the data set.
#' See Details and examples.
#'
#' @seealso \code{\link{qable}} for marking up \code{qwraps2_data_summary}
#' objects.  \code{\link[dplyr]{group_by}} for \code{\link[dplyr]{grouped_df}}
#' objects.  The \code{vignette("summary-statistics", package = "qwraps2")} for
#' detailed use of these functions and caveats.
#'
#' @examples
#'
#' data(mtcars2)
#' st <- summary_table_042(mtcars2[, c("mpg", "wt", "cyl_factor")])
#' print(st, markup = "markdown")
#'
#' # build the summaries quickly
#' qs <- qsummary_042(mtcars2)
#'
#' summary_table_042(mtcars2, summaries = qs[c("mpg", "cyl", "wt", "gear_factor")])
#'
#' # the _042 method would only allow for summary by a variable is
#' # dplyr::group_by was used.  The updated version for qwraps2 version 0.5.0
#' # has a improved api to summarize by a variable much easier.
#' st <- summary_table_042(dplyr::group_by(mtcars2, transmission),
#'                   summaries = qs[c("mpg", "wt", "cyl", "cyl_character", "cyl_factor")])
#' print(st, markup = "markdown")
#'
#' @export
#' @rdname deprecated
summary_table_042 <- function(x, summaries = qsummary_042(x)) {
  UseMethod("summary_table_042")
}

#' @method summary_table_042 data.frame
#' @export
summary_table_042.data.frame <- function(x, summaries = qsummary_042(x)) {

  out <- lapply(summaries, function(s) { lapply(s, function(y) { rlang::f_rhs(y) }) })
  out <- lapply(out, function(dots) { dplyr::summarize(x, !!!(dots)) })
  out <- lapply(out, t)
  out <- do.call(rbind, out)

  colnames(out) <- paste0(deparse(substitute(x), nlines = 1L, backtick = TRUE), " (N = ", frmt(nrow(x)), ")")
  attr(out, "rgroups") <- sapply(summaries, length)
  class(out) <- c("qwraps2_summary_table_042", class(out))
  out
}

#' @export
summary_table_042.grouped_df <- function(x, summaries = qsummary_042(x)) {

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

  out <- lapply(summaries, function(s) { lapply(s, function(y) { rlang::f_rhs(y) }) })
  out <- lapply(out, function(dots) { dplyr::summarize(x, !!!(dots)) })
  out <- lapply(out, t)
  out <- lapply(out, function(y) `[`(y, -1, ))
  out <- do.call(rbind, out)

  colnames(out) <- lbs
  rownames(out) <- unlist(lapply(summaries, names), use.names = FALSE)

  attr(out, "rgroups") <- sapply(summaries, length)
  class(out) <- c("qwraps2_summary_table_042", class(out))

  out
}

#' @export
print.qwraps2_summary_table_042 <- function(x, rgroup = attr(x, "rgroups"), rnames = rownames(x), cnames = colnames(x), ...) {
  print(qable(x, rgroup = rgroup, rnames = rnames, cnames = cnames, ...))
}

#' @export
#' @rdname deprecated
#' @param ... \code{qwraps2_summary_table_042} objects to bind together
#' @param deparse.level integer controlling the construction of labels in the
#' case of non-matrix-like arguments (for the default method): \code{deparse.level =
#' 0} constructs no labels; the default, \code{deparse.level = 1} or
#' \code{deparse.level = 2} constructs labels from the argument names.
#' @seealso \code{cbind}
cbind.qwraps2_summary_table_042 <- function(..., deparse.level = 1) {
  tabs <- list(...)

  for(i in seq_along(tabs)[-1]) {
    if (inherits(tabs[[i-1]], "qwraps2_summary_table_042") & inherits(tabs[[i]], "qwraps2_summary_table_042")) {
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
#' @rdname deprecated
#' @seealso \code{rbind}
rbind.qwraps2_summary_table_042 <- function(..., deparse.level = 1) {
  tabs <- list(...)

  for(i in seq_along(tabs)[-1]) {
    if (inherits(tabs[[i-1]], "qwraps2_summary_table_042") & inherits(tabs[[i]], "qwraps2_summary_table_042")) {
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
#' @rdname deprecated
qsummary_042 <- function(.data, numeric_summaries, n_perc_args, env) {
  UseMethod("qsummary_042")
}

#' @export
qsummary_042.data.frame <- function(.data,
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
  out <- lapply(out[!is.na(out)], function(x) lapply(x, FUN = stats::as.formula, env = env))

  labs <- lapply(.data, attr, "label")

  for (i in seq_along(out)) {
    if (!is.null(labs[[i]])) {
      names(out)[i] <- labs[[i]]
    }
  }

  out
}

#' @export
qsummary_042.grouped_df <- function(.data, ...) {
  qsummary_042(dplyr::ungroup(.data), ...)
}


#' @param x a variable to summarize
#' @param n_perc_args a list of arguments to pass to \code{n_perc}
#' @param envir the environment to attach to the resulting formulea
#' @rdname deprecated
#' @export
tab_summary <- function(x, n_perc_args = list(digits = 0, show_symbol = FALSE), envir = parent.frame()) {
  .Deprecated(new = "qsummary_042")
  UseMethod("tab_summary")
}

#' @export
tab_summary.numeric <- function(x, n_perc_args = list(digits = 0, show_symbol = FALSE), envir = parent.frame()) {
  v <- deparse(substitute(x), nlines = 1L, backtick = TRUE)

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
  v <- deparse(substitute(x), nlines = 1L, backtick = TRUE)

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
  v <- deparse(substitute(x), nlines = 1L, backtick = TRUE)

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

