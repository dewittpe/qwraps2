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
#' @param .data a \code{data.frame} or \code{grouped_df}.
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
#' @examples
#' # A list-of-lists for the summaries arg.  This object is of the basic form:
#' # list("row group A" =
#' #      list("row 1A" = ~ <summary function>,
#' #           "row 2A" = ~ <summary function>),
#' #      "row group B" =
#' #      list("row 1B" = ~ <summary function>,
#' #           "row 2B" = ~ <summary function>,
#' #           "row 3B" = ~ <summary function>))
#' 
#' our_summaries <-
#'   list("Miles Per Gallon" = 
#'        list("min"  = ~ min(mpg),
#'             "mean" = ~ mean(mpg),
#'             "mean &plusmn; sd" = ~ qwraps2::mean_sd(mpg),
#'             "max"  = ~ max(mpg)),
#'        "Weight" = 
#'        list("median" = ~ median(wt)),
#'        "Cylinders" = 
#'        list("4 cyl: n (%)" = ~ qwraps2::n_perc0(cyl == 4),
#'             "6 cyl: n (%)" = ~ qwraps2::n_perc0(cyl == 6),
#'             "8 cyl: n (%)" = ~ qwraps2::n_perc0(cyl == 8)))
#' 
#' # Going to use markdow for the markup language in this example,  the original
#' # option will be reset at the end of the example.
#' orig_opt <- options()$qwraps2_markup
#' options(qwraps2_markup = "markdown")
#' 
#' # The summary table for the whole mtcars data set
#' whole_table <- summary_table(mtcars, our_summaries)
#' whole_table
#' 
#' # The summary table for mtcars grouped by am (automatic or manual transmission)
#' # This will generate one column for each level of mtcars$am
#' grouped_by_table <- summary_table(dplyr::group_by(mtcars, am), our_summaries)
#' grouped_by_table
#' 
#' # To build a table with a column for the whole data set and each of the am
#' # levels
#' cbind(whole_table, grouped_by_table)
#' 
#' # A **warning** about grouped_df objects.  The attr
#' # If you use dplyr::group_by or
#' # dplyr::rowwise to manipulate a data set and fail to use dplyr::ungroup you
#' # might find a table that takes a long time to create and does not summarize the
#' # data as exapected.  For example, let's build a data set with twenty subjects
#' # and injury severity scores for head and face injuries.  We'll clean the data
#' # by finding the max ISS score for each subject and then reporting summary
#' # statistics there of.
#' set.seed(42)
#' library(dplyr)
#' dat <- dplyr::data_frame(id = letters[1:20],
#'                          head_iss = sample(1:6, 20, replace = TRUE, prob = 10 * (6:1)),
#'                          face_iss = sample(1:6, 20, replace = TRUE, prob = 10 * (6:1)))
#' 
#' iss_summary <-
#'   list("Head ISS" = 
#'        list("min"    = ~ min(head_iss),
#'             "median" = ~ median(head_iss),
#'             "max"    = ~ max(head_iss)),
#'        "Face ISS" = 
#'        list("min"    = ~ min(face_iss),
#'             "median" = ~ median(face_iss),
#'             "max"    = ~ max(face_iss)),
#'        "Max ISS" = 
#'        list("min"    = ~ min(iss),
#'             "median" = ~ median(iss),
#'             "max"    = ~ max(iss)))
#' 
#' 
#' subject_level_dat <-
#'   dat %>%
#'     dplyr::group_by(id) %>%
#'     dplyr::mutate(iss = max(head_iss, face_iss))
#' 
#' # Want: a table with one column for all subjects with nine rows divided up into
#' # three row groups.  However, this will create a table with 20 columns, one for
#' # each subject
#' summary_table(subject_level_dat, iss_summary)
#' 
#' # Ungroup the data.frame to get the correct output
#' subject_level_dat %>%
#'   dplyr::ungroup() %>%
#'   summary_table(iss_summary)
#' 
#' # reset the original markup option that was used before this example was
#' # evaluated.
#' options(qwraps2_markup = orig_opt)
#' 
#' # Detailed examples in the vignette
#' # vignette("summary-statistics", package = "qwraps2")
#'
#' @export
#' @rdname summary_table
summary_table <- function(.data, summaries) {
  UseMethod("summary_table")
}

#' @method summary_table data.frame
#' @export
summary_table.data.frame <- function(.data, summaries) {
  out <- lapply(summaries, 
                function(dots, .df) {
                  t(dplyr::summarize_(.df, .dots = dots))
                },
                .df = .data)
  out <- do.call(rbind, out)
  colnames(out) <- paste0(deparse(substitute(.data)), " (N = ", nrow(.data), ")")
  attr(out, "rgroups") <- sapply(summaries, length)
  class(out) <- c("qwraps2_summary_table", class(out))
  out
}

#' @export
summary_table.grouped_df <- function(.data, summaries) {
  ngrps <- length(attr(.data, "vars"))

  lbs <- attr(.data, "labels")
  grpsz <- attr(.data, "group_sizes")
    
  lbs <- apply(cbind(matrix(paste(rep(names(lbs), each = nrow(lbs)), as.matrix(lbs), sep= ": "), nrow = nrow(lbs)), paste0("(N = ", grpsz, ")")), 1, paste, collapse = " ")
                       
  out <- lapply(summaries, 
                function(dots, .df) {
                  t(dplyr::summarize_(.df, .dots = dots))[-seq(1, ngrps, by = 1), ]
                },
                .df = .data)
  out <- do.call(rbind, out)
  colnames(out) <- lbs
  attr(out, "rgroups") <- sapply(summaries, length)
  class(out) <- c("qwraps2_summary_table", class(out))
  out
}

#' @export
print.qwraps2_summary_table <- function(x, rgroup = attr(x, "rgroups"), rnames = rownames(x), cnames = colnames(x), ...) { 
  print(qable(x, rgroup = rgroup, rnames = rnames, cnames = cnames, ...))
}


#' @param x a variable to summarize
#' @param n_perc_args a list of arguments to pass to \code{n_perc}
#' @param envir the environment to attach to the resulting formulea
#' @export
#' @rdname summary_table
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
    s <- stats::setNames(s, c(sort(unique(x)), "Unknown"))
  } else {
    s <- lapply(levels(x),
                function(xx) {
                  paste0("~ qwraps2::n_perc(", v, " == '", xx, "'", n_args, ")")
                })
    s <- stats::setNames(s, sort(unique(x)))
  } 
  lapply(s, stats::as.formula, env = envir)
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

