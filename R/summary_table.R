#' Data Summary Tables
#'
#' Tools useful for building data summary tables.  
#'
#' Detailed use of these functions can be found the a vignette.
#'
#' The \code{print} method for the \code{qwraps2_summary_table} objects is just
#' a simple wrapper for \code{qable}.
#'
#' @param .data a \code{data.frame} or \code{grouped_df}.
#' @param summaries a list of lists of formulea for summarizing the data set.
#'
#' @seealso \code{\link{qable}} for marking up \code{qwraps2_data_summary}
#' objects.  \code{\link[dplyr]{group_by}} for \code{\link[dplyr]{grouped_df}}
#' objects.
#'
#' @return a \code{qwraps2_summary_table} object.
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
#' @export
#' @rdname summary_table
tab_summary <- function(x) {
  UseMethod("tab_summary")
}

#' @export
tab_summary.numeric <- function(x) {
  v <- deparse(substitute(x))

  if (any(is.na(x))) {
    s <- list(
              "min" = stats::as.formula(paste("~ min(", v, ", na.rm = TRUE)")),
              "median (IQR)" = stats::as.formula(paste("~ qwraps2::median_iqr(", v, ", na_rm = TRUE)")),
              "mean (sd)" = stats::as.formula(paste("~ qwraps2::mean_sd(", v, ", na_rm = TRUE)")),
              "max" = stats::as.formula(paste("~ max(", v, ", na.rm = TRUE)")))

    s <- c(s, list("Unknown" = stats::as.formula(paste(" ~ qwraps2::n_perc0(is.na(", v, "))"))))

  } else {
    s <- list(
              "min" = stats::as.formula(paste("~ min(", v, ")")),
              "median (IQR)" = stats::as.formula(paste("~ qwraps2::median_iqr(", v, ")")),
              "mean (sd)" = stats::as.formula(paste("~ qwraps2::mean_sd(", v, ")")),
              "max" = stats::as.formula(paste("~ max(", v, ")")))
  } 
  s
}

#' @export
tab_summary.character <- function(x) {
  v <- deparse(substitute(x))

  if (any(is.na(x))) {
    x <- stats::na.omit(x)
    s <- lapply(sort(unique(x)), 
                function(xx) {
                  stats::as.formula(paste0("~ qwraps2::n_perc0(", v, " == '", xx, "', na_rm = TRUE)"))
                })
    s <- c(s, stats::as.formula(paste(" ~ qwraps2::n_perc0(is.na(", v, "))")))
    s <- stats::setNames(s, c(sort(unique(x)), "Unknown"))
  } else {
    s <- lapply(sort(unique(x)), 
                function(xx) {
                  stats::as.formula(paste0("~ qwraps2::n_perc0(", v, " == '", xx, "')"))
                })
    s <- stats::setNames(s, sort(unique(x)))
  } 
  s
}

#' @export
tab_summary.factor <- function(x) {
  v <- deparse(substitute(x))

  if (any(is.na(x))) {
    s <- lapply(levels(x),
                function(xx) {
                  stats::as.formula(paste0("~ qwraps2::n_perc0(", v, " == '", xx, "', na_rm = TRUE)"))
                })
    s <- c(s, stats::as.formula(paste(" ~ qwraps2::n_perc0(is.na(", v, "))")))
    s <- stats::setNames(s, c(sort(unique(x)), "Unknown"))
  } else {
    s <- lapply(levels(x),
                function(xx) {
                  stats::as.formula(paste0("~ qwraps2::n_perc0(", v, " == '", xx, "')"))
                })
    s <- stats::setNames(s, sort(unique(x)))
  } 
  s
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

