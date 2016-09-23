#' Data Summary Tables
#'
#' Tools useful for building data summary tables.  
#'
#' Detailed use of these functions can be found the a vignette.
#'
#' @param .data a \code{data.frame} or \code{grouped_df}.
#'
#' @seealso \code{qable} for marking up \code{qwraps2_data_summary} objects.
#' \code{\link[dplyr]{group_by}} for \code{grouped_df} objects.
#'
#' @export
#' @rdname summary_table
summary_table <- function(.data, summaries) {
  UseMethod("summary_table")
}

#' @method data.frame summary_table
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
print.qwraps2_summary_table <- function(x, ...) { 
  print(qable(x, rgroup = attr(x, "rgroups"), rnames = rownames(x), cnames = colnames(x), ...))
}

#' @export
#' @rdname summary_table
tab_summary <- function(x) {
  UseMethod("tab_summary")
}

# @export
# tab_summary.default <- function(x, .data = parent.frame()) {
#   cat("in default\n")
#   x <- as.name(x)
#   tab_summary(with(.data, x))
# }

#' @export
tab_summary.numeric <- function(x) {
  v <- deparse(substitute(x))

  if (any(is.na(x))) {
    s <- list(
              "min" = as.formula(paste("~ min(", v, ", na.rm = TRUE)")),
              "median (IQR)" = as.formula(paste("~ qwraps2::median_iqr(", v, ", na_rm = TRUE)")),
              "mead (sd)" = as.formula(paste("~ qwraps2::mean_sd(", v, ", na_rm = TRUE)")),
              "max" = as.formula(paste("~ max(", v, ", na.rm = TRUE)")))

    s <- c(s, list("Unknown" = as.formula(paste(" ~ qwraps2::n_perc0(is.na(", v, "))"))))

  } else {
    s <- list(
              "min" = as.formula(paste("~ min(", v, ")")),
              "median (IQR)" = as.formula(paste("~ qwraps2::median_iqr(", v, ")")),
              "mead (sd)" = as.formula(paste("~ qwraps2::mean_sd(", v, ")")),
              "max" = as.formula(paste("~ max(", v, ")")))
  } 
  s
}


#' @export
tab_summary.factor <- tab_summary.character <- function(x) {
  v <- deparse(substitute(x))

  if (any(is.na(x))) {
    x <- na.omit(x)
    s <- lapply(sort(unique(x)), 
                function(xx) {
                  as.formula(paste0("~ qwraps2::n_perc0(", v, " == '", xx, "', na_rm = TRUE)"))
                })
    s <- c(s, as.formula(paste(" ~ qwraps2::n_perc0(is.na(", v, "))")))
    s <- setNames(s, c(sort(unique(x)), "Unknown"))
  } else {
    s <- lapply(sort(unique(x)), 
                function(xx) {
                  as.formula(paste0("~ qwraps2::n_perc0(", v, " == '", xx, "')"))
                })
    s <- setNames(s, sort(unique(x)))
  } 
  s
}
