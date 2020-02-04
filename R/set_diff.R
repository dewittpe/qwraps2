#' Set Differences
#'
#' function for testing for unique values between two vectors, specifically,
#' which values are in vector1, and not in vector2, which values are not in
#' vector1 and in vector2, which values are in both vector1 and vector2.
#'
#' Wrapper to call the \code{\link{union}},
#'
#' @param x,y vectors (of the same mode)
#'
#' @examples
#'
#' # example with two sets which as a union are the upper and lower case vowels.
#' set_a <- c("A", "a", "E",      "I", "i", "O", "o", "U", "u", "E", "I")
#' set_b <- c("A", "a", "E", "e",      "i",      "o", "U", "u", "u", "a", "e")
#' set_diff(set_a, set_b)
#' set_diff(set_b, set_a)
#'
#' # example
#' set_a <- 1:90
#' set_b <- set_a[-c(23, 48)]
#' set_diff(set_a, set_b)
#' set_diff(set_b, set_a)
#'
#' # example
#' set_a <- c("A", "A", "B")
#' set_b <- c("B", "A")
#' set_diff(set_a, set_b)
#'
#' @export
set_diff <- function(x, y) {
  out <-
    list(all_values = base::union(x, y),
         x_only     = base::setdiff(x, y),
         y_only     = base::setdiff(y, x),
         both       = base::intersect(x, y),
         equal      = base::setequal(x, y))
  attr(out, "xname") <- deparse(substitute(x))
  attr(out, "yname") <- deparse(substitute(y))
  class(out) <- c("qwraps2.set_diff")
  out
}

#' @export
print.qwraps2.set_diff <- function(x, ...) {
  cat("Total number of unique values: ", length(x$all_values), "\n",
      "Number of elements in both ", attr(x, "xname"), " and ", attr(x, "yname"), ": ", length(x$both), "\n",
      "Number of elements only in ", attr(x, "xname"), ": ", length(x$x_only),
      if (length(x$x_only)) paste0("\n  unique elements: ", paste(x$x_only, collapse = ", "), "\n") else "\n",
      "Number of elements only in ", attr(x, "yname"), ": ", length(x$y_only),
      if (length(x$y_only)) paste0("\n  unique elements: ", paste(x$y_only, collapse = ", "), "\n") else "\n",
      sep = "")
  invisible(x)
}

