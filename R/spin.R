#' Spin Comment Check
#'
#' A tool to help identify the opening and closing of comments in a spin
#' document.  This function is designed to help the user resolve the error
#' "comments must be put in pairs of start and end delimiters."
#'
#' @param hair Path to the R script. The script must be encoded in UTF-8 if it
#'   contains multi-byte characters.
#' @param comment A pair of regular expressions for the start and end delimiters
#'   of comments; the lines between a start and an end delimiter will be
#'   ignored. By default, the delimiters are \verb{/*} at the beginning of a
#'   line, and \verb{*/} at the end, following the convention of C-style
#'   comments.
#' @param text A character vector of code, as an alternative way to provide the
#'   R source. If \code{text} is not \code{NULL}, \code{hair} will be ignored.
#' @param ... additional arguments (not currently used.)
#'
#' @examples
#'
#' spin_comments(hair = system.file("examples/spinner1.R", package = "qwraps2"))
#'
#' @export
spin_comments <- function(hair, comment = c("^[# ]*/[*]", "^.*[*]/ *$"), text = NULL, ...) {
  x <- if (nosrc <- is.null(text)) xfun::read_utf8(hair) else xfun::split_lines(text)
  stopifnot(length(comment) == 2L)
  c1 <- grep(comment[1], x)
  c2 <- grep(comment[2], x)
  check_comments(c1, c2)
}

#' Check Comments
#'
#' A more robust check for open/close matching sets of comments in a spin file.
#'
#' @param c1 index (line numbers) for the start delimiter of comments
#' @param c2 index (line numbers) for the end delimiter of comments
#'
check_comments <- function(c1, c2) {

  cs <- sort(c(openers = c1, closers = c2))
  err <- FALSE
  notes <- character()
  while(length(cs)) {
    i <- 1
    if (grepl("closer", names(cs)[1])) {
      notes <- append(notes, paste0("  * no starting delimiter; ended on line ", cs[1]))
      cs <- cs[-1]
      err <- TRUE
    } else if (all(grepl("opener", names(cs)))) {
      notes <- append(notes, paste0("  * started on line ", cs[1], "; no end delimiter"))
      cs <- cs[-1]
      err <- TRUE
    } else {
      while (i < length(cs)) {
        if (grepl("opener", names(cs)[i]) & grepl("closer", names(cs)[i + 1])) {
          notes <- append(notes, paste0("  * started on line ", cs[i], "; ended on line ", cs[i + 1]))
          cs <- cs[-c(i, i + 1)]
          break
        } else {
          i <- i + 1
        }
      }
    }
  }

  if (err) {
     warning(paste('comments must be put in pairs of start and end delimiters.\n', paste(notes, collapse = '\n'), collapse = "\n"),
          call. = FALSE)
  }
  notes
}
