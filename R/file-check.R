#' File and Working Directory Check
#'
#' This check is three-fold: 1) verify the current working directory is as
#' expected, 2) verify the user can access the file, and 3) verify the file
#' contents are as expected (via md5sum).
#'
#' The test for the file access is done to verify the file can be read by the
#' current user.
#'
#' The return of the function is \code{TRUE} if all the files in \code{paths}
#' are accessible and all of requested md5sum checks pass.  \code{FALSE} is any
#' file is not accessible or any md5sum check fails.  By default, if the return
#' is \code{TRUE} then only \code{TRUE} will be printed to the console.  If the
#' return is \code{FALSE} then the \code{attr(, "checks")} is printed by default
#' as well.
#'
#' Good practice would be to use relative paths, a warning will be given if any
#' of the \code{paths} are determined to be absolute paths.
#'
#' @param paths a character path to the target file
#' @param md5sums a character string for the expected md5sum of the target file.
#' If \code{NULL} then only a \code{file.exists} check will be done.
#' @param stop if \code{TRUE} then an error is thrown if any of the checks fail.
#' If \code{FALSE} (default) a logical is returned.
#'
#' @return The function will return a single TRUE/FALSE value with attributes
#' \code{attr(, "checks")}.
#'
#'
#' @example examples/file-check.R
#'
#' @export
file_check <- function(paths, md5sums = NULL, stop = FALSE) {
  paths <- as.character(paths)
  md5sums <- as.character(md5sums)
  current_md5sums <- tools::md5sum(path.expand(paths))
  md5check <- rep(NA, length(paths))

  if (length(md5sums) > 0L) {
    if (length(md5sums) != length(paths)) {
      stop("md5sums vector needs to have length equal to the length of paths.")
    }
    md5check <- mapply(identical, x = current_md5sums, y = md5sums)
    md5check[is.na(md5sums)] <- NA
  }

  # Where the files paths absolute or relative?
  absolute_paths <- regexpr("^((~|/)|.:(/|\\\\))", paths) != -1L

  if (any(absolute_paths)) {
    warning("Absolute path used.  It would be preferable if you used a relative path.")
  }

  checks <-
    dplyr::data_frame(path          = paths,
                      absolute_path = absolute_paths,
                      accessible    = file.access(paths, mode = 4) != -1L,
                      current_md5sum = current_md5sums,
                      expected_md5sum = NA_character_,
                      md5check = md5check)

  if (length(md5sums) > 0L) { 
    checks$expected_md5sum <- md5sums
  }

  checks 

  status <- all(c(checks$accessible, stats::na.omit(checks$md5check)))
  attr(status, "checks") <- checks
  attr(status, "class") <- "qwraps2_file_check"

  if (!status && stop) {
    print(status)
    stop("At least one path or md5sum check failed.", call. = FALSE)
  }

  status
}

#' @export
print.qwraps2_file_check <- function(x, ...) {
  if (x) {
    print(TRUE)
  } else {
    attr(x, 'class') <- NULL
    print.default(x)
  } 
}

