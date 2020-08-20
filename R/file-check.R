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
#' @examples
#' # create two example files in the working directory:
#' cat("example file.", file = "QWRAPS2_EXAMPLE_1.txt")
#' cat("Another example file.", file = "QWRAPS2_EXAMPLE_2.txt")
#'
#' # Check that you have access to these files:  (Should return TRUE)
#' test1 <- file_check(c("QWRAPS2_EXAMPLE_1.txt", "QWRAPS2_EXAMPLE_2.txt"))
#' test1
#'
#' # By default, when the checks return TRUE the details of the checks are not
#' # printed.  You can view the details of the checks as follows:
#' attr(test1, "checks")
#'
#' # If one or more files is not accessable then return is FALSE and the meta data
#' # is printed by default.
#' test2 <- file_check(c("UNLIKELYFILENAME", "QWRAPS2_EXAMPLE_1.txt", "QWRAPS2_EXAMPLE_2.txt"))
#' test2
#'
#' # Or have an error thrown:
#' \dontrun{
#' file_check(c("UNLIKELYFILENAME", "QWRAPS2_EXAMPLE_1.txt", "QWRAPS2_EXAMPLE_2.txt"),
#'            stop = TRUE)
#' }
#'
#' # Verify the md5sums as well as file access:
#' file_check("QWRAPS2_EXAMPLE_1.txt", "7a3409e17f9de067740e64448a86e708")
#'
#' # If you only need to verify a subset of md5sums then use an NA in the md5sums
#' # argument:
#' file_check(c("QWRAPS2_EXAMPLE_1.txt", "QWRAPS2_EXAMPLE_2.txt"),
#'            c("7a3409e17f9de067740e64448a86e708", NA))
#'
#' # Verify all the md5sums
#' file_check(c("QWRAPS2_EXAMPLE_1.txt", "QWRAPS2_EXAMPLE_2.txt"),
#'            c("7a3409e17f9de067740e64448a86e708", "798e52b92e0ae0e60f3f3db1273235d0"))
#'
#'
#' # clean up working directory
#' unlink("QWRAPS2_EXAMPLE_1.txt")
#' unlink("QWRAPS2_EXAMPLE_2.txt")
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
    data.frame(path          = paths,
               absolute_path = absolute_paths,
               accessible    = file.access(paths, mode = 4) != -1L,
               current_md5sum = current_md5sums,
               expected_md5sum = NA_character_,
               md5check = md5check,
               stringsAsFactors = FALSE)

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

