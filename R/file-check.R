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
#' are accessible, are case matched (optional), and all of requested md5sum
#' checks pass.  Windows and macOS are generally case-insensitive systems, but
#' many Linux systems are case-sensitive.  As such
#' \code{\link[base]{file.exists}} and \code{\link[base]{file.access}} may
#' return different values depending the OS that is active.  \code{file_check}
#' looks for a case match as part of its checks to hopefully prevent issues
#' across operating systems.
#'
#' By default, if the return is \code{TRUE} then only \code{TRUE} will
#' be printed to the console.  If the return is \code{FALSE} then the
#' \code{attr(, "checks")} is printed by default as well.
#'
#' Good practice would be to use relative paths, a warning will be given if any
#' of the \code{paths} are determined to be absolute paths.  That said, there
#' are cases when an absolute path is needed, e.g., a common data file on a
#' server with multiple users accessing the file(s).  Set \code{absolute_paths =
#' c("silent")} to silence the warnings.
#'
#' @param paths a character path to the target file
#' @param md5sums a character string for the expected md5sum of the target file.
#' If \code{NULL} then only a \code{file.exists} check will be done.
#' @param absolute_paths a character string to set the behavior of warning
#' (default), stopping, or silent if/when absolute file paths are used.
#' @param stop if \code{TRUE} then an error is thrown if any of the checks fail,
#' else returns a logical. If \code{FALSE} (default) a logical is returned.
#'
#' @return The function will return a single TRUE/FALSE value with attributes
#' \code{attr(, "checks")}.
#'
#'
#' @examples
#' # create example files
#'
#' relative_example_file1 <-
#'   basename(
#'     tempfile(
#'       pattern = "QWRAPS2_EXAMPLE_1"
#'       , fileext = ".txt"
#'       , tmpdir = getwd()
#'     )
#'   )
#'
#' relative_example_file2 <-
#'   basename(
#'     tempfile(
#'       pattern = "QWRAPS2_EXAMPLE_2"
#'       , fileext = ".txt"
#'       , tmpdir = getwd()
#'     )
#'   )
#'
#' absolute_example_file <- tempfile()
#'
#' cat("example file.", file = relative_example_file1)
#' cat("Another example file.", file = relative_example_file2)
#' cat("Another example file.", file = absolute_example_file)
#'
#' # Check that you have access to the files in the working directory.
#' test1 <- file_check(c(relative_example_file1, relative_example_file2))
#' test1
#'
#' # By default, when the checks return TRUE the details of the checks are not
#' # printed.  You can view the details of the checks as follows:
#' attr(test1, "checks")
#'
#' # access to absolute_example_file will generate a warning about
#' # absolute_paths by default
#' test2 <- file_check(absolute_example_file)
#' test2 <- file_check(absolute_example_file, absolute_paths = "silent")
#' test2
#'
#' # Case Match
#' test_case_match <-
#'   file_check(
#'     c(relative_example_file1, tolower(relative_example_file1))
#'   )
#' test_case_match
#'
#' # If one or more files is not accessable then return is FALSE and the meta data
#' # is printed by default.
#' test_non_existent_file <-
#'   file_check(
#'     c("UNLIKELYFILENAME", relative_example_file1, relative_example_file2)
#'   )
#' test_non_existent_file
#'
#' # Or have an error thrown:
#' \dontrun{
#' file_check(
#'   c("UNLIKELYFILENAME", relative_example_file1, relative_example_file2)
#' , stop = TRUE
#' )
#' }
#'
#' # Verify the md5sums as well as file access:
#' md5_check1 <- file_check(relative_example_file1, "7a3409e17f9de067740e64448a86e708")
#' md5_check1
#'
#' # If you only need to verify a subset of md5sums then use an NA in the md5sums
#' # argument:
#' md5_check2 <-
#'   file_check(c(relative_example_file1, relative_example_file2),
#'              c("7a3409e17f9de067740e64448a86e708", NA))
#' md5_check2
#'
#' # Verify all the md5sums
#' md5_check3 <-
#'   file_check(c(relative_example_file1, relative_example_file2),
#'              c("7a3409e17f9de067740e64448a86e708", "798e52b92e0ae0e60f3f3db1273235d0"))
#' md5_check3
#'
#' # clean up working directory
#' unlink(relative_example_file1)
#' unlink(relative_example_file2)
#' unlink(absolute_example_file)
#'
#' @export
file_check <- function(paths, md5sums = NULL, absolute_paths = c("warn", "stop", "silent"), stop = FALSE) {
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

  # are files paths absolute or relative?
  action <- match.arg(absolute_paths)
  abspths <- NA
  if (action %in% c("warn", "stop")) {
    abspths <- regexpr("^((~|/)|.:(/|\\\\))", paths) != -1L
    if (any(abspths)) {
      if (action == "warn") {
        warning("Absolute path used. It would be preferable if you used a relative path.")
      } else {
        stop("Absolute path used. It would be preferable if you used a relative path.")
      }
    }
  }

  case_match <-
    sapply(paths, function(x) {length(list.files(dirname(x), pattern = basename(x), ignore.case = FALSE)) == 1L})

  checks <-
    data.frame(
               path          = paths
               , absolute_path = abspths
               , accessible    = file.access(paths, mode = 4) != -1L
               , case_match    = case_match
               , current_md5sum = current_md5sums
               , expected_md5sum = NA_character_
               , md5check = md5check
               , stringsAsFactors = FALSE
    )

  if (length(md5sums) > 0L) {
    checks$expected_md5sum <- md5sums
  }

  status <- all(c(checks$accessible, checks$case_match, stats::na.omit(checks$md5check)))
  attr(status, "checks") <- checks
  attr(status, "class") <- "qwraps2_file_check"

  if (!status && stop) {
    print(status)
    stop("At least one path, case match, or md5sum check failed.", call. = FALSE)
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

