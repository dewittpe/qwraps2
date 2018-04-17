#' Package Checks
#'
#' Check if a package is available on the local machine and optionally verify a
#' version.
#'
#' When writing a script that will be shared it is very likely that the multiple
#' authors/users will need to have a certain set of packages available to load.
#' The \code{pkg_check} function will verify that the packages are available to
#' load, this includes an optional version test, and attach the package to the
#' search list if requested.
#'
#' Testing for package versions will is done as \code{packageVersion(x) >=
#' version}.  If you need a specific version of a package you should explicitly
#' use \code{packageVersion(x) == version} in your script.
#' 
#' @param pkgs a character vector of package names to check for
#' @param versions an optional character vector, of the same length of
#' \code{pkgs} for the minimum version of the packages.
#' @param stop if \code{TRUE} then an error is thrown if any of the checks fail.
#' If \code{FALSE} (default) a logical is returned.
#'
#' @export
pkg_check <- function(pkgs, versions, stop = FALSE) {
  if (missing(versions)) {
    versions <- rep(NA, length(pkgs))
  } else {
    if (length(pkgs) != length(versions)) {
      stop("length(pkgs) != length(versions)")
    }
  }

  checks <-
    Map(function(p, v) {
          pkgv <- try(utils::packageVersion(p), silent = TRUE)

          if (inherits(pkgv, "try-error")) {
            out <- list(package = p, version = v, available = FALSE, installed_version = NA)
          } else {
            if (is.na(v)) {
              out <- list(package = p, version = v, available = TRUE, installed_version = as.character(pkgv))
            } else {
              out <- list(package = p, version = v, available = pkgv >= v, installed_version = as.character(pkgv))
            }
          }
          out 
  },
           p = pkgs, v = versions) %>%
    lapply(dplyr::as_data_frame) %>%
    dplyr::bind_rows(.)

  out <- all(checks$available)
  attr(out, "checks") <- checks
  attr(out, "class")  <- "qwraps2_pkg_check"

  if (!out && stop) {
    print(out)
    stop("At least one package is not available.", call. = FALSE)
  }

  out
}

#' @export
print.qwraps2_pkg_check <- function(x, ...) {
  if (x) {
    print(TRUE)
  } else {
    attr(x, 'class') <- NULL
    print.default(x)
  } 
}

