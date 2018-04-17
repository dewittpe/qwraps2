#' Create Package
#'
#' A wrapper about \code{\link[devtools]{create}} for creating R package
#' skeletons.
#'
#' Based on the \code{\link[devtools]{create}} function from devtools, this
#' function will create a package skeleton with directories for R files, and
#' examples.
#'
#' Common files are also added to the project root directory:
#' \itemize{
#'   \item CONTRIBUTING.md
#'   \item DESCRIPTION
#'   \item NAMESPACE
#'   \item NEWS.md
#'   \item README.md
#'   \item .gitignore
#'   \item .Rbuildignore
#'   \item makefile
#' }
#'
#' If data will be used in the package set \code{use_data_raw = TRUE} when
#' creating the package skeleton, or use \code{create_data_raw()} after the
#' initial package skeleton is created to add the data and data-raw directories.
#'
#' Template files for CI via travis and gitlab runners are in the template
#' directtory. Setting \code{ci} to \code{travis} of \code{gitlab} will place
#' the needed template files into the top level of the package skeleton.
#'
#' @param path location to create a new package.  The last component of the path
#' will be used as the package name.
#' @param use_data_raw logical (default \code{FALSE}), if \code{TRUE} then data
#' and data-raw directories are created.
#' @param ci when set to \code{travis} of \code{gitlab} will place
#' the needed template files into the top level of the package skeleton.
#' @param rstudio passed to \code{\link[devtools]{create}}.  devtools sets the
#' default to \code{TRUE} but \code{create_pkg} sets the default to \code{FALSE}
#' since I don't use RStudio.
#' @param ... additional arguments passed to \code{\link[devtools]{create}}
#' @param name the name of the vigenette
#'
#' @example examples/create_pkg.R
#'
#' @name create_pkg
NULL

#' @rdname create_pkg
#' @export
create_pkg <- function(path, use_data_raw = FALSE, ci = NULL, rstudio = FALSE, ...) {

  path <- normalizePath(path.expand(path))

  templates <-
    data.frame(temp_file = c("gitignore",  "Rbuildignore",
                             "NEWS.md", "CONTRIBUTING.md", "README.md",
                             "pkg-make-file"),
               pkg_path  = c(".gitignore", ".Rbuildignore",
                             "NEWS.md", "CONTRIBUTING.md", "README.md",
                             "makefile"))



  if (!is.null(ci)) {
    if (ci == "travis") {
      templates <-
        dplyr::bind_rows(templates,
                         data.frame(temp_file = c("travis.yml"),
                                    pkg_path  = c(".travis.yml"))
                         )
    } else if (ci == "gitlab") {
      templates <-
        dplyr::bind_rows(templates,
                         data.frame(temp_file = c("gitlab-ci.yml"),
                                    pkg_path  = c(".gitlab-ci.yml"))
                         )
    } else {
      stop("ci needs to be 'travis' or 'gitlab'", call. = FALSE)
    }
  }

  devtools::create(path,
                   description = list(Depends = NULL),
                   rstudio = rstudio,
                   quiet = TRUE,
                   ...)

  dir.create(paste0(path, "/examples"))

  if (use_data_raw) {
    create_data_raw(path)
  }

  apply(templates, 1, function(x) {
          writeLines(readLines(system.file("templates", x[1], package = "qwraps2")),
                     paste0(path, "/", x[2]))
                   })

  cat("VignetteBuilder: knitr\n", file = paste0(path, "/DESCRIPTION"), append = TRUE)

  invisible(TRUE)
}

#' @rdname create_pkg
#' @export
create_vignette <- function(name, path = ".") {
  path <- normalizePath(path.expand(path))
  dir.create(paste0(path, "/vignettes"))
  writeLines(readLines(system.file("templates", "vignette.R", package = "qwraps2")),
             paste0(path, "/vignettes/", name))
  writeLines(readLines(system.file("templates", "vignettes-make-file", package = "qwraps2")),
             paste0(path, "/vignettes/makefile"))
  invisible(TRUE)
}

#' @rdname create_pkg
#' @export
create_data_raw <- function(path = ".") {
  path <- normalizePath(path.expand(path))
  dir.create(paste0(path, "/data"))
  dir.create(paste0(path, "/data-raw"))
  writeLines(readLines(system.file("templates", "data-raw-make-file", package = "qwraps2")),
             paste0(path, "/data-raw/makefile"))
  invisible(TRUE)
}
