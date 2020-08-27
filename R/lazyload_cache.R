#' Lazyload Cache
#'
#' Lazyload Cached label(s) or a whole directory.
#'
#' These functions helpful for loading cached chunks into an interactive R
#' session.  Consider the following scenario: you use knitr and have cached
#' chunks for lazyloading.  You've created the document, close up your IDE and
#' move on to the next project.  Later, you revisit the initial project and need
#' to retrieve the objects created in the cached chunks.  One option is to
#' reevaluate all the code, but this could be time consuming.  The other option
#' is to use \code{lazyload_cache_labels} or \code{lazyload_cache_dir} to
#' quickly (lazy)load the chunks into an active R session.
#'
#' Use \code{lazyload_cache_dir} to load a whole directory of cached objects.
#'
#' Use \code{lazyload_cache_labels} to load and explicit set of cached chunks.
#'
#' @param path the path to the cache directory.
#' @param envir the environment to load the objects into
#' @param ask if TRUE ask the user to confirm loading each database found in
#' \code{path}
#' @param verbose if TRUE display the chunk labels being loaded
#' @param full.names use the full name, i.e., include the path, for the chunk
#' label? This argument is passed to \code{\link[base]{list.files}}.
#' @param ... additional arguments passed to \code{\link[base]{list.files}}.
#'
#' @examples
#' # this example is based on \url{https://stackoverflow.com/a/41439691/1104685}
#'
#' # create a temp directory for a and place a .Rmd file within
#' tmpdr <- paste0(tempdir(), "/llcache_eg")
#' dir.create(tmpdr)
#' old_wd <- getwd()
#' setwd(tmpdr)
#'
#' cat("---",
#'     "title: \"A Report\"",
#'     "output: html_document",
#'     "---",
#'     "",
#'     "```{r first-chunk, cache = TRUE}",
#'     "mpg_by_wt_hp <- lm(mpg ~ wt + hp, data = mtcars)",
#'     "x_is_pi <- pi",
#'     "```",
#'     "",
#'     "```{r second-chunk, cache = TRUE}",
#'     "mpg_by_wt_hp_am <- lm(mpg ~ wt + hp + am, data = mtcars)",
#'     "x_is_e <- exp(1)",
#'     "```",
#'     sep = "\n",
#'     file = paste0(tmpdr, "/report.Rmd"))
#'
#' # knit the file.  evaluate the chuncks in a new environment so we can compare
#' # the objects after loading the cache.
#' kenv <- new.env()
#' knitr::knit(input = paste0(tmpdr, "/report.Rmd"), envir = kenv)
#'
#' # The objects defined in the .Rmd file are now in kenv
#' ls(envir = kenv)
#'
#' # view the cache
#' list.files(path = tmpdr, recursive = TRUE)
#'
#' # create another environment, and load only the second chunk
#' lenv <- new.env()
#' ls(envir = lenv)
#'
#' lazyload_cache_labels(labels = "second-chunk",
#'                       path = paste0(tmpdr, "/cache"),
#'                       envir = lenv)
#' lenv$x_is_e
#' lenv$mpg_by_wt_hp_am
#'
#' # load all the chuncks
#' menv <- new.env()
#' lazyload_cache_dir(path = paste0(tmpdr, "/cache"), envir = menv)
#'
#' ls(envir = menv)
#' menv$x_is_pi
#' menv$x_is_e
#'
#' # cleanup
#' setwd(old_wd)
#' unlink(tmpdr, recursive = TRUE)
#'
#' @export
#' @rdname lazyload_cache
lazyload_cache_dir <- function(path = "./cache", envir = parent.frame(), ask = FALSE, verbose = TRUE, full.names = TRUE, ...) {
  files <- do.call(list.files, list(path = path, pattern = "\\.rdx$", full.names = full.names, ...))
  files <- gsub("\\.rdx", "", files)

  load_these <- rep(TRUE, length(files))

  if (ask) {
    for (i in seq_along(files)) {
      answer <- readline(prompt = paste("load database:", gsub("_[0-9a-f]{32}", "", files[i]), "(y/n)"))
      load_these[i] <- answer %in% c("Yes", "yes", "Y", "y")
    }
  }

  files <- files[load_these]

  if (!verbose) {
    sapply(files, lazyLoad, envir = envir)
  } else {
    sapply(files,
           function(x, envir) {
             message(paste("Lazyloading:", gsub("_[0-9a-f]{32}", "", x)))
             lazyLoad(x, envir = envir) },
           envir = envir)
  }

  invisible()
}

#' @param labels a character vector of the chunk labels to load.
#' @param filter an optional function passed to \code{\link[base:lazyload]{lazyLoad}}.
#' when called on a character vector of object names returns a logical vector:
#' only objects for which this is true will be loaded.
#' @export
#' @rdname lazyload_cache
lazyload_cache_labels <- function(labels, path = "./cache/", envir = parent.frame(), verbose = TRUE, filter, full.names = TRUE, ...) {
  files <- do.call(list.files, list(path = path, pattern = paste0("^(", paste(labels, collapse = "|"), ")_[0-9a-f]{32}\\.rdx$"), full.names = full.names, ...))
  files <- gsub("\\.rdx$", "", files)

  lfound <- sapply(lapply(labels, grepl, x = files), any)

  if (!all(lfound)) {
    files <- do.call(list.files, list(path = path, pattern = "_[0-9a-f]{32}\\.rdx$", full.names = FALSE, ...))
    files <- gsub("_[0-9a-f]{32}\\.rdx$", "", files)
    message(paste0("label(s)\n", paste(paste0("  ", labels[!lfound]), collapse = "\n"), "\nnot found in path '", path, "\n\n",
                   "Available labels:\n", paste(paste0("  ", files), collapse = "\n")))
    warning("Nothing loaded", call. = FALSE)
  } else {

    if (!verbose) {
      sapply(files, lazyLoad, envir = envir, filter = filter)
    } else {
      sapply(files,
             function(x, envir, filter) {
               message(paste("Lazyloading", x))
               lazyLoad(x, envir = envir, filter = filter) },
             envir = envir,
             filter = filter)
    }
  }
  invisible()
}

