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
#' @param ... additional arguments passed to \code{\link[base]{list.files}}.
#'
#' @examples
#' # this example is based on \url{https://stackoverflow.com/a/41439691/1104685}
#'
#' # create a temp directory for a and place a .Rmd file within
#' tmpdir <- normalizePath(paste0(tempdir(), "/llcache_eg"), mustWork = FALSE)
#' tmprmd <- tempfile(pattern = "report", tmpdir = tmpdir, fileext = "Rmd")
#' dir.create(tmpdir)
#' oldwd <- getwd()
#' setwd(tmpdir)
#'
#' # build and example .Rmd file
#' # note that the variable x is created in the first chunck and then over
#' # written in the second chunk
#' cat("---",
#'     "title: \"A Report\"",
#'     "output: html_document",
#'     "---",
#'     "",
#'     "```{r first-chunk, cache = TRUE}",
#'     "mpg_by_wt_hp <- lm(mpg ~ wt + hp, data = mtcars)",
#'     "x_is_pi <- pi",
#'     "x <- pi",
#'     "```",
#'     "",
#'     "```{r second-chunk, cache = TRUE}",
#'     "mpg_by_wt_hp_am <- lm(mpg ~ wt + hp + am, data = mtcars)",
#'     "x_is_e <- exp(1)",
#'     "x <- exp(1)",
#'     "```",
#'     sep = "\n",
#'     file = tmprmd)
#'
#' # knit the file.  evaluate the chuncks in a new environment so we can compare
#' # the objects after loading the cache.
#' kenv <- new.env()
#' knitr::knit(input = tmprmd, envir = kenv)
#'
#' # The objects defined in the .Rmd file are now in kenv
#' ls(envir = kenv)
#'
#' # view the cache
#' list.files(path = tmpdir, recursive = TRUE)
#'
#' # create three more environment, and load only the first chunk into the
#' # first, and the second chunck into the second, and then load all of the
#' # cache into the third
#' env1 <- new.env()
#' env2 <- new.env()
#' env3 <- new.env()
#'
#' lazyload_cache_labels(labels = "first-chunk",
#'                       path = paste0(tmpdir, "/cache"),
#'                       envir = env1)
#'
#' lazyload_cache_labels(labels = "second-chunk",
#'                       path = paste0(tmpdir, "/cache"),
#'                       envir = env2)
#'
#' lazyload_cache_dir(path = paste0(tmpdir, "/cache"), envir = env3)
#'
#' # Look at the conents of each of these environments
#' ls(envir = kenv)
#' ls(envir = env1)
#' ls(envir = env2)
#' ls(envir = env3)
#'
#' # The regression models are only fitted once an should be the same in all the
#' # environments where they exist, as should the variables x_is_e and x_is_pi
#' all.equal(kenv$mpg_by_wt_hp, env1$mpg_by_wt_hp)
#' all.equal(env1$mpg_by_wt_hp, env3$mpg_by_wt_hp)
#'
#' all.equal(kenv$mpg_by_wt_hp_am, env2$mpg_by_wt_hp_am)
#' all.equal(env2$mpg_by_wt_hp_am, env3$mpg_by_wt_hp_am)
#'
#' # The value of x, however, should be different in the differnet
#' # environments.  For kenv, env2, and env3 the value should be exp(1) as that
#' # was the last assignment value.  In env1 the value should be pi as that is
#' # the only relevent assignment.
#'
#' all.equal(kenv$x, exp(1))
#' all.equal(env1$x, pi)
#' all.equal(env2$x, exp(1))
#' all.equal(env3$x, exp(1))
#'
#' # cleanup
#' setwd(oldwd)
#' unlink(tmpdir, recursive = TRUE)
#'
#' @export
#' @rdname lazyload_cache
lazyload_cache_dir <- function(path = "./cache", envir = parent.frame(), ask = FALSE, verbose = TRUE, ...) {
  files <- do.call(list.files, list(path = path, pattern = "\\.rdx$", full.names = TRUE, ...))
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
lazyload_cache_labels <- function(labels, path = "./cache/", envir = parent.frame(), verbose = TRUE, filter, ...) {
  files <- do.call(list.files, list(path = path, pattern = paste0("^(", paste(labels, collapse = "|"), ")_[0-9a-f]{32}\\.rdx$"), full.names = TRUE, ...))
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

