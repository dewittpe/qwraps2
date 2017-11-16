#'---
#'title: "Create A Package Skeleton"
#'author: "Peter DeWitt"
#'output: rmarkdown::html_vignette
#'vignette: >
#'  %\VignetteEngine{knitr::rmarkdown}
#'  %\VignetteIndexEntry{Create A Package Skeleton}
#'  %\VignetteEncoding{UTF-8}
#'---

#+ label=setup, include = FALSE
library(knitr)
knitr::opts_chunk$set(collapse = TRUE)

#'
#' The `create_pkg`, and its sister functions `create_data_raw` and
#' `create_vignette`, are provided to build a package skeleton that I prefer.
#' These functions are based on the
#' [devtools](https://cran.r-project.org/package=devtools) `create`,
#' `use_vignette` and `use_data_raw` functions.
#' 
#' This vignette will provided a detailed explanation of the package structure
#' and design I prefer for R package development.
#'
#' This vignette only needs the `qwraps2` namespace loaded and attached.
library(qwraps2)

# /*
# =============================================================================
# */
#'
#' # Section 1: `create_pkg`
#' 
#' The basic package skeleton is created with the `create_pkg` function.  
str(create_pkg)

#'
#' The `path` is a directory to use for the package.  For the examples to follow
#' we will use a temporary directory.  
#'
tmp_dir <- tempdir()
pkg_dir <- paste(tmp_dir, "eg.pkg", sep = "/")
pkg_dir
create_pkg(pkg_dir)

files <- list.files(pkg_dir, all.files = TRUE, full.names = TRUE, recursive = TRUE, include.dirs = TRUE)

data.tree::as.Node(data.frame(filename = files), pathName = "filename")







#'
#' `use_data_raw` sets up the `data` and
#' `data-raw` directories.  If this
#'

#'
# /*
# =============================================================================
# */
#'
#' # Section 2
#'


#'
# /*
# =============================================================================
# */
#'
#' # Session Info
#'
print(sessionInfo(), local = FALSE)

# /*
# =============================================================================
# */ 
