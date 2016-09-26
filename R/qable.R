#' @title Qable: an extended verion of knitr::kable
#'
#' @description Create a simple table via kable with row groups and rownames
#' similar to those of \code{hmisc::latex} or \code{htmlTable::htmlTable}.
#'
#' @details
#' \code{qable} is used as the printing method for \code{qwraps2_summary_table}
#' objects.  Check the vignettes for examples on building data summary tables.
#'
#' @seealso
#' \code{\link[knitr]{kable}}
#' \code{summary_table}, for 
#'
#' @param x \code{matrix} or \code{data.frame} to be turned into a qable
#' @param rgroup a named numeric vector with the name of the row group and the
#' number of rows within the group.  \code{sum(rowgroup) == nrow(x)}.
#' @param rnames a character vector of the row names
#' @param cnames column names
#' @param markup the markup language to use, passed to the \code{format}
#' argument of \code{knitr::kable}.
#' @param ... additional arguments passed to \code{knitr::kable}
#'
#' @return a character vector of the formatted numbers
#'
#' @examples
#' data(mtcars)
#' qable(mtcars)
#' qable(mtcars, markup = "markdown")
#' 
#' # by make
#' make <- sub("^(\\w+)\\s?(.*)$", "\\1", rownames(mtcars))
#' make <- c(table(make))
#' 
#' qable(mtcars[sort(rownames(mtcars)), ], rgroup = make)
#' qable(mtcars[sort(rownames(mtcars)), ], rgroup = make, markup = "markdown")
#' 
#' # define your own column names
#' qable(mtcars[sort(rownames(mtcars)), ], 
#'       rgroup = make, 
#'       cnames = toupper(colnames(mtcars)), 
#'       markup = "markdown")
#' 
#' @export   
#' @rdname qable
qable <- function(x, rgroup, rnames = rownames(x), cnames = colnames(x), markup = getOption("qwraps2_markup", "latex"), ...) { 

  if (!(markup %in% c("latex", "markdown"))) {
    stop("markup is either 'latex' or 'markdown'")
  }

  if (missing(rgroup)) { rgroup <- numeric(0) }

  xmat <- matrix("~", nrow = nrow(x) + length(rgroup), ncol = 1 + ncol(x))


  if (length(rgroup) > 0) { 
    rg_idx <- cumsum(c(1, 1 + rgroup[-length(rgroup)])) 

    if (markup == "latex") { 
      xmat[rg_idx, 1] <- paste0("\\bf{", names(rgroup), "}")
      xmat[-rg_idx, 1] <- paste("~~", rnames)
    } else {
      xmat[rg_idx, 1] <- paste0("**", names(rgroup), "**")
      xmat[-rg_idx, 1] <- paste("&nbsp;&nbsp;", rnames)
    } 
    xmat[-rg_idx, -1] <- as.matrix(x)
  } else {
    xmat[, 1] <- rnames 
    xmat[, -1] <- as.matrix(x)
  }

  if (markup == "markdown") {
    xmat <- apply(xmat, 1:2, function(x) gsub("~", "&nbsp;&nbsp;", x))
  }

  knitr::kable(xmat, format = markup, escape = !(markup == "latex"), row.names =  FALSE, col.names = c("", cnames), ...)
}
