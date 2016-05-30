#' @title Qable: an extended verion of knitr::kable
#'
#' @description Create a simple table via kable with row groups and rownames
#' similar to those of \code{hmisc::latex} or \code{htmlTable::htmlTable}.
#'
#' @details
#' TO DO
#'
#' @seealso
#' \link[knitr]{kable}
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
#' # Two ways of building tables are given in this example.  The first example
#' # explicitly build the table, row groups, rownames, cnames and used
#' # dplry::sumarize.  
#' # The second example uses a list of list and dplyr::summarize_.
#' this_summary <- function(.data) { 
#'   dplyr::summarize(.data, 
#'                    qwraps2::frmt(min(mpg)), 
#'                    qwraps2::frmt(median(mpg)),
#'                    qwraps2::frmt(max(mpg)), 
#'                    qwraps2::frmt(min(hp)), 
#'                    qwraps2::frmt(max(hp)), 
#'                    qwraps2::frmt(mean(wt)))
#' }
#' 
#' mtcars$cyl_factor <- factor(mtcars$cyl, levels = c(4, 6, 8))
#' 
#' tab <- cbind(t(this_summary(mtcars)),
#'              t(this_summary(dplyr::group_by(mtcars, cyl_factor)))[-1, ])
#' 
#' rwgrp <- c("Miles Per Gallon" = 3, "Horse Power" = 2, "Weight" = 1)
#' rwnms <- c("Min MPG", "Median MPG", "Max MPG", "Min HP", "Max HP", "Mean Weight")
#' cnms  <- c("All mtcars", paste(levels(mtcars$cyl_factor), "Cyl"))
#' 
#' qable(tab, rwgrp, rwnms, cnms, markup = "latex")
#' qable(tab, rwgrp, rwnms, cnms, markup = "markdown")
#' 
#' # Another why to build the tables is to use a list of lists, the outer list for
#' # the row groups and the inner lists for the rows of each row group.  This also
#' # let's use use the named from the list of list for the rnames and rgroups in
#' # the qable call.
#' mtcar_summary_dots <- 
#'   list("MPG" = 
#'        list("min:"    = ~ qwraps2::frmt(min(mpg)),
#'             "median:" = ~ qwraps2::frmt(median(mpg)),
#'             "max:"    = ~ qwraps2::frmt(max(mpg))),
#'        "HP" = 
#'        list("min:"    = ~ qwraps2::frmt(min(hp)),
#'             "max:"    = ~ qwraps2::frmt(max(hp))),
#'        "Weight" = 
#'        list("mean (sd)" = ~ qwraps2::mean_sd(wt)))
#' 
#' whole <- 
#'   lapply(mtcar_summary_dots, 
#'          function(dots, .data) {
#'           t(dplyr::summarize_(.data, .dots = dots))
#'          },
#'          .data = mtcars)
#' whole <- do.call(rbind, whole)
#' 
#' by_cyl <- 
#'   lapply(mtcar_summary_dots, 
#'          function(dots, .data) {
#'           t(
#'             dplyr::summarize_(.data, .dots = dots)[, -1]
#'             )
#'          },
#'          .data = dplyr::group_by(mtcars, cyl_factor))
#' by_cyl <- do.call(rbind, by_cyl)
#' 
#' tab <- cbind(whole, by_cyl)
#' 
#' qwraps2::qable(tab,
#'                markup = "latex", 
#'                rgroup = sapply(mtcar_summary_dots, length), 
#'                rnames = rownames(tab), 
#'                cnames = c("All mtcars", paste(levels(mtcars$cyl_factor), "Cyl"))
#'                )
#' 
#' qwraps2::qable(tab,
#'                markup = "markdown",
#'                rgroup = sapply(mtcar_summary_dots, length), 
#'                rnames = rownames(tab), 
#'                cnames = c("All mtcars", paste(levels(mtcars$cyl_factor), "Cyl"))
#'                ) 
#' 
#'
#' @export   
#' @rdname qable
qable <- function(x, rgroup, rnames, cnames, markup = getOption("qwraps2_markup", "latex"), ...) { 

  rg_idx <- cumsum(c(1, 1 + rgroup[-length(rgroup)]))

  if (markup == "latex") { 
    xmat <- matrix("~", nrow = nrow(x) + length(rgroup), ncol = 1 + ncol(x))
    xmat[rg_idx, 1] <- paste0("\\bf{", names(rgroup), "}")
    xmat[-rg_idx, 1] <- paste("~~", rnames)
  } else if (markup == "markdown") { 
    xmat <- matrix("&nbsp;", nrow = nrow(x) + length(rgroup), ncol = 1 + ncol(x))
    xmat[rg_idx, 1] <- paste0("**", names(rgroup), "**")
    xmat[-rg_idx, 1] <- paste("&nbsp;&nbsp;", rnames)
  } else {
    stop("markup is either 'latex' or 'markdown'")
  }

  xmat[-rg_idx, -1] <- x

  knitr::kable(xmat, format = markup, escape = !(markup == "latex"), row.names =  FALSE, col.names = c("", cnames), ...)
}

