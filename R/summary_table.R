#' Data Summary Tables
#'
#' Tools useful for building data summary tables.
#'
#' \code{summary_table} can be used to generate good looking, simple tables in
#' LaTeX or markdown.  Functions like xtables::print.xtable and Hmisc::latex
#' provide many more tools for formatting tables.  The purpose of
#' \code{summary_table} is to generate good looking tables quickly within
#' workflow for summarizing a data set.
#'
#' Creating a list-of-lists of summary functions to apply to a data set will
#' allow the exploration of the whole data set and grouped data sets.  In the
#' example provided on this page we see a set of summary measures for the
#' \code{\link[datasets]{mtcars}} data set and the construction of a table for
#' the whole data set and for a grouped data set.
#'
#'
#' The list-of-lists should be thought of as follows:  the outer list defines
#' row groups, the inner lists define the rows within each row group.
#'
#' More detailed use of these functions can be found the "summary-statistics"
#' vignette.
#'
#' The \code{print} method for the \code{qwraps2_summary_table} objects is just
#' a simple wrapper for \code{\link{qable}}.
#'
#' @param x a \code{data.frame}.
#' @param summaries a list of lists of formulea for summarizing the data set.
#' See Details and examples.
#' @param by a character vector of variable names to generate the summary by,
#' that is one column for each unique values of the variables specified.
#' @param qable_args additional values passed to \code{\link{qable}}
#' @param ... pass through
#'
#' @seealso \code{\link{qsummary}} for generating the summaries,
#' \code{\link{qable}} for marking up \code{qwraps2_data_summary} objects.
#' The \code{vignette("summary-statistics", package = "qwraps2")} for detailed
#' use of these functions and caveats.
#'
#' @return a \code{qwraps2_summary_table} object.
#'
#' @examples
#' # A list-of-lists for the summaries arg.  This object is of the basic form:
#' #
#' # list("row group A" =
#' #      list("row 1A" = ~ <summary function>,
#' #           "row 2A" = ~ <summary function>),
#' #      "row group B" =
#' #      list("row 1B" = ~ <summary function>,
#' #           "row 2B" = ~ <summary function>,
#' #           "row 3B" = ~ <summary function>))
#'
#' our_summaries <-
#'   list("Miles Per Gallon" =
#'          list("min"  = ~ min(mpg),
#'               "mean" = ~ mean(mpg),
#'               "mean &plusmn; sd" = ~ qwraps2::mean_sd(mpg),
#'               "max"  = ~ max(mpg)),
#'        "Weight" =
#'          list("median" = ~ median(wt)),
#'        "Cylinders" =
#'          list("4 cyl: n (%)" = ~ qwraps2::n_perc0(cyl == 4),
#'               "6 cyl: n (%)" = ~ qwraps2::n_perc0(cyl == 6),
#'               "8 cyl: n (%)" = ~ qwraps2::n_perc0(cyl == 8)))
#'
#' # Going to use markdown for the markup language in this example,  the original
#' # option will be reset at the end of the example.
#' orig_opt <- options()$qwraps2_markup
#' options(qwraps2_markup = "markdown")
#'
#' # The summary table for the whole mtcars data set
#' whole_table <- summary_table(mtcars, our_summaries)
#' whole_table
#'
#' # The summary table for mtcars grouped by am (automatic or manual transmission)
#' # This will generate one column for each level of mtcars$am
#' grouped_by_table <-
#'   summary_table(mtcars, our_summaries, by = "am")
#' grouped_by_table
#'
#' # an equivalent call if you are using the tidyverse:
#' summary_table(dplyr::group_by(mtcars, am), our_summaries)
#'
#' # To build a table with a column for the whole data set and each of the am
#' # levels
#' cbind(whole_table, grouped_by_table)
#'
#' # Adding a caption for a LaTeX table
#' print(whole_table, caption = "Hello world", markup = "latex")
#'
#' # A **warning** about grouped_df objects.
#' # If you use dplyr::group_by or
#' # dplyr::rowwise to manipulate a data set and fail to use dplyr::ungroup you
#' # might find a table that takes a long time to create and does not summarize the
#' # data as expected.  For example, let's build a data set with twenty subjects
#' # and injury severity scores for head and face injuries.  We'll clean the data
#' # by finding the max ISS score for each subject and then reporting summary
#' # statistics there of.
#' set.seed(42)
#' dat <- data.frame(id = letters[1:20],
#'                   head_iss = sample(1:6, 20, replace = TRUE, prob = 10 * (6:1)),
#'                   face_iss = sample(1:6, 20, replace = TRUE, prob = 10 * (6:1)))
#' dat <- dplyr::group_by(dat, id)
#' dat <- dplyr::mutate(dat, iss = max(head_iss, face_iss))
#'
#' iss_summary <-
#'   list("Head ISS" =
#'        list("min"    = ~ min(head_iss),
#'             "median" = ~ median(head_iss),
#'             "max"    = ~ max(head_iss)),
#'        "Face ISS" =
#'        list("min"    = ~ min(face_iss),
#'             "median" = ~ median(face_iss),
#'             "max"    = ~ max(face_iss)),
#'        "Max ISS" =
#'        list("min"    = ~ min(iss),
#'             "median" = ~ median(iss),
#'             "max"    = ~ max(iss)))
#'
#' # Want: a table with one column for all subjects with nine rows divided up into
#' # three row groups.  However, the following call will create a table with 20
#' # columns, one for each subject because dat is a grouped_df
#' summary_table(dat, iss_summary)
#'
#' # Ungroup the data.frame to get the correct output
#' summary_table(dplyr::ungroup(dat), iss_summary)
#'
#'
#' ################################################################################
#' # The Default call will work with non-syntactically valid names and will
#' # generate a table with statistics defined by the qsummary call.
#' summary_table(mtcars, by = "cyl")
#'
#' # Another example from the diamonds data
#' data("diamonds", package = "ggplot2")
#' diamonds["The Price"] <- diamonds$price
#' diamonds["A Logical"] <- sample(c(TRUE, FALSE), size = nrow(diamonds), replace = TRUE)
#'
#' # the next two lines are equivalent.
#' summary_table(diamonds)
#' summary_table(diamonds, qsummary(diamonds))
#'
#' summary_table(diamonds, by = "cut")
#'
#' summary_table(diamonds,
#'               summaries =
#'               list("My Summary of Price" =
#'                    list("min price" = ~ min(price),
#'                         "IQR"       = ~ stats::IQR(price))),
#'               by = "cut")
#'
#' ################################################################################
#' # Data sets with missing values
#' temp <- mtcars
#' temp$cyl[5] <- NA
#' temp$am[c(1, 5, 10)] <- NA
#' temp$am <- factor(temp$am, levels = 0:1, labels = c("Automatic", "Manual"))
#' temp$vs <- as.logical(temp$vs)
#' temp$vs[c(2, 6)] <- NA
#' qsummary(temp[, c("cyl", "am", "vs")])
#' summary_table(temp[, c("cyl", "am", "vs")])
#'
#' ################################################################################
#' # Group by Multiple Variables
#' temp <- mtcars
#' temp$trans <- factor(temp$am, 0:1, c("Manual", "Auto"))
#' temp$engine <- factor(temp$vs, 0:1, c("V-Shaped", "Straight"))
#' summary_table(temp, our_summaries, by = c("trans", "engine"))
#'
#' ################################################################################
#' # binding tables together.  The original design and expected use of
#' # summary_table did not require a rbind, as all rows are defined in the
#' # summaries argument.  That said, here are examples of using cbind and rbind to
#' # build several different tables.
#' our_summary1 <-
#'   list("Miles Per Gallon" =
#'        list("min" = ~ min(mpg),
#'             "max" = ~ max(mpg),
#'             "mean (sd)" = ~ qwraps2::mean_sd(mpg)),
#'        "Displacement" =
#'        list("min" = ~ min(disp),
#'             "max" = ~ max(disp),
#'             "mean (sd)" = ~ qwraps2::mean_sd(disp)))
#'
#' our_summary2 <-
#'   list(
#'        "Weight (1000 lbs)" =
#'        list("min" = ~ min(wt),
#'             "max" = ~ max(wt),
#'             "mean (sd)" = ~ qwraps2::mean_sd(wt)),
#'        "Forward Gears" =
#'        list("Three" = ~ qwraps2::n_perc0(gear == 3),
#'             "Four"  = ~ qwraps2::n_perc0(gear == 4),
#'             "Five"  = ~ qwraps2::n_perc0(gear == 5))
#'        )
#'
#' tab1 <- summary_table(mtcars, our_summary1)
#' tab2 <- summary_table(dplyr::group_by(mtcars, am), our_summary1)
#' tab3 <- summary_table(dplyr::group_by(mtcars, vs), our_summary1)
#'
#' tab4 <- summary_table(mtcars, our_summary2)
#' tab5 <- summary_table(dplyr::group_by(mtcars, am), our_summary2)
#' tab6 <- summary_table(dplyr::group_by(mtcars, vs), our_summary2)
#'
#' cbind(tab1, tab2, tab3)
#' cbind(tab4, tab5, tab6)
#'
#' # row bind is possible, but it is recommended to extend the summary instead.
#' rbind(tab1, tab4)
#' summary_table(mtcars, summaries = c(our_summary1, our_summary2))
#'
#' \dontrun{
#'   cbind(tab1, tab4) # error because rows are not the same
#'   rbind(tab1, tab2) # error because columns are not the same
#' }
#'
#' ################################################################################
#' # reset the original markup option that was used before this example was
#' # evaluated.
#' options(qwraps2_markup = orig_opt)
#'
#' # Detailed examples in the vignette
#' # vignette("summary-statistics", package = "qwraps2")
#'
#'
#' @export
#' @rdname summary_table
summary_table <- function(x, summaries = qsummary(x), by = NULL, qable_args = list(), ...) {
  UseMethod("summary_table")
}

#' @export
summary_table.grouped_df <- function(x, summaries = qsummary(x), by = NULL, qable_args = list(), ...) {
  if (!is.null(by)) {
    warning("You've passed a grouped_df to summary_table and specified the `by` argument.  The `by` argument will be ignored.")
  }

  # this assumes dplyr version 0.8.0 or newer
  lbs <- names(attr(x, "groups"))
  lbs <- lbs[-length(lbs)]
  warning(paste0("grouped_df detected. Setting `by` argument to\n  c('", paste(lbs, collapse = "', '"), "')"))
  NextMethod(object = x, by = lbs, qable_args = qable_args, ...)
}

#' @export
summary_table.data.frame <- function(x, summaries = qsummary(x), by = NULL, qable_args = list(), ...) {

  if (!missing(summaries)) {
    if ( any(grepl("\\.data\\$", parse(text = summaries))) ) {
      warning("Use of the data pronoun is no longer required/encouraged.  The ability to use it has been deprecated.  See the documentation for summary_table, qsummary, and the vignettes for more detail.  The use of the data pronoun will be supported in version 0.5.0 of qwraps2 with this warning.")
    }
  }

  if (!is.null(by)) {
    subsets <- split(x, interaction(x[, by]))
  } else {
    subsets <- list(x)
  }

  rtn <- lapply(subsets, apply_summaries, summaries = summaries)

  if (length(rtn) > 1L) {
    clnms <- paste0(names(rtn), " (N = ", sapply(rtn, attr, "n"), ")")
  } else {
    clnms <- paste0(deparse(substitute(x), nlines = 1L, backtick = TRUE), " (N = ", frmt(nrow(x)), ")")
  }

  for (i in 1:length(rtn)) {
    colnames(rtn[[i]]) <- clnms[i]
    rtn[[i]] <- do.call(qable, c(list(x = rtn[[i]]), list(rgroup = attr(rtn[[i]], "rgroup")), qable_args)
    )
  }

  if (length(rtn) > 1) {
    rtn <- do.call(cbind.qwraps2_qable, rtn)
  } else {
    rtn <- rtn[[1]]
  }

  class(rtn) <- c("qwraps2_summary_table", "qwraps2_qable")
  rtn
}

apply_summaries <- function(summaries, x) {
  rtn <- lapply(summaries, lapply, stats::model.frame, x)
  rtn <- lapply(rtn, lapply, function(xx) if(nrow(xx) == 0) data.frame("NA") else xx)
  rtn <- lapply(rtn, lapply, function(y) {attr(y, "terms") <- NULL; y})
  rtn <- lapply(rtn, lapply, unlist)
  rtn <- lapply(rtn, lapply, function(y) {attr(y, "names") <- NULL; y})
  rtn <- lapply(rtn, unlist)
  rtn <- lapply(rtn, as.matrix, ncol = 1)

  rgroup <- sapply(rtn, nrow)

  rtn <- do.call(rbind, rtn)
  attr(rtn, "rgroup") <- rgroup
  attr(rtn, "n") <- nrow(x)
  rtn
}


#' @param numeric_summaries a list of functions to use for summarizing numeric
#' variables.  The functions need to be provided as character strings with the
#' single argument defined by the \code{\%s} symbol.
#' @param n_perc_args a list of arguments to pass to
#' \code{\link[qwraps2]{n_perc}} to be used with \code{character} or
#' \code{factor} variables within \code{x}.
#' @param env environment to assign to the resulting formulae

#' @rdname summary_table
#' @export
qsummary <- function(x, numeric_summaries, n_perc_args, env = parent.frame()) {
  UseMethod("qsummary")
}

#' @export
qsummary.grouped_df <- function(x, numeric_summaries, n_perc_args, env = parent.frame()) {
  NextMethod()
}

#' @export
qsummary.data.frame <- function(x,
                                numeric_summaries =
                                  list("minimum"      = "~ qwraps2::frmt(min(%s))",
                                       "median (IQR)" = "~ qwraps2::median_iqr(%s)",
                                       "mean (sd)"    = "~ qwraps2::mean_sd(%s)",
                                       "maximum"      = "~ qwraps2::frmt(max(%s))"),
                                n_perc_args = list(digits = 0, show_symbol = FALSE)
                                ,
                                env = parent.frame()) {

  npa <- lapply(n_perc_args, deparse)
  npa <- paste(paste(names(npa), npa, sep = " = "), collapse = ", ")

  rtn <-
    lapply(names(x),
           function(variable) {
             if (is.numeric(x[[variable]])) {
               summaries <- numeric_summaries
             } else if (is.character(x[[variable]]) | is.factor(x[[variable]])) {
               lvls <- levels(as.factor(x[[variable]]))
               summaries <- as.list(paste0("~ qwraps2::n_perc(%s == '", lvls, "', ", npa, ")"))
               summaries <- stats::setNames(summaries, lvls)
             } else if (is.logical(x[[variable]])) {
               summaries <- as.list(paste0("~ qwraps2::n_perc(%s, ", npa, ")"))
             } else if (inherits(x[[variable]], "Date")) {
               summaries <- list("first" = " ~ as.character(min(%s))", "last" = "~ as.character(max(%s))")
             } else {
               warning(sprintf("no default summary method for class '%s'", class(x[[variable]])))
               return(NULL)
             }

             if (any(is.na(x[[variable]]))) {
               summaries <- lapply(summaries, sprintf, "na.omit(%s)")
               summaries <- c(summaries, list("Unknown/Missing" = "~ qwraps2::n_perc(is.na(%s))"))
             }

             if (make.names(variable) == variable) {
               lapply(summaries, sprintf, variable)
             } else {
               lapply(summaries, sprintf, paste0("`", variable, "`"))
             }

           })
  rtn <- stats::setNames(rtn, names(x))

  rtn <- lapply(rtn, lapply, stats::as.formula, env = env)

  # if there is a label for the column use that as the name for the summary
  labs <- lapply(x, attr, "label")
  if (!all(sapply(labs, is.null))) {
    labs_i <- which(!sapply(labs, is.null))
    names(rtn)[labs_i] <- labs[labs_i]
  }

  rtn
}

#' @export
print.qwraps2_summary_table <- function(x, qable_args = list(), ...) {

  for (nm in names(qable_args)) {
    if (nm == "kable_args") {
      for (nm2 in names(qable_args$kable_args)) {
        attr(x, "qable_args")[["kable_args"]][[nm2]] <- qable_args$kable_args[[nm2]]
      }
    } else {
      attr(x, "qable_args")[[nm]] <- qable_args[[nm]]
    }
  }

  NextMethod(x, ...)

  invisible(x)
}
