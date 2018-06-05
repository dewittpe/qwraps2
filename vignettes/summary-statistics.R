#'---
#'title: "Formatted Summary Statistics and Data Summary Tables with qwraps2"
#'author: "Peter DeWitt"
#'date: "`r Sys.Date()`"
#'output: rmarkdown::html_vignette
#'vignette: >
#'  %\VignetteIndexEntry{summary-statistics}
#'  %\VignetteEngine{knitr::rmarkdown}
#'  %\VignetteEncoding{UTF-8}
#'---
#'
#+ label = "setup", include = FALSE
knitr::opts_chunk$set(collapse = TRUE)

#'
# /*
# =============================================================================
# */
#'
#' # Introduction
#'
#' It is common for a manuscript to require a data summary table.  The table might
#' include simple summary statistics for the whole sample and for subgroups.
#' There are several tools available to build such tables.  In my opinion, though,
#' most of those tools have nuances imposed by the creators/authors such that other
#' users need not only understand the tool, but also think like the authors.
#' I wrote this package to be as flexible and general as possible.  I hope you like
#' these tools and will be able to use them in your work.
#'
#' This vignette presents the use of the `summary_table`, `tab_summary`, and
#' `qable` functions for quickly building data summary tables.  These functions
#' implicitly use the `mean_sd`, `median_iqr`, and `n_perc0` functions from
#' `qwraps2` as well.
#'
#' ## Prerequisites Example Data Set
#' We will use a modified version of the `mtcars` data set for examples throughout
#' this vignette. The following packages are required to run the code in this
#' vignette and to construct the `mtcars2` `data.frame`.
#'
#' The `mtcars2` data frame will have three versions of the `cyl` vector: the
#' original numeric values in `cyl`, a `character` version, and a `factor` version.
set.seed(42)
library(magrittr)
library(dplyr)
library(qwraps2)

# define the markup language we are working in.
# options(qwraps2_markup = "latex") is also supported.
options(qwraps2_markup = "markdown")

data(mtcars)

mtcars2 <-
  dplyr::mutate(mtcars,
                cyl_factor = factor(cyl,
                                    levels = c(6, 4, 8),
                                    labels = paste(c(6, 4, 8), "cylinders")),
                cyl_character = paste(cyl, "cylinders"))

str(mtcars2)

#'
#' Notice that the construction of the `cyl_factor` and `cyl_character` vectors
#' was done such that the coercion of `cyl_character` to a `factor` will not be the
#' same as the `cyl_factor` vector; the levels are in a different order.
#'
with(mtcars2, table(cyl_factor, cyl_character))
with(mtcars2, all.equal(factor(cyl_character), cyl_factor))

#'
#' # Review of Summary Statistic Functions and Formatting
#'
#' ## Means and Standard Deviations
#' `mean_sd` will return the (arithmetic) mean and standard deviation for numeric
#' vector. For example, `mean_sd(mtcars2$mpg)` will return the formatted string.
#'
mean_sd(mtcars2$mpg)
mean_sd(mtcars2$mpg, denote_sd = "paren")

#'
#' The default setting for `mean_sd` is to return the mean &plusmn; sd.  In a
#' table this default is helpful because the default table formatting for counts
#' and percentages is n (%).
#'
#' `mean_sd` and other functions are helpful for in-line text too:
#'
#'     The `r nrow(mtcars2)` vehicles in the `mtcars` data set had an average fuel
#'     economy of `r mean_sd(mtcars$mpg)` miles per gallon.
#'
#' produces
#'
#'> The `r nrow(mtcars2)` vehicles in the `mtcars` data set had an average fuel
#'> economy of `r mean_sd(mtcars$mpg)` miles per gallon.
#'
#' ## Mean and Confidence intervals
#' If you need the mean and a confidence interval there is `mean_ci`.
#' `mean_ci` returns a `qwraps2_mean_ci` object which is a
#' named vector with the mean, lower confidence limit, and the upper confidence
#' limit.   The printing method for `qwraps2_mean_ci` objects is a call to the
#' `frmtci` function.  You an modify the formatting of printed result by adjusting
#' the arguments pasted to `frmtci`.
mci <- mean_ci(mtcars2$mpg)
mci
print(mci, show_level = TRUE)

#'
#' ## Median and Inner Quartile Range
#' Similar to the `mean_sd` function, the `median_iqr` returns the median and the
#' inner quartile range (IQR) of a data vector.
median_iqr(mtcars2$mpg)

#'
#' ## Count and Percentages
#' The `n_perc` function is the workhorse, but `n_perc0` is also provided for ease
#' of use in the same way that base R has `paste` and `paste0`.  `n_perc` returns
#' the n (%) with the percentage sign in the string, `n_perc0` omits the
#' percentage sign from the string.  The latter is good for tables, the former for
#' in-line text.
#'
n_perc(mtcars2$cyl == 4)
n_perc0(mtcars2$cyl == 4)

n_perc(mtcars2$cyl_factor == 4)  # this returns 0 (0.00%)
n_perc(mtcars2$cyl_factor == "4 cylinders")
n_perc(mtcars2$cyl_factor == levels(mtcars2$cyl_factor)[2])

# The count and percentage of 4 or 6 cylinders vehicles in the data set is
n_perc(mtcars2$cyl %in% c(4, 6))

#'
#' ## Geometric Means and Standard Deviations
#'
#' Let $\left\{x_1, x_2, x_3, \ldots, x_n \right\}$ be a sample of size $n$ with
#' $x_i > 0$ for all $i.$  Then the geometric mean, $\mu_g,$ and geometric standard
#' deviation are in Equation \@ref(eq:geometricmean) and \@ref(eq:geometricsd)
#' respectively.
#'
#' $$
#' \begin{equation}
#'   (\#eq:geometricmean)
#'   \mu_g = \left( \prod_{i = 1}^{n} x_i \right)^{\frac{1}{n}} = b^{ \sum_{i = 1}^{n} \log_{b} x_i }
#' \end{equation}
#' $$
#'
#' $$
#' \begin{equation}
#'   (\#eq:geometricsd)
#'   \sigma_g = b ^ {
#'   \sqrt{ \frac{\sum_{i = 1}^{n} \left( \log_{b} \frac{x_i}{\mu_g}
#'   \right)^2}{n}}}
#' \end{equation}
#' $$
#'
#' When looking for the geometric standard deviation in R, the simple
#' `exp(sd(log(x)))` is not exactly correct.  Note that in
#' \@ref(eq:geometricsd) the denominator is $n,$ the full sample size, where as
#' the `sd` and `var` functions in R use the denominator $n - 1.$  To get
#' the geometric standard deviation one should adjust the result by multiplying the
#' variance by $(n - 1) / n$ or the standard deviation by $\sqrt{(n - 1) / n}.$
#' See the example below.
x <- runif(6, min = 4, max = 70)

# geometric mean
mu_g <- prod(x) ** (1 / length(x))
mu_g
exp(mean(log(x)))
1.2 ** mean(log(x, base = 1.2))

# geometric standard deviation
exp(sd(log(x)))  ## This is wrong

# these equations are correct
sigma_g <- exp(sqrt(sum(log(x / mu_g) ** 2) / length(x)))
sigma_g

exp(sqrt((length(x) - 1) / length(x)) * sd(log(x)))

#'
#' The functions `gmean`, `gvar`, and `gsd` in the
#' package, provide the geometric
#' mean, variance, and standard deviation for a sample.
gmean(x)
all.equal(gmean(x), mu_g)

gvar(x)
all.equal(gvar(x), sigma_g^2)  # This is supposed to be FALSE
all.equal(gvar(x), exp(log(sigma_g)^2))

gsd(x)
all.equal(gsd(x), sigma_g)

#'
#' `gmean_sd` will provide a quick way for reporting the geometric mean and
#' geometric standard deviation in the same way that `mean_sd` does for the
#' arithmetic mean and arithmetic standard deviation:
gmean_sd(x)

#'
#' # Building a Data Summary Table
#'
#' Objective: build a table reporting summary statistics for some of the variables
#' in the `mtcars2` `data.frame` overall and within subgroups.  We'll start with
#' something very simple and build up to something bigger.
#'
#' Let's report the min, max, and mean (sd) for continuous variables and n (%) for
#' categorical variables.  We will report `mpg`, `disp`, `wt`, and `gear` overall
#' and by number of cylinders.
#'
#' The function `summary_table`, along with some `dplyr` functions will do the work
#' for us.  `summary_table` takes two arguments:
#'
#' 1. `.data` a (`grouped_df`) data.frame
#' 2. `summaries` a list of summaries.  This is a list-of-lists.  The outer list
#'    defines the row groups and the inner lists define the specif summaries.
#'
args(summary_table)

#'
#' Let's build a list-of-lists to pass to the `summaries` argument of
#' `summary_table`.  The inner lists are named `formula`e defining the wanted
#' summary.  These `formula`e are passed through `dplyr::summarize_` to generate
#' the table.  The names are important, as they are used to label row groups and row
#' names in the table.  The arguemnt for the functions below use the `.data`
#' pronoun for tidy evaluation (see `help(topic = ".data", package = "rlang")`).
#' The use of this pronoun is not mandatory, however, the use of the pronoun is
#' strongly encouraged.
our_summary1 <-
  list("Miles Per Gallon" =
       list("min" = ~ min(.data$mpg),
            "max" = ~ max(.data$mpg),
            "mean (sd)" = ~ qwraps2::mean_sd(.data$mpg)),
       "Displacement" =
       list("min" = ~ min(.data$disp),
            "max" = ~ max(.data$disp),
            "mean (sd)" = ~ qwraps2::mean_sd(.data$disp)),
       "Weight (1000 lbs)" =
       list("min" = ~ min(.data$wt),
            "max" = ~ max(.data$wt),
            "mean (sd)" = ~ qwraps2::mean_sd(.data$wt)),
       "Forward Gears" =
       list("Three" = ~ qwraps2::n_perc0(.data$gear == 3),
            "Four"  = ~ qwraps2::n_perc0(.data$gear == 4),
            "Five"  = ~ qwraps2::n_perc0(.data$gear == 5))
       )
#'
#' Building the table is done with a call to `summary_table`:
#'
#+ results = "asis"
### Overall
whole <- summary_table(mtcars2, our_summary1)
whole

#'
#' The `summary_table` will work with grouped data frames too.
#+ results = "asis"
### By number of Cylinders
by_cyl <- summary_table(dplyr::group_by(mtcars2, cyl_factor), our_summary1)
by_cyl

#'
#' To report a table with both the whole sample summary and conditional columns
#' together:
both <- cbind(whole, by_cyl)
both

#'
#' If you want to change the column names, do so via the `cnames` argument to
#' `qable` via the print method for `qwraps2_summary_table` objects.  Any argument
#' that you want to send to `qable` can be sent there when explicitly using the
#' `print` method for `qwraps2_summary_table` objects.
#+ results = "asis"
print(both,
      rtitle = "Summary Statistics",
      cnames = c("Col 0", "Col 1", "Col 2", "Col 3"))
#'
#' ## Easy building of the summaries
#'
#' The task of building the `summaries` list-of-lists can be tedious.  `qsummary`
#' is designed to make it easier.  `qsummary` will use a set of predefined
#' functions to summarize numeric columns of a `data.frame`, a set of arguments
#' to pass to `qwraps2::n_perc` for categorical (`character` and `factors`)
#' variables.
#'
#' By default, calling `summary_table` will use the default summary metrics
#' defined by `qsummary`.  The purpose of `qsummary` is to provide the same
#' summary for all numeric variables within a data.frame and a single style of
#' summary for categorical variables within the data.frame.  For example, the
#' default summary for the `mtcars2` data set is
qsummary(mtcars2)

#'
#' That default summary is used for a table as follows:
#+label="summary_table_mtcars2_default", result = "asis"
summary_table(mtcars2)

#'
#' Now, say we want to only report the minimum and maximum for each of the
#' numeric variables and for the categorical variables we want two show the
#' demoninator for each category and for the percentage, to one digit with the
#' percent symbol in the table.
#' Note that when defining the list of numeric_summaries that the argument place
#' holder is the `%s` character.
new_summary <-
  qsummary(mtcars2,
           numeric_summaries = list("Minimum" = "~ min(%s)",
                                    "Maximum" = "~ max(%s)"),
           n_perc_args = list(digits = 1, show_symbol = TRUE, show_denom = "always"))

new_summary

#'
#' The resulting table is:
#+results = "asis"
summary_table(mtcars2, new_summary)

#'
#' The summary can easily be used on a grouped `data.frame`.
summary_table(dplyr::group_by(mtcars2, .data$am), new_summary)

#'
#' ## Adding P-values to a Summary Table
#'
#' There are many, many different ways to format data summary tables. Adding
#' p-values to a table is just one thing that can be done in more than one way.
#' For example, if a row group reports the counts and percentages for each level
#' of a categorical variable across multiple (column) groups, then I would argue
#' that the p-value resulting from a chi square test or a Fisher exact test
#' would be best placed on the line of the table labeling the row group.
#' However, say we reported the minimum, median, mean, and maximum with in a
#' row group for one variable.  The p-value from a t-test, or other meaningful
#' test, for the difference in mean I would suggest should be reported on the
#' line of the summary table for the mean, not the row group itself.
#'
#' With so many possibilities I have reserved construction of a p-value column
#' to be ad hoc.  Perhaps an additional column wouldn't be used and the p-values
#' are edited into row group labels, for example.
#'
#' If you want to add a p-value column to a `qwraps2_summary_table` object you
#' can with some degree of ease.  Note that `qwraps2_summary_table` objects are
#' just character matrices.
both %>% str

#'
#' Let's added p-values for testing the difference in the mean between the three
#' cylinder groups.
# difference in means
mpvals <-
  list(lm(mpg ~ cyl_factor,  data = mtcars2),
       lm(disp ~ cyl_factor, data = mtcars2),
       lm(wt ~ cyl_factor,   data = mtcars2)) %>%
  lapply(aov) %>%
  lapply(summary) %>%
  lapply(function(x) x[[1]][["Pr(>F)"]][1]) %>%
  lapply(frmtp) %>%
  do.call(c, .)

# Fisher test
fpval <- frmtp(fisher.test(table(mtcars2$gear, mtcars2$cyl_factor))$p.value)

#'
#' Adding the p-value column is done as follows:
both <- cbind(both, "P-value" = "")
both[grepl("mean \\(sd\\)", rownames(both)), "P-value"] <- mpvals
a <- capture.output(print(both))
a[grepl("Forward Gears", a)] %<>% sub("&nbsp;&nbsp;\\ \\|$", paste(fpval, "|"), .)

#'
#' and the resulting table is:
#+ results = "asis"
cat(a, sep = "\n")

#'
#' ## Closing Note on `summary_table` and `tab_summary`.
#'
#' I encourage you, the end user, to use `summary_table` primarily, and use
#' `tab_summary` as a quick tool for generating a script.  It might be best if
#' you use `tab_summary` to generate a template of the `formula`e you will want,
#' copy the template into your script and edit accordingly.
#'
#' # Session Info
print(sessionInfo(), local = FALSE)
