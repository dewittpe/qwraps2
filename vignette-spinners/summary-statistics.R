#'---
#'title: "Formatted Summary Statistics and Data Summary Tables with qwraps2"
#'author: "Peter E. DeWitt"
#'output:
#'  rmarkdown::html_vignette:
#'    toc: true
#'    number_sections: true
#'vignette: >
#'  %\VignetteIndexEntry{Formatted Summary Statistics and Data Summary Tables with qwraps2}
#'  %\VignetteEngine{knitr::rmarkdown}
#'  %\VignetteEncoding{UTF-8}
#'---
#'
#+ label = "setup", include = FALSE
knitr::opts_chunk$set(collapse = TRUE)
#'
set.seed(42)
# /*  if interactive load_all, else, a library(qwraps2) is called below for the
# vignette
if (interactive()) {
  devtools::load_all()
} else {
# */
library(qwraps2)
# /*
}
# */
# define the markup language we are working in.
# options(qwraps2_markup = "latex") is also supported.
options(qwraps2_markup = "markdown")

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
#' This vignette presents the use of the
{{ paste0(backtick(summary_table), ",") }}
{{ paste0(backtick(qsummary), ",") }}
#' and
{{ backtick(qable) }}
#' functions for quickly building data summary tables.  We will be using summary
#' statistic functions,
{{ paste0(backtick(mean_sd), ",") }}
{{ paste0(backtick(median_iqr), ",") }}
{{ paste0(backtick(n_perc), ",") }}
#' and others, from
{{ CRANpkg(qwraps2) }}
#' as well.
#'
#' ## Prerequisites Example Data Set
# /*
while (FALSE) {
# */
library(qwraps2)
# /*
}
# */
#'
#' We will use the data set
{{ backtick(mtcars2) }}
#' for the examples throughout this vignette
#' data set for examples throughout this vignette.
{{ backtick(mtcars2) }}
#' is a modified and extended version of the base R data set
{{ paste(backtick(mtcars), ".") }}
#' For details on the construction of the
{{ backtick(mtcars2) }}
#' data set please view the vignette:
{{ backtick(vignette('mtcars', package = "qwraps2")) }}
data(mtcars2)
str(mtcars2)

#'
#' # Review of Summary Statistic Functions and Formatting
#'
#'
#' ## Means and Standard Deviations
#'
{{ backtick(mean_sd) }}
#' returns the (arithmetic) mean and standard deviation for numeric
#' vector as a formatted character string. For example,
{{ backtick(mean_sd(mtcars2$mpg)) }}
#' returns the formatted string
{{ paste0(mean_sd(mtcars2$mpg), ".") }}
#' There are other options for formatting character string:
mean_sd(mtcars2$mpg)
mean_sd(mtcars2$mpg, denote_sd = "paren")

#'
#' ## Mean and Confidence intervals
#'
#' If you need the mean and a confidence interval there is the function
{{ paste0(backtick(mean_ci), ".") }}
#' which returns a
{{ backtick(qwraps2_mean_ci) }}
#' object which is a
#' named vector with the mean, lower confidence limit, and the upper confidence
#' limit.   The printing method for
{{ backtick(qwraps2_mean_ci) }}
#' objects is a call to the
{{ backtick(frmtci) }}
#' function.  You an modify the formatting of printed result by adjusting
#' the arguments pasted to
{{ paste0(backtick(frmtci), ".") }}
mci <- mean_ci(mtcars2$mpg)
str(mci)
mci
print(mci, show_level = TRUE)

#'
#' ## Median and Inner Quartile Range
#'
#' Similar to the
{{ backtick(mean_sd) }}
#' function, the
{{ backtick(median_iqr) }}
#' returns the median and the inner quartile range (IQR) of a data vector.
median_iqr(mtcars2$mpg)

#'
#' ## Count and Percentages
#'
#' The
{{ backtick(n_perc) }}
#' function is the workhorse.
{{ backtick(n_perc0) }}
#' is also provided for ease of use in the same way that base R has
{{ backtick(paste) }}
#' and
{{ paste(backtick(paste0), ".") }}
{{ backtick(n_perc) }}
#' returns the n (%) with the percentage sign in the string,
{{ backtick(n_perc0) }}
#' omits the percentage sign from the string.  The latter is good for tables,
#' the former for in-line text.
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
#' deviation are
#'
#' $$
#' \begin{equation}
#'   \mu_g = \left( \prod_{i = 1}^{n} x_i \right)^{\frac{1}{n}} = b^{ \sum_{i =
#'   1}^{n} \log_{b} x_i },
#' \end{equation}
#' $$
#' and
#' $$
#' \begin{equation}
#'   \sigma_g = b ^ {
#'   \sqrt{
#'     \frac{\sum_{i = 1}^{n} \left( \log_{b} \frac{x_i}{\mu_g} \right)^2}{n}
#'   }
#'   }
#' \end{equation}
#' $$
#' or, for clarity,
#' $$
#' \begin{equation}
#'   \log_{b} \sigma_g =
#'   \sqrt{ \frac{\sum_{i = 1}^{n} \left( \log_{b} \frac{x_i}{\mu_g}
#'   \right)^2}{n}}
#' \end{equation}
#' $$
#'
#' When looking for the geometric standard deviation in R, the simple
{{ backtick(exp(sd(log(x)))) }}
#' is not exactly correct.  The geometric standard deviation
#' uses $n,$ the full sample size, in the denominator, where as
#' the
{{ backtick(sd) }}
#' and
{{ backtick(var) }}
#' functions in R use the denominator $n - 1.$  To get
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
#' The functions
{{ paste0(backtick(gmean), ",") }}
{{ paste0(backtick(gvar), ",") }}
#' and
{{ backtick(gsd) }}
#' provide the geometric mean, variance, and standard deviation for a numeric
#' vector.
gmean(x)
all.equal(gmean(x), mu_g)

gvar(x)
all.equal(gvar(x), sigma_g^2)  # This is supposed to be FALSE
all.equal(gvar(x), exp(log(sigma_g)^2))

gsd(x)
all.equal(gsd(x), sigma_g)

#'
{{ backtick(gmean_sd) }}
#' will provide a quick way for reporting the geometric mean and
#' geometric standard deviation in the same way that
{{ backtick(mean_sd) }}
#' does for the arithmetic mean and arithmetic standard deviation:
gmean_sd(x)

#'
#'
#'
#' # Building a Data Summary Table
#'
#' Objective: build a table reporting summary statistics for some of the variables
#' in the
{{ backtick(mtcars2) }}
#' `data.frame` overall and within subgroups.  We'll start with
#' something very simple and build up to something bigger.
#'
#' Let's report the min, max, and mean (sd) for continuous variables and n (%) for
#' categorical variables.  We will report mpg, displacement (disp), wt (weight),
#' and gear overall and by number of cylinders and transmission type.
#'
#' **END USER VISIBLE CHANGE:** for qwraps2 version before 0.4.2 the
{{ backtick(summary_table) }}
#' method relied on
{{ CRANpkg(dplyr) }}
#' verbs for the implementation and end user specifications.  This created
#' several limitations and required what could be considered a non-intuitive api
#' due to the use of the rlang data pronoun
{{ paste0(backtick(.data), ".") }}
#' Building a table with the summary based on a grouping, e.g., mpg by number of
#' cylinders, was achieved by the use of
{{ backtick(dplyr::group_by) }}
#' to specify the group.  Further only one grouping variable was supported.
#' Starting with version 0.5.0 the implementation of the
{{ backtick(summary_table) }}
#' and
{{ backtick(qsummary) }}
#' is based on base R methods.  The change in the implementation will make it
#' easier for all users as the use of the tidyverse is no longer required or
#' assumed.  The use of
{{ backtick(dplyr::group_by) }}
#' is still supported, and has been improved.
#'
#' There are two changes to the API for defining summaries:
#' 1. Use of the rlang data pronoun
{{ paste0(backtick(.data), ".") }}
#' is no longer recommend / supported. Version 0.5.3 a warning will be given
#' noting that the use of the pronoun _might_ result in unexpected behavior.
#' 2. A new function argument
{{ backtick(by) }}
#' as been added to the
{{ backtick(summary_table) }}
#' method such that the use of
{{ backtick(dplyr::group_by) }}
#' is no longer needed.
#'
#' The use of the
{{ backtick(summary_table) }}
#' use to define a summary, that is, a list-of-lists of formulas for summarizing
#' the data.frame.
#'
#' The inner lists are named `formula`e defining the wanted
#' summary.  The names are important, as they are used to label row groups and row
#' names in the table.
#'
our_summary1 <-
  list("Miles Per Gallon" =
       list("min"       = ~ min(mpg),
            "max"       = ~ max(mpg),
            "mean (sd)" = ~ qwraps2::mean_sd(mpg)),
       "Displacement" =
       list("min"       = ~ min(disp),
            "median"    = ~ median(disp),
            "max"       = ~ max(disp),
            "mean (sd)" = ~ qwraps2::mean_sd(disp)),
       "Weight (1000 lbs)" =
       list("min"       = ~ min(wt),
            "max"       = ~ max(wt),
            "mean (sd)" = ~ qwraps2::mean_sd(wt)),
       "Forward Gears" =
       list("Three" = ~ qwraps2::n_perc0(gear == 3),
            "Four"  = ~ qwraps2::n_perc0(gear == 4),
            "Five"  = ~ qwraps2::n_perc0(gear == 5))
       )

#'
#' Building the table is done with a call to
{{ backtick(summary_table) }}
#'
#+ results = "asis"
### Overall
whole <- summary_table(mtcars2, our_summary1)
whole

#qable(whole[[1]], rgroup = attr(whole[[1]], "rgroup"), rtitle = "r", cnames = )

### By number of Cylinders
by_cyl <- summary_table(mtcars2, summaries = our_summary1, by = c("cyl_factor"))
by_cyl

#'
#' With the refactor of the
{{ backtick(summary_table) }}
#' method in version 0.5.0 it is easier to group by multiple variables.  For
#' example, getting a column for combination of cylinders and transmission type:
#+
by_cyl_am <- summary_table(mtcars2, summaries = our_summary1, by = c("cyl_factor", "am"))
by_cyl_am

#'
#' To report a table with both the whole sample summary and conditional columns
#' together:
#+ results = "asis"
both <- cbind(whole, by_cyl)
both

#'
#' If you want to change the column names, do so via the
{{ backtick(cnames) }}
#' argument to
{{ backtick(qable) }}
#' via the print method for
{{ backtick(qwraps2_summary_table) }}
#' objects.  Any argument
#' that you want to send to
{{ backtick(qable) }}
#'  can be sent there when explicitly using the
{{ backtick(print) }}
#' method for
{{ backtick(qwraps2_summary_table) }}
#' objects.
#+ results = "asis"
print(both,
      rtitle = "Summary Statistics",
      cnames = c("Col 0", "Col 1", "Col 2", "Col 3"))

#'
#' ## Easy building of the summaries
#'
#' The task of building the
{{ backtick(summaries) }}
#' list-of-lists can be tedious. The function
{{ backtick(qummaries) }}
#' is designed to make it easier.
{{ backtick(qummaries) }}
#' will use a set of predefined
#' functions to summarize numeric columns of a data.frame, a set of arguments
#' to pass to
{{ backtick(n_perc) }}
#' for categorical (character and factor) variables.
#'
#' By default, calling
{{ backtick(summary_table) }}
#' will use the default summary metrics
#' defined by
{{ paste0(backtick(qsummary), ".") }}
#' The purpose of
{{ backtick(qsummary) }}
#' is to provide the same
#' summary for all numeric variables within a data.frame and a single style of
#' summary for categorical variables within the data.frame.  For example, the
#' default summary for a set of variables from the
{{ backtick(mtcars2) }}
#' data set is
qsummary(mtcars2[, c("mpg", "cyl_factor", "wt")])

#'
#' That default summary is used for a table as follows:
#+label="summary_table_mtcars2_default", results = "asis"
summary_table(mtcars2[, c("mpg", "cyl_factor", "wt")])

#'
#' Now, say we want to only report the minimum and maximum for each of the
#' numeric variables and for the categorical variables we want two show the
#' denominator for each category and for the percentage, to one digit with the
#' percent symbol in the table.
#' Note that when defining the list of numeric_summaries that the argument place
#' holder is the `%s` character.
new_summary <-
  qsummary(mtcars2[, c("mpg", "cyl_factor", "wt")],
           numeric_summaries = list("Minimum" = "~ min(%s)",
                                    "Maximum" = "~ max(%s)"),
           n_perc_args = list(digits = 1, show_symbol = TRUE, show_denom = "always"))
str(new_summary)

#'
#' The resulting table is:
#+results = "asis"
summary_table(mtcars2, new_summary)

#'
#' The summary can easily be used with a
{{ backtick(by) }}
#' argument
#+results = "asis"
summary_table(mtcars2, new_summary, by = c("cyl_factor"))

#'
#'
#'
#' ## Adding P-values to a Summary Table
#'
#' Starting with qwraps2 version 0.6, the task of adding p-values to a summary
#' table has been made considerably easier.  This is due to a change in how the
#' function
{{ backtick(qable) }}
#' generated the structured matrix.  Older versions of
{{ backtick(qable) }}
#' generated a structured matrix and passed that matrix to
{{ backtick(knitr::kable) }}
#' and returned the formatted character string.  In version 0.6
{{ backtick(qable) }}
#' was changed to return the structured matrix and a separate print method was
#' added to make the call to
{{ backtick(knitr::kable) %s% "."}}
#'
#' There are many different ways to format data summary tables. Adding
#' p-values to a table is just one thing that can be done in more than one way.
#' For example, if a row group reports the counts and percentages for each level
#' of a categorical variable across multiple (column) groups, then I would argue
#' that the p-value resulting from a chi square test or a Fisher exact test
#' would be best placed on the line of the table labeling the row group.
#' However, say we reported the minimum, median, mean, and maximum with in a
#' row group for one variable.  The p-value from a t-test, or other meaningful
#' test for the difference in mean, I would suggest should be reported on the
#' line of the summary table for the mean, not the row group itself.
#'
#' With so many possibilities I have reserved construction of a p-value column
#' to be ad hoc.  Perhaps an additional column wouldn't be used and the p-values
#' are edited into row group labels, for example.
#'
#' If you want to add a p-value column to a
{{ backtick(qwraps2_summary_table) }}
#' object you can with some degree of ease.  Note that
{{ backtick(qwraps2_summary_table) }}
#' objects are just character matrices.
str(both)

#'
#' Let's added p-values for testing the difference in the mean between the three
#' cylinder groups.
# difference in means
mpvals <-
  sapply(
         list(lm(mpg ~ cyl_factor,  data = mtcars2),
              lm(disp ~ cyl_factor, data = mtcars2),
              lm(wt ~ cyl_factor,   data = mtcars2)),
         extract_fpvalue)

# Fisher test
fpval <- frmtp(fisher.test(table(mtcars2$gear, mtcars2$cyl_factor))$p.value)

#'
#' Adding the p-value column is done as follows:
both <- cbind(both, "P-value" = "")
both[grepl("mean \\(sd\\)", both[, 1]), "P-value"] <- mpvals
both[grepl("Forward Gears", both[, 1]), "P-value"] <- fpval

#'
#' and the resulting table is:
#+ results = "asis"
both

#'
#' Another option you might consider is to have the p-value in the row group
#' name.  Consider the following construction.  The p-values are added to the
#' names of the row groups when building the summary table.
#+ results = "asis"
gear_summary <-
  list("Forward Gears" =
       list("Three" = ~ qwraps2::n_perc0(gear == 3),
            "Four"  = ~ qwraps2::n_perc0(gear == 4),
            "Five"  = ~ qwraps2::n_perc0(gear == 5)),
       "Transmission" =
       list("Automatic" = ~ qwraps2::n_perc0(am == 0),
            "Manual"    = ~ qwraps2::n_perc0(am == 1))
       )

gear_summary <-
setNames(gear_summary,
         c(
         paste("Forward Gears: ", frmtp(fisher.test(xtabs( ~ gear + cyl_factor, data = mtcars2))$p.value)),
         paste("Transmission: ",  frmtp(fisher.test(xtabs( ~ am + cyl_factor, data = mtcars2))$p.value)))
         )

summary_table(mtcars2, gear_summary, by = "cyl_factor")

#'
#' ## Using Variable Labels
#'
#' Some data management paradigms will use attributes to keep a label associated
#' with a variable in a data.frame.  Notable examples are the
{{ CRANpkg(Hmisc) }}
#' and
{{ paste0(CRANpkg(sjPlot), ".") }}
#' If you associate a label with a variable in the data frame the that label
#' will be used when building a summary table.  This feature was suggested
#' https://github.com/dewittpe/qwraps2/issues/74 and implemented thusly:

new_data_frame <-
  data.frame(age = c(18, 20, 24, 17, 43),
             edu = c(1, 3, 1, 5, 2),
             rt  = c(0.01, 0.04, 0.02, 0.10, 0.06))

# Set a label for the variables
attr(new_data_frame$age, "label") <- "Age in years"
attr(new_data_frame$rt,  "label") <- "Reaction time"

# mistakenly set the attribute to name instead of label
attr(new_data_frame$edu, "name") <- "Education"

#'
#' When calling
{{ backtick(qsummary) }}
#' the provide labels for the age and rt variables will
#' be used.  Since the attribute "label" does not exist for the edu variable,
#' edu will be used in the output.
qsummary(new_data_frame)

#'
#' This behavior is also seen with the `summary_table` call
#+ results = "asis"
summary_table(new_data_frame)

#'
#'
#'
#'
#' # Session Info
#'
print(sessionInfo(), local = FALSE)





