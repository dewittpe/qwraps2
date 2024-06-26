#'---
#'title: "qwraps2: Summary Table"
#'author: "Peter E. DeWitt"
#'output:
#'  rmarkdown::html_vignette:
#'    toc: true
#'    number_sections: true
#'vignette: >
#'  %\VignetteIndexEntry{qwraps2: Summary Table}
#'  %\VignetteEngine{knitr::rmarkdown}
#'  %\VignetteEncoding{UTF-8}
#'---
#'
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
options(qwraps2_markup = "markdown")

#'
#' The
{{ backtick(summary_table) }}
#' method appears to be the most popular and widely used feature of the
{{ CRANpkg(qwraps2) }}
#' package.  As such, this vignette is provided to give as much detail on the
#' use of the method, and the underlying
{{ backtick(qable) }}
#' method for quickly building well formatted summary tables.
#'
#' # qable
#'
{{ backtick(qable) }}
#' builds a formatted character matrix from inputs and then renders a table via
#' knitr::kable.  The primary objective of this function is to allow for easy
#' construction of row groups.
#'
#' ## kable vs qable
#'
#' For a simple example we will use the following data set with a grouping
#' variable, subject id, and two variables, V2, and V3.  For simplicity, we will
#' order the data by group and id as well.
#+ label = 'build_example_data_for_qable'
d <- data.frame(
       group = sample(size = 15, paste0("grp", 1:5), replace = TRUE)
     , id = sample(size = 15, x = LETTERS)
     , V2 = rnorm(15)
     , V3 = rep(c(1, 2, NA), times = 5)
     )
d <- d[order(d$group, d$id), ]

#'
#' Making a simple table via kable:
#'
#+ label = "kable1", results = "asis"
knitr::kable(d, row.names = FALSE)

#'
#' The group column is great for data analysis, but is not the best for human
#' readability.  This is where
{{ backtick(qable) }}
#' can be useful.  Start by building a named numeric column with the name being
#' the row group name and the value the number of rows.  For the _ordered_ data
#' this is a simple call to table:
c(table(d$group))

#'
#' If we pass that named vector to
{{ backtick(qable) }}
#' as the rgroup and with specify the id column as the row names we have the
#' same information but in format that is better for humans:
#'
#+ label = "qable1", results = "asis"
qable(  x = d[, c("V2", "V3")]
      , rgroup = c(table(d$group)) # row group
      , rnames = d$id              # row names
)

#'
#' The return object from
{{ backtick(qable) }}
#' is a character matrix.  Also, when a data.frame is passed to
{{ backtick(qable) }}
#' it is coerced to a matrix before anything else, as such, any formatting of
#' numeric values or other strings should be done before calling
{{ backtick(qable) %s% "."}}
#'
#' To pass arguments to knitr::kable do so via the
{{ backtick(kable_args) }}
#' argument.
#'
#' ## Example: Regression Model Summary Table
#'
#' We will build a summary table for a regression model with row groups for
#' conceptually similar predictors.
#'
#+ results = "asis"
model <-
  glm(spam ~
        word_freq_your + word_freq_conference + word_freq_business +
        char_freq_semicolon + char_freq_exclamation_point +
        capital_run_length_total + capital_run_length_longest
    , data = spambase
    , family = binomial()
  )

model_summary <-
  data.frame(
    parameter = names(coef(model))
  , odd_ratio = frmt(exp(coef(model)), digits = 3)
  , lcl       = frmt(exp(coef(model) + qnorm(0.025) * sqrt(diag(vcov(model)))), digits = 3)
  , ucl       = frmt(exp(coef(model) + qnorm(0.975) * sqrt(diag(vcov(model)))), digits = 3)
  , pval      = frmtp(summary(model)$coef[, 4])
  )

qable(model_summary[-1, c('odd_ratio', 'lcl', 'ucl', 'pval')]
      , rtitle = "Parameter"
      , rgroup = c("Word Frequency" = 3, "Character Frequency" = 2, "Capital Run Length" = 2)
      , rnames = c("Your", "Conference", "Business", ";", "!", "Total", "Longest")
      , kable_args = list(align = "lrrrr", caption = "Regression Model Summary")
      , cnames = c("Odds Ratio", "Lower Conf. Limit", "Upper Conf. Limit", "P-value")
      )

#'
#'
#'
#'
#' # summary_table
#'
{{ backtick(summary_table) }}
#' was developed with the primary objective to build well formatted and easy to
#' read data summary tables.  Conceptually, the construction of these tables
#' start by building a "list-of-lists" of summaries and then generating these
#' summaries for specific groupings of the data set.
#'
#' ## Defining a Summary
#'
#' We will use the
{{ backtick(mtcars2) }}
#' data set for these examples.  We'll start with
#' something very simple and build up to something bigger.
#'
#' Let's report the min, max, and mean (sd) for continuous variables and n (%) for
#' categorical variables.  We will report mpg, displacement (disp), wt (weight),
#' and gear overall and by number of cylinders and transmission type.
#'
#' The use of the
{{ backtick(summary_table) }}
#' use to define a summary, that is, a list-of-lists of formulas for summarizing
#' the data.frame.
#'
#' The inner lists are named formulae defining the wanted
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
#' and rendered in Table \@ref(tab:mtcars_whole).
#'
#+ label = "mtcars2_whole", results = "asis"
whole <-
  summary_table(
    x = mtcars2
  , summaries = our_summary1
  , qable_args = list(kable_args = list(caption = "mtcars2 data summary"))
  )
whole

#'
#' ## Summarize by
#'
#' Use the
{{ backtick(by) }}
#' argument to specify a grouping variable and generate the same summary as
#' above but for subsets of the data.  When the
{{ backtick(by) }}
#' column is a factor, the columns will be in the order of the levels of the
#' factor.  In comparison, the column order
#' is alphabetical if the variable is just a character.
#'
#+ label = "mtcars2_by_cylf", results = "asis"
by_cylf <-
  summary_table(
    x = mtcars2
  , summaries = our_summary1
  , by = c("cyl_factor")
  , qable_args = list(rtitle = "Summary Statistics"
                      , kable_args = list(caption = "mtcars2 data summary by cyl_factor"))
  )
by_cylf

#+ label = "mtcars2_by_cylc", results = "asis"
by_cylc <-
  summary_table(
    x = mtcars2
  , summaries = our_summary1
  , by = c("cyl_character")
  , qable_args = list(rtitle = "Summary Statistics"
                      , kable_args = list(caption = "mtcars2 data summary by cyl_character"))
  )
by_cylc

#'
#' You are also able to generate summaries by multiple columns. For example, Table
#' \@ref(tab:mtcars2_by_cyl_transmission) reports the summary by the combination
#' of the number of cylinders and the type of transmission.
#+ label = "mtcars2_by_cyl_transmission", results = "asis"
by_cyl_am <-
  summary_table(
    x = mtcars2
  , summaries = our_summary1
  , by = c("cyl_factor", "transmission")
  )
by_cyl_am

#'
#' ## cbind summary_table
#'
#' It is common that I will want to have a summary table with the first column
#' reporting for the whole data sets and the additional columns for subsets of
#' the data set.  The returned objects from
{{ backtick(summary_table) }}
#' can be joined together via
{{ backtick(cbind) }}
#' assuming that the row groupings (summaries) are the same.
#'
#' Note: the
{{ backtick(kable_args) }}
#' of the first item passed to
{{ backtick(cbind) }}
#' will be assigned to the resulting object (Table \@ref(tab:mtcars2_cbind)).
#' However, there is an easy way to modify the qable_args and kable_args via the
#' print method.
#'
#+ label = "mtcars2_cbind", results = "asis"
both <- cbind(whole, by_cylf)
both


#'
#' If you want to update how a summary table is printed, you can do so by
#' calling the print method explicitly while passing a new set of
{{ backtick(qable_args) %s% "," }}
#' see Table \@ref(tab:updated_both).
#+ label = "updated_both", results = "asis"

print(both,
      qable_args = list(
        rtitle = "ROW-TITLE",
        cnames = c("Col 0", "Col 1", "Col 2", "Col 3"),
        kable_args = list(
          align = "lcrcr",
          caption = "mtcars2 data summary - new caption"
        )
      ))

#'
#'
#' ## Adding P-values to a Summary Table
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
#' If you want to add a p-value column, or any other column(s) to a
{{ backtick(qwraps2_summary_table) }}
#' object you can with some degree of ease.  Note that
{{ backtick(qwraps2_summary_table) }}
#' objects are just character matrices with additional attributes.
str(both)

#'
#' For this example, we will added p-values for testing the difference in the
#' mean between the three cylinder groups and the distribution of forward gears
#' by cylinder groups.
# difference in means
mpvals <-
  sapply(
         list(mpg  = lm(mpg  ~ cyl_factor, data = mtcars2),
              disp = lm(disp ~ cyl_factor, data = mtcars2),
              wt   = lm(wt   ~ cyl_factor, data = mtcars2)),
         extract_fpvalue)

# Fisher test
fpval <- frmtp(fisher.test(table(mtcars2$gear, mtcars2$cyl_factor))$p.value)

#'
#' In this case, adding the p-value column, is done by creating a empty column
#' and then writing in the needed p-value on the wanted rows.  This could be
#' within a row group (tests for means) or for a row group (Fisher test).
both <- cbind(both, "P-value" = "")
both[grepl("mean \\(sd\\)", both[, 1]), "P-value"] <- mpvals
both[grepl("Forward Gears", both[, 1]), "P-value"] <- fpval

#'
#'
#+ label = "both_with_pvals", results = "asis"
print(both, qable_args = list(kable_args = list(caption = "mtcars2 summary with p-values")))

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
#' ## rbind summary_table
#'
#' There is a rbind method of summary tables.  This can be useful when building
#' a large a table in smaller sections would be advantageous.  For example, it
#' might be helpful to add p-values to a summary table with just one row group
#' and then rbind all the tables together for printing.  Consider that in the
#' above example for adding p-values we have made an assumption that the order
#' of the summary and the
{{ backtick(mpvals) }}
#' will be static. Remembering to make the
#' sequence changes in more than one location can be more difficult than we
#' would like to admit.  Writing code to be robust to such changes is
#' preferable.
#'
t_mpg  <- summary_table(mtcars2, summaries = our_summary1["Miles Per Gallon"], by = "cyl_factor")
t_disp <- summary_table(mtcars2, summaries = our_summary1["Displacement"], by = "cyl_factor")
t_wt   <- summary_table(mtcars2, summaries = our_summary1["Weight (1000 lbs)"], by = "cyl_factor")

t_mpg  <- cbind(t_mpg,  "pvalue" = "")
t_disp <- cbind(t_disp, "pvalue" = "")
t_wt   <- cbind(t_wt,   "pvalue" = "")

t_mpg[ grepl("mean", t_mpg[, 1]),  "pvalue"] <- "mpg-pvalue"
t_disp[grepl("mean", t_disp[, 1]), "pvalue"] <- "disp-pvalue"
t_wt[  grepl("mean", t_wt[, 1]),   "pvalue"] <- "wt-pvalue"

#'
#' Calling rbind now will let us have the table in different sequences without
#' having to worry about the alignment of rows between different elements:
rbind(t_mpg, t_disp, t_wt)
rbind(t_wt, t_disp, t_mpg)

#'
#'
#' ## Using Variable Labels
#'
#' Some data management paradigms will use attributes to keep a label associated
#' with a variable in a data.frame.  Notable examples are the
{{ CRANpkg(Hmisc) }}
#' and
{{ CRANpkg(sjPlot) %s% "." }}
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
#' This behavior is also seen with the
{{ backtick(summary_table) }}
#' call.
#+ results = "asis"
summary_table(new_data_frame)

#'
#' ## Alternative building of the summaries
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
#+ label="summary_table_mtcars2_default", results = "asis"
summary_table(mtcars2[, c("mpg", "cyl_factor", "wt")])

#'
#' Now, say we want to only report the minimum and maximum for each of the
#' numeric variables and for the categorical variables we want two show the
#' denominator for each category and for the percentage, to one digit with the
#' percent symbol in the table.
#' Note that when defining the list of numeric_summaries that the argument place
#' holder is the
{{ backtick("%s%", dequote = TRUE) }}
#' character.
new_summary <-
  qsummary(mtcars2[, c("mpg", "cyl_factor", "wt")],
           numeric_summaries = list("Minimum" = "~ min(%s)",
                                    "Maximum" = "~ max(%s)"),
           n_perc_args = list(digits = 1, show_symbol = TRUE, show_denom = "always"))
str(new_summary)

#'
#' The resulting table is:
#+ results = "asis"
summary_table(mtcars2, new_summary)

#'
#'
#' # Session Info
#'
print(sessionInfo(), local = FALSE)
