#'
#' Hi @estephanmoana, thank you for the suggestions.  I am unable to reproduce your example.  In the future you may find the [reprex](https://cran.r-project.org/package=reprex) package helpful for producing reproducable examples and posts for github issues.
#'
#' I think the idea behind your comment is covered by the options in `qsummary`.  You can define the summary stats for numeric variables and options for how `n_perc` will display counts and pecentages for characters, factors, and logicals.
library(qwraps2)
packageVersion("qwraps2")
options(qwraps2_markup = "markdown")
set.seed(42)

#' Define a function for finding the coeffient of variation
cv <- function(x, na_rm = FALSE) {
  m <- mean(x, na.rm = na_rm)
  s <- sd(x, na.rm = na_rm)
  s / m
}

#' example cv
cv(rnorm(100, mean = 12, sd = 2))

#'
#' extend the mtcars data.frame to include a logial vector, a character vector,
#' and a factor vector.

mtcars2 <- mtcars[, c("mpg", "cyl", "am", "gear", "hp", "wt")]
mtcars2[["MPG > 25"]] <- mtcars2$mpg > 25                       # logical
mtcars2[["gear"]] <- factor(mtcars2$gear, levels = c(3, 4, 5), labels = c("Three Gears", "Four Gears", "Five Gears"))

#' The list-of-lists of formula for the summary is generated thusly
qs <- qsummary(mtcars2,
               numeric_summaries = list("min" = "~ qwraps2::frmt(min(%s))",
                                        "mean and sd" = "~ qwraps2::mean_sd(%s)",
                                        "cv"  = "~ qwraps2::frmt(cv(%s))"),
               n_perc_args = list(digits = 4, show_symbol = TRUE))
qs

#' A simple table:
#+ results = "asis"
summary_table(mtcars2, qs)

#'
#' When grouping, I would drop the summary of the grouping var
#+ results = "asis"
summary_table(mtcars2, qs[-which(names(qs) == "cyl")], by = c("cyl"))
summary_table(mtcars2, qs[-which(names(qs) %in% c("cyl", "am"))], by = c("cyl", "am"))

