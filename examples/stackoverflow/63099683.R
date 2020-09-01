# /* answer for https://stackoverflow.com/q/63099683/1104685 */
#'
#' duckmayr's answer addresses the posted question.  However, I would be
#' remiss if I did not draw attention to a major error in the construction of
#' `summary_statistics`.  Every summary statistic is defined by
#' `myData$<variable>` which means that the grouping function fails to look at
#' the results within the group (city name).  Look at the values in the table -
#' the same summary statistics are in each and every column.
#'
#' I expect part of the problem is that prior to version 0.5.0 of qwraps2 the
#' use of the data pronoun `.data` was suggests/needed.  However, as of the
#' release of version 0.5.0 (published on CRAN on 1 September 2020) the
#' `summary_table` method have been refactored to avoid the use of the data
#' pronoun `.data` and to support, but not require, `dplyr`.
#'
#' For example, let's build and example data set:
set.seed(42)
myData <-
  data.frame(city          = gl(n = 4, k = 25, labels = c("Eilat", "Jerusalem", "Metula", "TelAviv")),
             hobby_hr_week = rpois(n = 100, lambda = 15.54),
             work_hr_week  = rpois(n = 100, lambda = 30.34),
             wellness      = rnorm(n = 100, mean = -56.11, sd = 100.01),
             RU_happy      = rbinom(n = 100, size = 1, p = 0.35))

#'
#' Load and attach qwraps2
library(qwraps2)
options(qwraps2_markup = "markdown")
packageVersion("qwraps2")

#'
#' Defining the `summary_table` as in the original question post:
summary_statistics <-
  list(
    "Hobby(hours/week)" =
      list(
           "mean (sd)" = ~ qwraps2::mean_sd(myData$hobby_hr_week, na_rm = TRUE),
           "min"       = ~ min(myData$hobby_hr_week, na.rm = TRUE),
           "max"       = ~ max(myData$hobby_hr_week, na.rm = TRUE)
           ),
       "Work(hours/week)" =
         list(
              "mean (sd)" = ~ qwraps2::mean_sd(myData$work_hr_week, na_rm = TRUE),
              "min"       = ~ min(myData$work_hr_week, na.rm = TRUE),
              "max"       = ~ max(myData$work_hr_week, na.rm = TRUE)
              ),
       "Wellness" =
         list(
              "mean (sd)" = ~ qwraps2::mean_sd(myData$wellness, na_rm = TRUE),
              "min"       = ~ min(myData$wellness, na.rm = TRUE),
              "max"       = ~ max(myData$wellness, na.rm = TRUE)
              ),

       "Happiness" =
         list(
              "Happiness" = ~qwraps2::n_perc(myData$RU_happy)

         )
  )

#'
#' we'll get a table, as posted in the original question, but again, the value
#' are incorrect, they are the value for the whole data set because `myData`
#' prefaces each of the variable names.
#'
summary_table(myData, summary_statistics)

#'
#'
summary_table(myData, summary_statistics, by = "city")

#'
#' To get the correct summary statistics for each city only use the variable
#' name.
summary_statistics <-
  list(
    "Hobby(hours/week)" =
      list(
           "mean (sd)" = ~ qwraps2::mean_sd(hobby_hr_week, na_rm = TRUE),
           "min"       = ~ min(hobby_hr_week, na.rm = TRUE),
           "max"       = ~ max(hobby_hr_week, na.rm = TRUE)
           ),
       "Work(hours/week)" =
         list(
              "mean (sd)" = ~ qwraps2::mean_sd(work_hr_week, na_rm = TRUE),
              "min"       = ~ min(work_hr_week, na.rm = TRUE),
              "max"       = ~ max(work_hr_week, na.rm = TRUE)
              ),
       "Wellness" =
         list(
              "mean (sd)" = ~ qwraps2::mean_sd(wellness, na_rm = TRUE),
              "min"       = ~ min(wellness, na.rm = TRUE),
              "max"       = ~ max(wellness, na.rm = TRUE)
              ),

       "Happiness" =
         list(
              "Happiness" = ~qwraps2::n_perc(RU_happy)

         )
  )

#'
#' And the updated table:
print(
      summary_table(myData, summary_statistics, by = "city"),
      rtitle = "Summary Statistics Table for the Wellness Data Set"
)

#+ echo = FALSE, results = "asis"
print(
      summary_table(myData, summary_statistics, by = "city"),
      rtitle = "Summary Statistics Table for the Wellness Data Set"
)
