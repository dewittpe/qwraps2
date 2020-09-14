#'
#' The ordering of the columns defaults to the order the levels of the factor.
#'
#' To illustrate this the `mtcars2` data frame is presented with the cylinders
#' column as a factor. The example provided in the vignette intentionally
#' ordered the factor in a non-increasing order to demonstrate the behavior.
#'
#' **Update:** As of version 0.5.0 of qwraps2 the `mtcars2` data set is exported
#' as part of the package and the use of the `.data` pronoun is no longer
#' needed.

library(qwraps2)
options(qwraps2_markup = "markdown")
data(mtcars2, package = "qwraps2")


our_summary1 <-
  list("Miles Per Gallon" =
       list("min" = ~ min(mpg),
            "max" = ~ max(mpg),
            "mean (sd)" = ~ qwraps2::mean_sd(mpg)),
       "Displacement" =
         list("min" = ~ min(disp),
              "median" = ~ median(disp),
              "max" = ~ max(disp),
              "mean (sd)" = ~ qwraps2::mean_sd(disp)),
       "Weight (1000 lbs)" =
         list("min" = ~ min(wt),
              "max" = ~ max(wt),
              "mean (sd)" = ~ qwraps2::mean_sd(wt)),
       "Forward Gears" =
         list("Three" = ~ qwraps2::n_perc0(gear == 3),
              "Four"  = ~ qwraps2::n_perc0(gear == 4),
              "Five"  = ~ qwraps2::n_perc0(gear == 5))
  )

by_cyl <- summary_table(mtcars2, summaries = our_summary1, by = "cyl_factor")
by_cyl

#'
#' If we build the same table, but using the `cyl_character` to group by we will
#' get the columns in a different order based on the default coercion from
#' character to factor.
#'

summary_table(mtcars2, summaries = our_summary1, by = "cyl_character")

