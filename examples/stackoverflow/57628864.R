#'
#' Starting with version 0.5.0 of qwraps2, the `summary_table` method has been
#' extended to handle this without building a interaction column.
#'
library(qwraps2)
options(qwraps2_markup = "markdown")

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

st <- summary_table(mtcars, summaries = our_summary1, by = c("vs", "gear"))
st

#'
#' The headers 0.3, 1.3, ... are not that helpful.  There are several options
#' for getting better headers.  One option would be to get human readable
#' character or factor versions of vs an gear, or to modify the output from
#' `summary_table`.
#'
colnames(st) <-
  sub("^(\\d).", "VS = \\1, ", sub("\\.(\\d)", " Gear = \\1", colnames(st) ))

st
