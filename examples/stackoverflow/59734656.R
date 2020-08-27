#' **EDIT - Updated answer for qwraps2 version 0.5.0**
#'
#' Prior to version 0.5.0 the `summary_table` method depended on the use of the
#' tidyverse and thus the use of the data pronoun `.data` was needed.  As of
#' version 0.5.0 the dependency on the tidyverse has been removed.  To update
#' the answer below to work with version 0.5.0 is

library(qwraps2)
data("iris")
options(qwraps2_markup = "markdown")

our_summary1 <-
  list("Sepal Length" =
       list("min" = ~ min(Sepal.Length),
            "max" = ~ max(Sepal.Length),
            "mean (sd)" = ~ qwraps2::mean_sd(Sepal.Length)),
       "Sepal Width" =
       list("min" = ~ min(Sepal.Width),
            "median" = ~ median(Sepal.Width),
            "max" = ~ max(Sepal.Width),
            "mean (sd)" = ~ qwraps2::mean_sd(Sepal.Width)),
       "Petal Length" =
       list("min" = ~ min(Petal.Length),
            "max" = ~ max(Petal.Length),
            "mean (sd)" = ~ qwraps2::mean_sd(Sepal.Length)),
       "Petal Width" =
       list("min" = ~ min(Petal.Width),
            "max" = ~ max(Petal.Width),
            "mean (sd)" = ~ qwraps2::mean_sd(Petal.Width)),
        "Species" =
       list("Setosa" = ~ qwraps2::n_perc0(Species == "setosa"),
            "Versicolor"  = ~ qwraps2::n_perc0(Species == "versicolor"),
            "Virginica"  = ~ qwraps2::n_perc0(Species == "virginica"))
       )


bytype <- qwraps2::summary_table(iris, our_summary1, by = "Species")
bytype
