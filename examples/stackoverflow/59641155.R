#'
#' The object returned by `summary_table` is a character matrix with the
#' additional S3 class `qwraps2_summary_table`. The rowgroup names `Value1` and
#' `Group` are not part of the character matrix explicitly, they are part
#' attributes. The print method for the `qwraps2_summary_table` object builds
#' the table as need for the appropriate markup language, LaTeX or markdown.
#'
#' Two edits the the example posted to get the table you are looking for:
#'
#' 1.  Add `options(qwraps2_markup = "markdown")` to your script. The default
#'     mark up language is LaTeX, setting this option changes the default to
#'     markdown.
#'
#' 2.  Do not wrap `summary_table` inside of `knitr::kable`: this prevents the
#'     needed print method from being called.

options(qwraps2_markup = "markdown")

set.seed(12345)
df <- data.frame(var1  = floor(runif(10,1000000,5000000)),
                 group = rep(c("A","B"),5),
                 event = rep(c("Yes","No"),5))


summary <- list("Value1" =
                list("min" = ~ min(var1),
                     "max" = ~ max(var1),
                     "mean (sd)" = ~ qwraps2::mean_sd(var1)),
                "Group" =
                  list("Yes" = ~ qwraps2::n_perc0(group == "A"),
                       "No"  = ~ qwraps2::n_perc0(group == "B")))


tab <- qwraps2::summary_table(df, summaries = summary, by = "event")
str(tab)

tab
