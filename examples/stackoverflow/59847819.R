#'
#' A minor correction when building the data set. I used dput() to get the
#' structure of the object. This is a little more robust than using `data.frame`
#' as the mode of each column is explicitly defined.
#'

dat <-
 structure(list(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), type = structure(c(1L,
 2L, 3L, 4L, 1L, 1L, 2L, 3L, 4L, 1L), class = "factor", .Label = c("tct",
 "tcx", "tht", "thx")), mass.g = c(0.03, 0.01, 0.04, 0.06, 0.07,
 0.03, 0.03, 0.01, 0.04, 0.02), size.length = c(8, 6, 5, 6.5,
 5, 5.5, 6, 7, 4, 3), size.width = c(2, 4, 3, 4, 5, 6, 3, 4, 2,
 1)), class = "data.frame", row.names = c(NA, -10L))

#'
#' loading the qwraps2 package and setting the markup language to "markdown".
#' Without this setting the default markup is LaTeX.
#'

library(qwraps2)
options(qwraps2_markup = "markdown")

#'
#' The same summary as in the question posting, but with the missing comma added
#' as noted in Ben's comment.
#'
summary <-
  list("Mass (g)" =
       list(
            "Min" = ~ min(mass.g),
            "Max" = ~ max(mass.g),
            "Median" = ~ median(mass.g),
            "Mean (SD)" = ~ qwraps2::mean_sd(mass.g)),
       "Length (mm)" =
         list(
              "Min" = ~ min(size.length),
              "Max" = ~ median(size.length),
              "Median" = ~ max(size.length),
              "Mean (SD)" = ~ qwraps2::mean_sd(size.length)),
       "Width (mm)" =
         list(
              "Min" = ~ min(size.width),
              "Max" = ~ median(size.width),
              "Median" = ~ max(size.width),
              "Mean (SD)" = ~ qwraps2::mean_sd(size.width)
              ))

#'
#' The summary table appears to render as expected:
#'

by_type <- summary_table(x = dat, summaries = summary, by = "type")
by_type
