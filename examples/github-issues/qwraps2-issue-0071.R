library(reprex)

reprex({
#'
#' Version 0.5.0 of qwraps2, which will be published soon, refactored how
#' `summary_table` was implmented.  The changes allow for grouping by multiple
#' variables and does not require use of dplyr.
#'
#' __NOTE__ use of the data pronoun `.data` in the summary is no longer needed
#' or recommended.
library(qwraps2)
options(qwraps2_markup = "markdown")
packageVersion("qwraps2")

our_summary <-
  list(
       "mpg" = list( "min" = ~ min(mpg),
                    "mean (95% CI)" = ~ frmtci(mean_ci(mpg))),
       "hp" = list("min" = ~ min(hp),
                   "max" = ~ max(hp),
                   "mean (95% CI)" = ~ frmtci(mean_ci(hp)))
  )

#' Using the refactored summary_table
by_cyl    <- summary_table(mtcars, our_summary, by = "cyl")
by_am     <- summary_table(mtcars, our_summary, by = "am")
by_cyl_am <- summary_table(mtcars, our_summary, by = c("cyl", "am"))

#+ results = "asis"
by_cyl
by_am
by_cyl_am

#'
#' Producing the same tables using dplyr::group_by
grpby_cyl    <- summary_table(dplyr::group_by(mtcars, cyl),     our_summary)
grpby_am     <- summary_table(dplyr::group_by(mtcars, am),      our_summary)
grpby_cyl_am <- summary_table(dplyr::group_by(mtcars, cyl, am), our_summary)

#+ results = "asis"
grpby_cyl
grpby_am
grpby_cyl_am
},
venue = "gh"
)
