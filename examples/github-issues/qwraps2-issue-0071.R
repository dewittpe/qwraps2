#'
#' A work around, at this time, would be use `interaction` within the `group_by`
#' call.  The `summary_table` table code assumes that there is only one variable
#' being used for the grouping.  Here is an example:
#+ message = FALSE
library(dplyr)
library(qwraps2)
options(qwraps2_markup = "markdown")

our_summary <-
  list(
       "mpg" = list( "min" = ~ min(.data$mpg),
                    "mean (95% CI)" = ~ frmtci(mean_ci(.data$mpg))),
       "hp" = list("min" = ~ min(.data$hp),
                   "max" = ~ max(.data$hp),
                   "mean (95% CI)" = ~ frmtci(mean_ci(.data$hp)))
  )


by_color <- summary_table(dplyr::group_by(mtcars, cyl), summaries = our_summary)
by_cut   <- summary_table(dplyr::group_by(mtcars, am), summaries = our_summary)

#+ results = "asis"
summary_table(dplyr::group_by(mtcars, interaction(cyl, am)), summaries = our_summary)


#'
#' To help with the default output it might be helpful to define a variable in
#' the `data.frame` for the grouping.  This is not dissimilar to how `group_by`
#' in dplyr v0.8.0 works.

mtcars$Engine <-
  case_when(
            mtcars$cyl == 4 & mtcars$am == 0 ~ "Four Cylinders, Manual",
            mtcars$cyl == 6 & mtcars$am == 0 ~ "Six Cylinders, Manual",
            mtcars$cyl == 8 & mtcars$am == 0 ~ "Eight Cylinders, Manual",
            mtcars$cyl == 4 & mtcars$am == 1 ~ "Four Cylinders, Automatic",
            mtcars$cyl == 6 & mtcars$am == 1 ~ "Six Cylinders, Automatic",
            mtcars$cyl == 8 & mtcars$am == 1 ~ "Eight Cylinders, Automatic")

#+ results = "asis"
summary_table(group_by(mtcars, Engine), our_summary)

