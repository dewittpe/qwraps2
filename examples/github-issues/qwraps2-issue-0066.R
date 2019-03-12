#'
#' @raheems, `qsummary` does a better job of handling environments and that will
#' fundamentally change the programming style.

library(qwraps2)  # version 0.3.0.9003
library(magrittr)
options(qwraps2_markup = "markdown")

#'
#' The provided example:
mtcars2 <-
  dplyr::mutate(mtcars,
                cyl_factor = factor(cyl,
                                    levels = c(6, 4, 8),
                                    labels = paste(c(6, 4, 8), "cylinders")),
                cyl_character = paste(cyl, "cylinders"))

our_summary2 <-
  with(mtcars2,
       list("Miles Per Gallon" = tab_summary(mpg)[c(1, 4, 3)],
            "Displacement (default summary)" = tab_summary(disp),
            "Displacement" = c(tab_summary(disp)[c(1, 4, 3)],
                               "mean (95% CI)" = ~ frmtci(qwraps2::mean_ci(disp))),
            "Weight (1000 lbs)" = tab_summary(wt)[c(1, 4, 3)],
            "Forward Gears" = tab_summary(as.character(gear))
            ))

#'
#+ results = "asis"
summary_table(mtcars2, our_summary2)


#'
#' The `tab_summary()` default for numeric values was to report the minimum,
#' median (IQR), mean (sd), and the maximum, in that order.  The `qsummary` has
#' the same defualts.  The following two call generate a similar object.
suppressWarnings(with(mtcars2, list("Miles Per Gallon" = tab_summary(mpg))))
qsummary(mtcars2["mpg"]) %>% setNames(., "Miles Per Gallon")

#'
#' So, to get the min, max, mean (sd) you could do the following:
suppressWarnings(with(mtcars2, list("Miles Per Gallon" = tab_summary(mpg)[c(1, 4, 3)])))

list("Miles Per Gallon" = qsummary(mtcars2["mpg"])[[1]][c(1, 4, 3)])

#'
#' To replicate the `our_summary2` object I would likely, in practice, write out
#' the explicit parts that I need.  I love having dynamic solutions, but I've
#' found time and time again with summary tables there is a limit to the level
#' of automation that can be made before something breaks.  Usually not withing
#' a project, but certainly between projects.
#'
#' That said, If we modify the `mtcars2$gear` to be factor we should be able to
#' recreate `our_summary2` from above as follows:

mtcars2$gear %<>% factor(levels = c(3, 4, 5), labels = c("Three", "Four", "Five"))

#'
#' only the needed columns to `qsummary`
new_summary <-
  setNames(
           qsummary(mtcars2[c("mpg", "disp", "disp", "wt", "gear")])
           ,
           c("Miles Per Gallon",
             "Displacement (default summary)",
             "Displacement",
             "Weight (1000 lbs)",
             "Forward Gears"))

#'
#' Remove the  median (IQR) rows and set the order of min, max, mean (sd) for
#' three of the row groups
for (r in c("Miles Per Gallon", "Displacement", "Weight (1000 lbs)")) {
  new_summary[[r]] %<>% .[c(1, 4, 3)]
}

#'
#' Add in the mean and CI for the second summary of displacement.  Note that
#' `disp.1` is the variable name hear because of the construction method.
new_summary$`Displacement`$`mean (95% CI)` = ~ qwraps2::frmtci(qwraps2::mean_ci(disp.1))
new_summary


#'
#' The resulting table is:
#+ results = "asis"
summary_table(mtcars2[c("mpg", "disp", "disp", "wt", "gear")], new_summary)



#'
#' Again, I would, in practice, just write out each wanted row.  The PIs I work
#' with/for change their minds so often about the summaries they want it is
#' easier for me to have the rows explicit and comment them in or out as needed.
#'

new_summary2 <-
  list("Miles Per Gallon" = 
       list(
            "min" = ~ min(.data$mpg)
            ,
            "max" = ~ max(.data$mpg)
            ,
            "mean (sd)" = ~ mean_sd(.data$mpg)
            )
       , 
       "Displacement (default summary)" = 
       list(
            "min" = ~ min(.data$disp)
            ,
            "median (IQR)" = ~ median_iqr(.data$disp)
            ,
            "mean (sd)" = ~ mean_sd(.data$disp)
            ,
            "max" = ~ max(.data$disp)
            )
       ,
       "Displacement" = 
       list(
            "min" = ~ min(.data$disp)
            ,
            "max" = ~ max(.data$disp)
            ,
            "mean (sd)" = ~ mean_sd(.data$disp)
            ,
            "mean (95% CI)" = ~ frmtci(qwraps2::mean_ci(.data$disp))
            )
       ,
       "Weight (1000 lbs)" = 
       list(
            "min" = ~ min(.data$mpg)
            ,
            "max" = ~ max(.data$mpg)
            ,
            "mean (sd)" = ~ mean_sd(.data$mpg)
            )
       , 
       "Forward Gears" = 
       list(
            "Three" = ~ n_perc0(.data$gear == "Three")
            ,
            "Four" = ~ n_perc0(.data$gear == "Four")
            ,
            "Five" = ~ n_perc0(.data$gear == "Five")
            )
       )

#'
#+ results = "asis"
summary_table(mtcars2, new_summary2)
       




