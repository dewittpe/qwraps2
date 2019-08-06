library(reprex)

reprex(

{
  library(qwraps2)
  library(dplyr)
  options(qwraps2_markup = "markdown")

  my_mtcars <- dplyr::select(mtcars, mpg, cyl, hp)
  stats_summary <- list("MPG" =
                        list("min" = ~ qwraps2::frmt(min(.data[["mpg"]])),
                             "max" = ~ qwraps2::frmt(max(.data[["mpg"]]))))

  st <- summary_table(dplyr::group_by(my_mtcars, cyl), stats_summary)

  #'
  #' The default table is
  #'
  st

  #'
  #' First question, is it possible to rename the columns, yes.  One way is to
  #' define the column names via the `cname` argument of the print method for
  #' the summary table.
  #'
  print(st, align = "r")

}

,
venue = "gh")
