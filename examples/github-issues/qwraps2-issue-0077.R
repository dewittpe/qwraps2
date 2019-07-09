library(reprex)

reprex(

{
  #' Simple answers to both questions, yes, and yes.
  #'
  #' Some examples.  First we set up a simple summary table
  #'
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
  print(st, cname = c("Four Cylinders", "Six", "Eight"))

  #'
  #' Another way to rename the columns, and one that might be safer in a dynamic
  #' program is to change the column names of the object.
  #' Look at the structure of the `st` object.  It is a character matrix and the
  #' column names used by the print method when the cname argument is not
  #' provided.
  str(st)
  colnames(st)
  colnames(st) <- gsub("cyl", "Cylinders", colnames(st))
  colnames(st) <- gsub("\\(.*\\)", "", colnames(st))
  st

  #'
  #' Reordering the columns is a little less intuitive right now.  Perhaps I
  #' should extend the package features.  For now, I would suggest building an
  #' factor in your data frame with the levels set in the order you want the
  #' columns to appear in.
  #'
  my_mtcars$my_cyl <- factor(my_mtcars$cyl, levels = c(6, 4, 8), labels = c("Six", "Four", "Eight"))
  summary_table(dplyr::group_by(my_mtcars, my_cyl), stats_summary)
}

,
venue = "gh")
