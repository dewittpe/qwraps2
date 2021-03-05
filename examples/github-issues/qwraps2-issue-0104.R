#'
#' This issue was originally posted on
#' [stackoverflow](https://stackoverflow.com/q/66362954/1104685)
#'
#' When there is missing data in a data set, some of the rows in a grouped
#' summary table may not generate correctly and through an error.  Here is a
#' reproducible example:
#'
library(qwraps2)
options(qwraps2_markup = "markdown")

summary1 <-
  list("Time-To-event" =
       list("min"       = ~ min(x),
            "max"       = ~ max(x),
            "mean (sd)" = ~ qwraps2::mean_sd(x, na_rm =F )),
       "Case Status" =
       list("Dead" = ~ qwraps2::n_perc0(y == 1),
            "Alive/Lost-to-follow"  = ~ qwraps2::n_perc0(y == 0)
            )
       )

dat <-  data.frame(x =c(1:10, rep(NA, 10)) ,y =rep(c(0,1), 10) ,group = c(rep("A", 3), rep("B", 17)))

summary_table(dplyr::group_by(dat, group), summary1)

#'
#' The error occurs using the updated qwraps2 api
summary_table(dat, summaries = summary1, by = "group")

#'
#' There is a notable issue with the min and max rows, e.g.,

summary_table(subset(dat, group == "A"), summary1)
summary_table(subset(dat, group == "B"), summary1)

#'
#' The missing values for x which are in group B result in rows being omitted
#' from the output.  This can be corrected by using the na.rm argument in the
#' min and max calls.
summary2 <- summary1
summary2[["Time-To-event"]][["min"]] <- ~ min(x, na.rm = TRUE)
summary2[["Time-To-event"]][["max"]] <- ~ max(x, na.rm = TRUE)

summary_table(dat, summary2, by = "group")


