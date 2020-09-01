# /* https://stackoverflow.com/q/54286241/1104685 */
#
#' @phi's answer is correct.  To explian in more detail: the summary is expected
#' to be a list of lists.  That is, a list hwere each element is a list.
#'
#' Let's look at the structure of the provided summary:   (**EDIT:** omitting
#' the `.data` pronoun as it is no longer recommened as of qwraps2 version
#' 0.5.0, released 1 Sept 2020).
#'
#'
summary_tbl1 <-
  list("Gender" =
       list("Female" = ~ qwraps2::n_perc0(gender == 0),
            "Male"   = ~ qwraps2::n_perc0(gender == 1)
           ),
       "Mean age (sd)" = ~ qwraps2::mean_sd(inage),
       "Age categories" =
         list("65-74" = ~ qwraps2::n_perc0(age_cat == 1),
              "75-84" = ~ qwraps2::n_perc0(age_cat == 2),
              "> 85"  = ~ qwraps2::n_perc0(age_cat == 3)
             )
       )

str(summary_tbl1, max.level = 1)

#'
#' The first and thrid elements are lists, but the second element is a formula.
#' The correct specification for the summary is:
#'
#'
summary_tbl1 <-
  list("Gender" =
         list("Female" = ~ qwraps2::n_perc0(gender == 0),
              "Male"   = ~ qwraps2::n_perc0(gender == 1)),
       "inage" =
         list("Mean age (sd)" = ~ qwraps2::mean_sd(inage)),
       "Age categories" =
         list("65-74" = ~ qwraps2::n_perc0(age_cat == 1),
              "75-84" = ~ qwraps2::n_perc0(age_cat == 2),
              "> 85"  = ~ qwraps2::n_perc0(age_cat == 3))
       )

str(summary_tbl1, max.level = 1)

