library(reprex)

reprex({
#' @billudada78, thank you for posting this issue.  You are correct that the
#' cause of the problem is in the qwraps2 code base.
#'
#' A quick reproducible example to reproduce the error:
library(qwraps2)
packageVersion('qwraps2')
library(magrittr)
options(qwraps2_markup = "markdown")

set.seed(42)

by_arm_counts <-
  list("ARM?" =
       list(
            "Active Drug" = ~ qwraps2::n_perc(.data$ARM == "Active"),
            "Placebo" = ~ qwraps2::n_perc(.data$ARM == "Placebo")
           )
      )

#'
#' build a data.frame and have two versions, one with a short name, and one with
#' a long name.
#'
test12345678912345 <- test <-
  data.frame(ARM = sample(c("Active", "Placebo"), size = 200, replace = TRUE),
             USUBJID = sample(LETTERS, size = 200, replace = TRUE))

#' summary table works as expected for a simple call
summary_table(test, by_arm_counts)
summary_table(test12345678912345, by_arm_counts)

#' omitting duplicated rows works for `test`
summary_table(test[!duplicated(test[, c("USUBJID")]), ], by_arm_counts)

#' omitting duplicated rows works for `test12345678912345` results in and error
summary_table(test12345678912345[!duplicated(test12345678912345[, c("USUBJID")]), ], by_arm_counts)

#' A workaround would be to omit the duplicated rows outside of the
#' `summary_table` call.
test12345678912345 %>%
  dplyr::filter(!duplicated(.data$USUBJID)) %>%
  summary_table(., by_arm_counts)

#'
#' The cause of this error is in the `summary_table.data.frame` call were
#' `deparse` is using the default width.cutoff of 60. The argument passed
#' through `summary_table` is exceeds this limit.
nchar('test12345678912345[!duplicated(test12345678912345[, c("USUBJID")]), ]')

#'
#' The result: `deparse` tries, and succeeds in placing a line break, and
#' generates a character vector with length > 1.
foo <- function(x) {
  deparse(substitute(x), backtick = TRUE)
}

str(
    foo(test12345678912345[!duplicated(test12345678912345[, c("USUBJID")]), ])
    )

#'
#' A fix for this will be to add `nlines = 1L` to the `deparse` calls.
#'
foo <- function(x) {
  deparse(substitute(x), nlines = 1L, backtick = TRUE)
}

str(
    foo(test12345678912345[!duplicated(test12345678912345[, c("USUBJID")]), ])
    )

#'
#' I will be posting the patch to kill this bug shortly.
#'

})

# After the fix:
reprex({
library(qwraps2)
packageVersion('qwraps2')
library(magrittr)
options(qwraps2_markup = "markdown")

set.seed(42)

by_arm_counts <-
  list("ARM?" =
       list(
            "Active Drug" = ~ qwraps2::n_perc(.data$ARM == "Active"),
            "Placebo" = ~ qwraps2::n_perc(.data$ARM == "Placebo")
           )
      )

test12345678912345 <- test <-
  data.frame(ARM = sample(c("Active", "Placebo"), size = 200, replace = TRUE),
             USUBJID = sample(LETTERS, size = 200, replace = TRUE))

#' summary table works as expected
summary_table(test, by_arm_counts)
summary_table(test12345678912345, by_arm_counts)
summary_table(test[!duplicated(test[, c("USUBJID")]), ], by_arm_counts)
summary_table(test12345678912345[!duplicated(test12345678912345[, c("USUBJID")]), ], by_arm_counts)

test12345678912345 %>%
  dplyr::filter(!duplicated(.data$USUBJID)) %>%
  summary_table(., by_arm_counts)
})
