@billudada78, thank you for posting this issue. You are correct that the
cause of the problem is in the qwraps2 code base.

A quick reproducible example to reproduce the error:

    library(qwraps2)
    packageVersion('qwraps2')
    #> [1] '0.4.2.9006'
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

build a data.frame and have two versions, one with a short name, and one with
a long name.

    test12345678912345 <- test <-
      data.frame(ARM = sample(c("Active", "Placebo"), size = 200, replace = TRUE),
                 USUBJID = sample(LETTERS, size = 200, replace = TRUE))

summary table works as expected for a simple call

    summary_table(test, by_arm_counts)
    #> Warning in summary_table.data.frame(test, by_arm_counts): Use of the data pronoun is no longer required/encouraged.  The
    #>               ability to use it has been deprecated.  See the documentation for
    #>               summary_table, qsummary, and the vignettes for more detail.  The
    #>               use of the data pronoun will be supported in version 0.5.0 of
    #>               qwraps2 with this warning.  Eventually an error will be thrown
    #>               before support is removed from the package completely.
    #> 
    #> 
    #> |                         |x (N = 200)  |
    #> |:------------------------|:------------|
    #> |**ARM?**                 |&nbsp;&nbsp; |
    #> |&nbsp;&nbsp; Active Drug |89 (44.50%)  |
    #> |&nbsp;&nbsp; Placebo     |111 (55.50%) |
    summary_table(test12345678912345, by_arm_counts)
    #> Warning in summary_table.data.frame(test12345678912345, by_arm_counts): Use of the data pronoun is no longer required/encouraged.  The
    #>               ability to use it has been deprecated.  See the documentation for
    #>               summary_table, qsummary, and the vignettes for more detail.  The
    #>               use of the data pronoun will be supported in version 0.5.0 of
    #>               qwraps2 with this warning.  Eventually an error will be thrown
    #>               before support is removed from the package completely.
    #> 
    #> 
    #> |                         |x (N = 200)  |
    #> |:------------------------|:------------|
    #> |**ARM?**                 |&nbsp;&nbsp; |
    #> |&nbsp;&nbsp; Active Drug |89 (44.50%)  |
    #> |&nbsp;&nbsp; Placebo     |111 (55.50%) |

omitting duplicated rows works for `test`

    summary_table(test[!duplicated(test[, c("USUBJID")]), ], by_arm_counts)
    #> Warning in summary_table.data.frame(test[!duplicated(test[, c("USUBJID")]), : Use of the data pronoun is no longer required/encouraged.  The
    #>               ability to use it has been deprecated.  See the documentation for
    #>               summary_table, qsummary, and the vignettes for more detail.  The
    #>               use of the data pronoun will be supported in version 0.5.0 of
    #>               qwraps2 with this warning.  Eventually an error will be thrown
    #>               before support is removed from the package completely.
    #> 
    #> 
    #> |                         |x (N = 26)   |
    #> |:------------------------|:------------|
    #> |**ARM?**                 |&nbsp;&nbsp; |
    #> |&nbsp;&nbsp; Active Drug |11 (42.31%)  |
    #> |&nbsp;&nbsp; Placebo     |15 (57.69%)  |

omitting duplicated rows works for `test12345678912345` results in and error

    summary_table(test12345678912345[!duplicated(test12345678912345[, c("USUBJID")]), ], by_arm_counts)
    #> Warning in summary_table.data.frame(test12345678912345[!duplicated(test12345678912345[, : Use of the data pronoun is no longer required/encouraged.  The
    #>               ability to use it has been deprecated.  See the documentation for
    #>               summary_table, qsummary, and the vignettes for more detail.  The
    #>               use of the data pronoun will be supported in version 0.5.0 of
    #>               qwraps2 with this warning.  Eventually an error will be thrown
    #>               before support is removed from the package completely.
    #> 
    #> 
    #> |                         |x (N = 26)   |
    #> |:------------------------|:------------|
    #> |**ARM?**                 |&nbsp;&nbsp; |
    #> |&nbsp;&nbsp; Active Drug |11 (42.31%)  |
    #> |&nbsp;&nbsp; Placebo     |15 (57.69%)  |

A workaround would be to omit the duplicated rows outside of the
`summary_table` call.

    test12345678912345 %>%
      dplyr::filter(!duplicated(.data$USUBJID)) %>%
      summary_table(., by_arm_counts)
    #> Warning in summary_table.data.frame(., by_arm_counts): Use of the data pronoun is no longer required/encouraged.  The
    #>               ability to use it has been deprecated.  See the documentation for
    #>               summary_table, qsummary, and the vignettes for more detail.  The
    #>               use of the data pronoun will be supported in version 0.5.0 of
    #>               qwraps2 with this warning.  Eventually an error will be thrown
    #>               before support is removed from the package completely.
    #> 
    #> 
    #> |                         |x (N = 26)   |
    #> |:------------------------|:------------|
    #> |**ARM?**                 |&nbsp;&nbsp; |
    #> |&nbsp;&nbsp; Active Drug |11 (42.31%)  |
    #> |&nbsp;&nbsp; Placebo     |15 (57.69%)  |

The cause of this error is in the `summary_table.data.frame` call were
`deparse` is using the default width.cutoff of 60. The argument passed
through `summary_table` is exceeds this limit.

    nchar('test12345678912345[!duplicated(test12345678912345[, c("USUBJID")]), ]')
    #> [1] 69

The result: `deparse` tries, and succeeds in placing a line break, and
generates a character vector with length &gt; 1.

    foo <- function(x) {
      deparse(substitute(x), backtick = TRUE)
    }

    str(
        foo(test12345678912345[!duplicated(test12345678912345[, c("USUBJID")]), ])
        )
    #>  chr [1:2] "test12345678912345[!duplicated(test12345678912345[, c(\"USUBJID\")]), " ...

A fix for this will be to add `nlines = 1L` to the `deparse` calls.

    foo <- function(x) {
      deparse(substitute(x), nlines = 1L, backtick = TRUE)
    }

    str(
        foo(test12345678912345[!duplicated(test12345678912345[, c("USUBJID")]), ])
        )
    #>  chr "test12345678912345[!duplicated(test12345678912345[, c(\"USUBJID\")]), "

I will be posting the patch to kill this bug shortly.

After the fix:
==============

    library(qwraps2)
    packageVersion('qwraps2')
    #> [1] '0.4.2.9006'
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

summary table works as expected

    summary_table(test, by_arm_counts)
    #> Warning in summary_table.data.frame(test, by_arm_counts): Use of the data pronoun is no longer required/encouraged.  The
    #>               ability to use it has been deprecated.  See the documentation for
    #>               summary_table, qsummary, and the vignettes for more detail.  The
    #>               use of the data pronoun will be supported in version 0.5.0 of
    #>               qwraps2 with this warning.  Eventually an error will be thrown
    #>               before support is removed from the package completely.
    #> 
    #> 
    #> |                         |x (N = 200)  |
    #> |:------------------------|:------------|
    #> |**ARM?**                 |&nbsp;&nbsp; |
    #> |&nbsp;&nbsp; Active Drug |89 (44.50%)  |
    #> |&nbsp;&nbsp; Placebo     |111 (55.50%) |
    summary_table(test12345678912345, by_arm_counts)
    #> Warning in summary_table.data.frame(test12345678912345, by_arm_counts): Use of the data pronoun is no longer required/encouraged.  The
    #>               ability to use it has been deprecated.  See the documentation for
    #>               summary_table, qsummary, and the vignettes for more detail.  The
    #>               use of the data pronoun will be supported in version 0.5.0 of
    #>               qwraps2 with this warning.  Eventually an error will be thrown
    #>               before support is removed from the package completely.
    #> 
    #> 
    #> |                         |x (N = 200)  |
    #> |:------------------------|:------------|
    #> |**ARM?**                 |&nbsp;&nbsp; |
    #> |&nbsp;&nbsp; Active Drug |89 (44.50%)  |
    #> |&nbsp;&nbsp; Placebo     |111 (55.50%) |
    summary_table(test[!duplicated(test[, c("USUBJID")]), ], by_arm_counts)
    #> Warning in summary_table.data.frame(test[!duplicated(test[, c("USUBJID")]), : Use of the data pronoun is no longer required/encouraged.  The
    #>               ability to use it has been deprecated.  See the documentation for
    #>               summary_table, qsummary, and the vignettes for more detail.  The
    #>               use of the data pronoun will be supported in version 0.5.0 of
    #>               qwraps2 with this warning.  Eventually an error will be thrown
    #>               before support is removed from the package completely.
    #> 
    #> 
    #> |                         |x (N = 26)   |
    #> |:------------------------|:------------|
    #> |**ARM?**                 |&nbsp;&nbsp; |
    #> |&nbsp;&nbsp; Active Drug |11 (42.31%)  |
    #> |&nbsp;&nbsp; Placebo     |15 (57.69%)  |
    summary_table(test12345678912345[!duplicated(test12345678912345[, c("USUBJID")]), ], by_arm_counts)
    #> Warning in summary_table.data.frame(test12345678912345[!duplicated(test12345678912345[, : Use of the data pronoun is no longer required/encouraged.  The
    #>               ability to use it has been deprecated.  See the documentation for
    #>               summary_table, qsummary, and the vignettes for more detail.  The
    #>               use of the data pronoun will be supported in version 0.5.0 of
    #>               qwraps2 with this warning.  Eventually an error will be thrown
    #>               before support is removed from the package completely.
    #> 
    #> 
    #> |                         |x (N = 26)   |
    #> |:------------------------|:------------|
    #> |**ARM?**                 |&nbsp;&nbsp; |
    #> |&nbsp;&nbsp; Active Drug |11 (42.31%)  |
    #> |&nbsp;&nbsp; Placebo     |15 (57.69%)  |

    test12345678912345 %>%
      dplyr::filter(!duplicated(.data$USUBJID)) %>%
      summary_table(., by_arm_counts)
    #> Warning in summary_table.data.frame(., by_arm_counts): Use of the data pronoun is no longer required/encouraged.  The
    #>               ability to use it has been deprecated.  See the documentation for
    #>               summary_table, qsummary, and the vignettes for more detail.  The
    #>               use of the data pronoun will be supported in version 0.5.0 of
    #>               qwraps2 with this warning.  Eventually an error will be thrown
    #>               before support is removed from the package completely.
    #> 
    #> 
    #> |                         |x (N = 26)   |
    #> |:------------------------|:------------|
    #> |**ARM?**                 |&nbsp;&nbsp; |
    #> |&nbsp;&nbsp; Active Drug |11 (42.31%)  |
    #> |&nbsp;&nbsp; Placebo     |15 (57.69%)  |

For version 0.5.0
=================

    library(qwraps2)
    options(qwraps2_markup = "markdown")

    set.seed(42)

    by_arm_counts <-
      list("ARM?" =
           list(
                "Active Drug" = ~ qwraps2::n_perc(ARM == "Active"),
                "Placebo" = ~ qwraps2::n_perc(ARM == "Placebo")
               )
          )

    test12345678912345 <- test <-
      data.frame(ARM = sample(c("Active", "Placebo"), size = 200, replace = TRUE),
                 USUBJID = sample(LETTERS, size = 200, replace = TRUE))

summary table works as expected

    summary_table(test, by_arm_counts)
    #> 
    #> 
    #> |                         |test (N = 200) |
    #> |:------------------------|:--------------|
    #> |**ARM?**                 |&nbsp;&nbsp;   |
    #> |&nbsp;&nbsp; Active Drug |89 (44.50%)    |
    #> |&nbsp;&nbsp; Placebo     |111 (55.50%)   |
    summary_table(test12345678912345, by_arm_counts)
    #> 
    #> 
    #> |                         |test12345678912345 (N = 200) |
    #> |:------------------------|:----------------------------|
    #> |**ARM?**                 |&nbsp;&nbsp;                 |
    #> |&nbsp;&nbsp; Active Drug |89 (44.50%)                  |
    #> |&nbsp;&nbsp; Placebo     |111 (55.50%)                 |
    summary_table(test[!duplicated(test[, c("USUBJID")]), ], by_arm_counts)
    #> 
    #> 
    #> |                         |test[!duplicated(test[, c("USUBJID")]), ] (N = 26) |
    #> |:------------------------|:--------------------------------------------------|
    #> |**ARM?**                 |&nbsp;&nbsp;                                       |
    #> |&nbsp;&nbsp; Active Drug |11 (42.31%)                                        |
    #> |&nbsp;&nbsp; Placebo     |15 (57.69%)                                        |
    summary_table(test12345678912345[!duplicated(test12345678912345[, c("USUBJID")]), ], by_arm_counts)
    #> 
    #> 
    #> |                         |test12345678912345[!duplicated(test12345678912345[, c("USUBJID")]),  (N = 26) |
    #> |:------------------------|:-----------------------------------------------------------------------------|
    #> |**ARM?**                 |&nbsp;&nbsp;                                                                  |
    #> |&nbsp;&nbsp; Active Drug |11 (42.31%)                                                                   |
    #> |&nbsp;&nbsp; Placebo     |15 (57.69%)                                                                   |

<sup>Created on 2020-08-27 by the [reprex package](https://reprex.tidyverse.org) (v0.3.0)</sup>
<details>
<summary>
Session info
</summary>

    devtools::session_info()
    #> ─ Session info ───────────────────────────────────────────────────────────────
    #>  setting  value                       
    #>  version  R version 4.0.2 (2020-06-22)
    #>  os       macOS Catalina 10.15.6      
    #>  system   x86_64, darwin17.0          
    #>  ui       X11                         
    #>  language (EN)                        
    #>  collate  en_US.UTF-8                 
    #>  ctype    en_US.UTF-8                 
    #>  tz       America/Denver              
    #>  date     2020-08-27                  
    #> 
    #> ─ Packages ───────────────────────────────────────────────────────────────────
    #>  package     * version    date       lib source        
    #>  assertthat    0.2.1      2019-03-21 [1] CRAN (R 4.0.0)
    #>  backports     1.1.9      2020-08-24 [1] CRAN (R 4.0.2)
    #>  callr         3.4.3      2020-03-28 [1] CRAN (R 4.0.0)
    #>  cli           2.0.2      2020-02-28 [1] CRAN (R 4.0.0)
    #>  crayon        1.3.4      2017-09-16 [1] CRAN (R 4.0.0)
    #>  desc          1.2.0      2018-05-01 [1] CRAN (R 4.0.0)
    #>  devtools      2.3.1      2020-07-21 [1] CRAN (R 4.0.2)
    #>  digest        0.6.25     2020-02-23 [1] CRAN (R 4.0.0)
    #>  dplyr         1.0.2      2020-08-18 [1] CRAN (R 4.0.2)
    #>  ellipsis      0.3.1      2020-05-15 [1] CRAN (R 4.0.0)
    #>  evaluate      0.14       2019-05-28 [1] CRAN (R 4.0.0)
    #>  fansi         0.4.1      2020-01-08 [1] CRAN (R 4.0.0)
    #>  fs            1.5.0      2020-07-31 [1] CRAN (R 4.0.2)
    #>  generics      0.0.2      2018-11-29 [1] CRAN (R 4.0.0)
    #>  glue          1.4.1      2020-05-13 [1] CRAN (R 4.0.0)
    #>  highr         0.8        2019-03-20 [1] CRAN (R 4.0.0)
    #>  htmltools     0.5.0      2020-06-16 [1] CRAN (R 4.0.0)
    #>  knitr         1.29       2020-06-23 [1] CRAN (R 4.0.0)
    #>  lifecycle     0.2.0      2020-03-06 [1] CRAN (R 4.0.0)
    #>  magrittr    * 1.5        2014-11-22 [1] CRAN (R 4.0.0)
    #>  memoise       1.1.0      2017-04-21 [1] CRAN (R 4.0.0)
    #>  pillar        1.4.6      2020-07-10 [1] CRAN (R 4.0.2)
    #>  pkgbuild      1.1.0      2020-07-13 [1] CRAN (R 4.0.2)
    #>  pkgconfig     2.0.3      2019-09-22 [1] CRAN (R 4.0.0)
    #>  pkgload       1.1.0      2020-05-29 [1] CRAN (R 4.0.0)
    #>  prettyunits   1.1.1      2020-01-24 [1] CRAN (R 4.0.0)
    #>  processx      3.4.3      2020-07-05 [1] CRAN (R 4.0.0)
    #>  ps            1.3.4      2020-08-11 [1] CRAN (R 4.0.2)
    #>  purrr         0.3.4      2020-04-17 [1] CRAN (R 4.0.0)
    #>  qwraps2     * 0.4.2.9006 2020-08-26 [1] local         
    #>  R6            2.4.1      2019-11-12 [1] CRAN (R 4.0.0)
    #>  Rcpp          1.0.5      2020-07-06 [1] CRAN (R 4.0.0)
    #>  remotes       2.2.0      2020-07-21 [1] CRAN (R 4.0.2)
    #>  rlang         0.4.7      2020-07-09 [1] CRAN (R 4.0.2)
    #>  rmarkdown     2.3        2020-06-18 [1] CRAN (R 4.0.0)
    #>  rprojroot     1.3-2      2018-01-03 [1] CRAN (R 4.0.0)
    #>  sessioninfo   1.1.1      2018-11-05 [1] CRAN (R 4.0.0)
    #>  stringi       1.4.6      2020-02-17 [1] CRAN (R 4.0.0)
    #>  stringr       1.4.0      2019-02-10 [1] CRAN (R 4.0.0)
    #>  testthat      2.3.2      2020-03-02 [1] CRAN (R 4.0.0)
    #>  tibble        3.0.3      2020-07-10 [1] CRAN (R 4.0.2)
    #>  tidyselect    1.1.0      2020-05-11 [1] CRAN (R 4.0.0)
    #>  usethis       1.6.1      2020-04-29 [1] CRAN (R 4.0.0)
    #>  vctrs         0.3.2      2020-07-15 [1] CRAN (R 4.0.2)
    #>  withr         2.2.0      2020-04-20 [1] CRAN (R 4.0.0)
    #>  xfun          0.16       2020-07-24 [1] CRAN (R 4.0.2)
    #>  yaml          2.2.1      2020-02-01 [1] CRAN (R 4.0.0)
    #> 
    #> [1] /Library/Frameworks/R.framework/Versions/4.0/Resources/library

</details>
