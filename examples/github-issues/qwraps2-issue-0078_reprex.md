    library(qwraps2)
    library(dplyr)
    #> 
    #> Attaching package: 'dplyr'
    #> The following objects are masked from 'package:stats':
    #> 
    #>     filter, lag
    #> The following objects are masked from 'package:base':
    #> 
    #>     intersect, setdiff, setequal, union
    options(qwraps2_markup = "markdown")

    my_mtcars <- dplyr::select(mtcars, mpg, cyl, hp)
    stats_summary <- list("MPG" =
                          list("min" = ~ qwraps2::frmt(min(.data[["mpg"]])),
                               "max" = ~ qwraps2::frmt(max(.data[["mpg"]]))))

    st <- summary_table(dplyr::group_by(my_mtcars, cyl), stats_summary)
    #> Warning in min(.data[["mpg"]]): no non-missing arguments to min; returning Inf
    #> Warning in max(.data[["mpg"]]): no non-missing arguments to max; returning -Inf
    #> Warning in min(.data[["mpg"]]): no non-missing arguments to min; returning Inf
    #> Warning in max(.data[["mpg"]]): no non-missing arguments to max; returning -Inf
    #> Warning in min(.data[["mpg"]]): no non-missing arguments to min; returning Inf
    #> Warning in max(.data[["mpg"]]): no non-missing arguments to max; returning -Inf

The default table is

    st
    #> 
    #> 
    #> |                 |4 (N = 11)   |6 (N = 7)    |8 (N = 14)   |
    #> |:----------------|:------------|:------------|:------------|
    #> |**MPG**          |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |
    #> |&nbsp;&nbsp; min |Inf          |Inf          |Inf          |
    #> |&nbsp;&nbsp; max |-Inf         |-Inf         |-Inf         |

First question, is it possible to rename the columns, yes. One way is to
define the column names via the `cname` argument of the print method for
the summary table.

    print(st, align = "r")
    #> 
    #> 
    #> |                 |   4 (N = 11)|    6 (N = 7)|   8 (N = 14)|
    #> |----------------:|------------:|------------:|------------:|
    #> |          **MPG**| &nbsp;&nbsp;| &nbsp;&nbsp;| &nbsp;&nbsp;|
    #> | &nbsp;&nbsp; min|          Inf|          Inf|          Inf|
    #> | &nbsp;&nbsp; max|         -Inf|         -Inf|         -Inf|

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
    #>  dplyr       * 1.0.2      2020-08-18 [1] CRAN (R 4.0.2)
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
    #>  magrittr      1.5        2014-11-22 [1] CRAN (R 4.0.0)
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
