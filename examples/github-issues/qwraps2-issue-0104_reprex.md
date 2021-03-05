This issue was originally posted on
[stackoverflow](https://stackoverflow.com/q/66362954/1104685)

When there is missing data in a data set, some of the rows in a grouped
summary table may not generate correctly and through an error. Here is a
reproducible example:

``` r
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
#> Error in cbind(deparse.level, ...): Not all row groups are identical.
```

The error occurs using the updated qwraps2 api

``` r
summary_table(dat, summaries = summary1, by = "group")
#> Error in cbind(deparse.level, ...): Not all row groups are identical.
```

There is a notable issue with the min and max rows, e.g.,

``` r
summary_table(subset(dat, group == "A"), summary1)
#> 
#> 
#> |                                  |subset(dat, group == "A") (N = 3) |
#> |:---------------------------------|:---------------------------------|
#> |**Time-To-event**                 |&nbsp;&nbsp;                      |
#> |&nbsp;&nbsp; min                  |1                                 |
#> |&nbsp;&nbsp; max                  |3                                 |
#> |&nbsp;&nbsp; mean (sd)            |2.00 &plusmn; 1.00                |
#> |**Case Status**                   |&nbsp;&nbsp;                      |
#> |&nbsp;&nbsp; Dead                 |1 (33)                            |
#> |&nbsp;&nbsp; Alive/Lost-to-follow |2 (67)                            |
summary_table(subset(dat, group == "B"), summary1)
#> 
#> 
#> |                                  |subset(dat, group == "B") (N = 17) |
#> |:---------------------------------|:----------------------------------|
#> |**Time-To-event**                 |&nbsp;&nbsp;                       |
#> |&nbsp;&nbsp; mean (sd)            |7;  NA &plusmn;  NA                |
#> |**Case Status**                   |&nbsp;&nbsp;                       |
#> |&nbsp;&nbsp; Dead                 |9 (53)                             |
#> |&nbsp;&nbsp; Alive/Lost-to-follow |8 (47)                             |
```

The missing values for x which are in group B result in rows being omitted
from the output. This can be corrected by using the na.rm argument in the
min and max calls.

``` r
summary2 <- summary1
summary2[["Time-To-event"]][["min"]] <- ~ min(x, na.rm = TRUE)
summary2[["Time-To-event"]][["max"]] <- ~ max(x, na.rm = TRUE)

summary_table(dat, summary2, by = "group")
#> 
#> 
#> |                                  |A (N = 3)          |B (N = 17)          |
#> |:---------------------------------|:------------------|:-------------------|
#> |**Time-To-event**                 |&nbsp;&nbsp;       |&nbsp;&nbsp;        |
#> |&nbsp;&nbsp; min                  |1                  |4                   |
#> |&nbsp;&nbsp; max                  |3                  |10                  |
#> |&nbsp;&nbsp; mean (sd)            |2.00 &plusmn; 1.00 |7;  NA &plusmn;  NA |
#> |**Case Status**                   |&nbsp;&nbsp;       |&nbsp;&nbsp;        |
#> |&nbsp;&nbsp; Dead                 |1 (33)             |9 (53)              |
#> |&nbsp;&nbsp; Alive/Lost-to-follow |2 (67)             |8 (47)              |
```

<sup>Created on 2021-03-05 by the [reprex package](https://reprex.tidyverse.org) (v1.0.0)</sup>

<details style="margin-bottom:10px;">
<summary>
Session info
</summary>

``` r
sessioninfo::session_info()
#> ─ Session info ───────────────────────────────────────────────────────────────
#>  setting  value                       
#>  version  R version 4.0.4 (2021-02-15)
#>  os       macOS Big Sur 10.16         
#>  system   x86_64, darwin17.0          
#>  ui       X11                         
#>  language (EN)                        
#>  collate  en_US.UTF-8                 
#>  ctype    en_US.UTF-8                 
#>  tz       America/Denver              
#>  date     2021-03-05                  
#> 
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  package     * version    date       lib source                       
#>  assertthat    0.2.1      2019-03-21 [1] CRAN (R 4.0.0)               
#>  backports     1.2.1      2020-12-09 [1] CRAN (R 4.0.2)               
#>  cli           2.3.1      2021-02-23 [1] CRAN (R 4.0.2)               
#>  crayon        1.4.1.9000 2021-03-01 [1] Github (r-lib/crayon@965d1dc)
#>  DBI           1.1.1      2021-01-15 [1] CRAN (R 4.0.2)               
#>  debugme       1.1.0      2017-10-22 [1] CRAN (R 4.0.2)               
#>  digest        0.6.27     2020-10-24 [1] CRAN (R 4.0.2)               
#>  dplyr         1.0.4      2021-02-02 [1] CRAN (R 4.0.2)               
#>  ellipsis      0.3.1      2020-05-15 [1] CRAN (R 4.0.0)               
#>  evaluate      0.14       2019-05-28 [1] CRAN (R 4.0.0)               
#>  fansi         0.4.2      2021-01-15 [1] CRAN (R 4.0.2)               
#>  fs            1.5.0      2020-07-31 [1] CRAN (R 4.0.2)               
#>  generics      0.1.0      2020-10-31 [1] CRAN (R 4.0.2)               
#>  glue          1.4.2      2020-08-27 [1] CRAN (R 4.0.2)               
#>  highr         0.8        2019-03-20 [1] CRAN (R 4.0.0)               
#>  htmltools     0.5.1.1    2021-01-22 [1] CRAN (R 4.0.2)               
#>  knitr         1.31       2021-01-27 [1] CRAN (R 4.0.2)               
#>  lifecycle     1.0.0      2021-02-15 [1] CRAN (R 4.0.2)               
#>  magrittr      2.0.1      2020-11-17 [1] CRAN (R 4.0.2)               
#>  pillar        1.5.0      2021-02-22 [1] CRAN (R 4.0.2)               
#>  pkgconfig     2.0.3      2019-09-22 [1] CRAN (R 4.0.0)               
#>  purrr         0.3.4      2020-04-17 [1] CRAN (R 4.0.0)               
#>  qwraps2     * 0.5.1      2021-03-01 [1] CRAN (R 4.0.4)               
#>  R6            2.5.0      2020-10-28 [1] CRAN (R 4.0.2)               
#>  Rcpp          1.0.6      2021-01-15 [1] CRAN (R 4.0.2)               
#>  reprex        1.0.0      2021-01-27 [1] CRAN (R 4.0.2)               
#>  rlang         0.4.10     2020-12-30 [1] CRAN (R 4.0.2)               
#>  rmarkdown     2.7        2021-02-19 [1] CRAN (R 4.0.2)               
#>  sessioninfo   1.1.1      2018-11-05 [1] CRAN (R 4.0.0)               
#>  stringi       1.5.3      2020-09-09 [1] CRAN (R 4.0.2)               
#>  stringr       1.4.0      2019-02-10 [1] CRAN (R 4.0.0)               
#>  styler        1.3.2      2020-02-23 [1] CRAN (R 4.0.0)               
#>  tibble        3.1.0      2021-02-25 [1] CRAN (R 4.0.2)               
#>  tidyselect    1.1.0      2020-05-11 [1] CRAN (R 4.0.0)               
#>  utf8          1.1.4      2018-05-24 [1] CRAN (R 4.0.0)               
#>  vctrs         0.3.6      2020-12-17 [1] CRAN (R 4.0.2)               
#>  withr         2.4.1      2021-01-26 [1] CRAN (R 4.0.2)               
#>  xfun          0.21       2021-02-10 [1] CRAN (R 4.0.2)               
#>  yaml          2.2.1      2020-02-01 [1] CRAN (R 4.0.0)               
#> 
#> [1] /Library/Frameworks/R.framework/Versions/4.0/Resources/library
```

</details>
