Version 0.5.0 of qwraps2, which will be published soon, refactored how
`summary_table` was implmented. The changes allow for grouping by multiple
variables and does not require use of dplyr.

**NOTE** use of the data pronoun `.data` in the summary is no longer needed
or recommended.

    library(qwraps2)
    options(qwraps2_markup = "markdown")
    packageVersion("qwraps2")
    #> [1] '0.4.2.9006'

    our_summary <-
      list(
           "mpg" = list( "min" = ~ min(mpg),
                        "mean (95% CI)" = ~ frmtci(mean_ci(mpg))),
           "hp" = list("min" = ~ min(hp),
                       "max" = ~ max(hp),
                       "mean (95% CI)" = ~ frmtci(mean_ci(hp)))
      )

Using the refactored summary\_table

    by_cyl    <- summary_table(mtcars, our_summary, by = "cyl")
    by_am     <- summary_table(mtcars, our_summary, by = "am")
    by_cyl_am <- summary_table(mtcars, our_summary, by = c("cyl", "am"))

    by_cyl

<table>
<colgroup>
<col style="width: 28%" />
<col style="width: 21%" />
<col style="width: 25%" />
<col style="width: 25%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">4 (N = 11)</th>
<th style="text-align: left;">6 (N = 7)</th>
<th style="text-align: left;">8 (N = 14)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;"><strong>mpg</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">21.4</td>
<td style="text-align: left;">17.8</td>
<td style="text-align: left;">10.4</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean (95% CI)</td>
<td style="text-align: left;">26.66 (24.00, 29.33)</td>
<td style="text-align: left;">19.74 (18.67, 20.82)</td>
<td style="text-align: left;">15.10 (13.76, 16.44)</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>hp</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="odd">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">52</td>
<td style="text-align: left;">105</td>
<td style="text-align: left;">150</td>
</tr>
<tr class="even">
<td style="text-align: left;">   max</td>
<td style="text-align: left;">113</td>
<td style="text-align: left;">175</td>
<td style="text-align: left;">335</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean (95% CI)</td>
<td style="text-align: left;">82.64 (70.27, 95.01)</td>
<td style="text-align: left;">122.29 (104.31, 140.26)</td>
<td style="text-align: left;">209.21 (182.51, 235.92)</td>
</tr>
</tbody>
</table>

    by_am

<table>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">0 (N = 19)</th>
<th style="text-align: left;">1 (N = 13)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;"><strong>mpg</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">10.4</td>
<td style="text-align: left;">15</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean (95% CI)</td>
<td style="text-align: left;">17.15 (15.42, 18.87)</td>
<td style="text-align: left;">24.39 (21.04, 27.74)</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>hp</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="odd">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">62</td>
<td style="text-align: left;">52</td>
</tr>
<tr class="even">
<td style="text-align: left;">   max</td>
<td style="text-align: left;">245</td>
<td style="text-align: left;">335</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean (95% CI)</td>
<td style="text-align: left;">160.26 (136.02, 184.50)</td>
<td style="text-align: left;">126.85 (81.15, 172.54)</td>
</tr>
</tbody>
</table>

    by_cyl_am

<table>
<colgroup>
<col style="width: 16%" />
<col style="width: 13%" />
<col style="width: 14%" />
<col style="width: 14%" />
<col style="width: 12%" />
<col style="width: 13%" />
<col style="width: 14%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">4.0 (N = 3)</th>
<th style="text-align: left;">6.0 (N = 4)</th>
<th style="text-align: left;">8.0 (N = 12)</th>
<th style="text-align: left;">4.1 (N = 8)</th>
<th style="text-align: left;">6.1 (N = 3)</th>
<th style="text-align: left;">8.1 (N = 2)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;"><strong>mpg</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">21.5</td>
<td style="text-align: left;">17.8</td>
<td style="text-align: left;">10.4</td>
<td style="text-align: left;">21.4</td>
<td style="text-align: left;">19.7</td>
<td style="text-align: left;">15</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean (95% CI)</td>
<td style="text-align: left;">22.90 (21.26, 24.54)</td>
<td style="text-align: left;">19.12 (17.53, 20.72)</td>
<td style="text-align: left;">15.05 (13.48, 16.62)</td>
<td style="text-align: left;">28.07 (24.97, 31.18)</td>
<td style="text-align: left;">20.57 (19.72, 21.42)</td>
<td style="text-align: left;">15.40 (14.62, 16.18)</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>hp</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="odd">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">62</td>
<td style="text-align: left;">105</td>
<td style="text-align: left;">150</td>
<td style="text-align: left;">52</td>
<td style="text-align: left;">110</td>
<td style="text-align: left;">264</td>
</tr>
<tr class="even">
<td style="text-align: left;">   max</td>
<td style="text-align: left;">97</td>
<td style="text-align: left;">123</td>
<td style="text-align: left;">245</td>
<td style="text-align: left;">113</td>
<td style="text-align: left;">175</td>
<td style="text-align: left;">335</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean (95% CI)</td>
<td style="text-align: left;">84.67 (62.42, 106.91)</td>
<td style="text-align: left;">115.25 (106.25, 124.25)</td>
<td style="text-align: left;">194.17 (175.29, 213.04)</td>
<td style="text-align: left;">81.88 (66.18, 97.57)</td>
<td style="text-align: left;">131.67 (89.20, 174.13)</td>
<td style="text-align: left;">299.50 (229.92, 369.08)</td>
</tr>
</tbody>
</table>

Producing the same tables using dplyr::group\_by

    grpby_cyl    <- summary_table(dplyr::group_by(mtcars, cyl),     our_summary)
    grpby_am     <- summary_table(dplyr::group_by(mtcars, am),      our_summary)
    grpby_cyl_am <- summary_table(dplyr::group_by(mtcars, cyl, am), our_summary)

    grpby_cyl

<table>
<colgroup>
<col style="width: 28%" />
<col style="width: 21%" />
<col style="width: 25%" />
<col style="width: 25%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">4 (N = 11)</th>
<th style="text-align: left;">6 (N = 7)</th>
<th style="text-align: left;">8 (N = 14)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;"><strong>mpg</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">21.4</td>
<td style="text-align: left;">17.8</td>
<td style="text-align: left;">10.4</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean (95% CI)</td>
<td style="text-align: left;">26.66 (24.00, 29.33)</td>
<td style="text-align: left;">19.74 (18.67, 20.82)</td>
<td style="text-align: left;">15.10 (13.76, 16.44)</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>hp</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="odd">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">52</td>
<td style="text-align: left;">105</td>
<td style="text-align: left;">150</td>
</tr>
<tr class="even">
<td style="text-align: left;">   max</td>
<td style="text-align: left;">113</td>
<td style="text-align: left;">175</td>
<td style="text-align: left;">335</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean (95% CI)</td>
<td style="text-align: left;">82.64 (70.27, 95.01)</td>
<td style="text-align: left;">122.29 (104.31, 140.26)</td>
<td style="text-align: left;">209.21 (182.51, 235.92)</td>
</tr>
</tbody>
</table>

    grpby_am

<table>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">0 (N = 19)</th>
<th style="text-align: left;">1 (N = 13)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;"><strong>mpg</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">10.4</td>
<td style="text-align: left;">15</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean (95% CI)</td>
<td style="text-align: left;">17.15 (15.42, 18.87)</td>
<td style="text-align: left;">24.39 (21.04, 27.74)</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>hp</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="odd">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">62</td>
<td style="text-align: left;">52</td>
</tr>
<tr class="even">
<td style="text-align: left;">   max</td>
<td style="text-align: left;">245</td>
<td style="text-align: left;">335</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean (95% CI)</td>
<td style="text-align: left;">160.26 (136.02, 184.50)</td>
<td style="text-align: left;">126.85 (81.15, 172.54)</td>
</tr>
</tbody>
</table>

    grpby_cyl_am

<table>
<colgroup>
<col style="width: 16%" />
<col style="width: 13%" />
<col style="width: 14%" />
<col style="width: 14%" />
<col style="width: 12%" />
<col style="width: 13%" />
<col style="width: 14%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">4.0 (N = 3)</th>
<th style="text-align: left;">6.0 (N = 4)</th>
<th style="text-align: left;">8.0 (N = 12)</th>
<th style="text-align: left;">4.1 (N = 8)</th>
<th style="text-align: left;">6.1 (N = 3)</th>
<th style="text-align: left;">8.1 (N = 2)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;"><strong>mpg</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">21.5</td>
<td style="text-align: left;">17.8</td>
<td style="text-align: left;">10.4</td>
<td style="text-align: left;">21.4</td>
<td style="text-align: left;">19.7</td>
<td style="text-align: left;">15</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean (95% CI)</td>
<td style="text-align: left;">22.90 (21.26, 24.54)</td>
<td style="text-align: left;">19.12 (17.53, 20.72)</td>
<td style="text-align: left;">15.05 (13.48, 16.62)</td>
<td style="text-align: left;">28.07 (24.97, 31.18)</td>
<td style="text-align: left;">20.57 (19.72, 21.42)</td>
<td style="text-align: left;">15.40 (14.62, 16.18)</td>
</tr>
<tr class="even">
<td style="text-align: left;"><strong>hp</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="odd">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">62</td>
<td style="text-align: left;">105</td>
<td style="text-align: left;">150</td>
<td style="text-align: left;">52</td>
<td style="text-align: left;">110</td>
<td style="text-align: left;">264</td>
</tr>
<tr class="even">
<td style="text-align: left;">   max</td>
<td style="text-align: left;">97</td>
<td style="text-align: left;">123</td>
<td style="text-align: left;">245</td>
<td style="text-align: left;">113</td>
<td style="text-align: left;">175</td>
<td style="text-align: left;">335</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean (95% CI)</td>
<td style="text-align: left;">84.67 (62.42, 106.91)</td>
<td style="text-align: left;">115.25 (106.25, 124.25)</td>
<td style="text-align: left;">194.17 (175.29, 213.04)</td>
<td style="text-align: left;">81.88 (66.18, 97.57)</td>
<td style="text-align: left;">131.67 (89.20, 174.13)</td>
<td style="text-align: left;">299.50 (229.92, 369.08)</td>
</tr>
</tbody>
</table>

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
