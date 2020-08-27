Hi @estephanmoana, thank you for the suggestions. I am unable to reproduce your example. In the future you may find the [reprex](https://cran.r-project.org/package=reprex) package helpful for producing reproducable examples and posts for github issues.

I think the idea behind your comment is covered by the options in `qsummary`. You can define the summary stats for numeric variables and options for how `n_perc` will display counts and pecentages for characters, factors, and logicals.

    library(qwraps2)
    packageVersion("qwraps2")
    #> [1] '0.4.2.9006'
    options(qwraps2_markup = "markdown")
    set.seed(42)

Define a function for finding the coeffient of variation

    cv <- function(x, na_rm = FALSE) {
      m <- mean(x, na.rm = na_rm)
      s <- sd(x, na.rm = na_rm)
      s / m
    }

example cv

    cv(rnorm(100, mean = 12, sd = 2))
    #> [1] 0.172624

extend the mtcars data.frame to include a logial vector, a character vector,
and a factor vector.

    mtcars2 <- mtcars[, c("mpg", "cyl", "am", "gear", "hp", "wt")]
    mtcars2[["MPG > 25"]] <- mtcars2$mpg > 25                       # logical
    mtcars2[["gear"]] <- factor(mtcars2$gear, levels = c(3, 4, 5), labels = c("Three Gears", "Four Gears", "Five Gears"))

The list-of-lists of formula for the summary is generated thusly

    qs <- qsummary(mtcars2,
                   numeric_summaries = list("min" = "~ qwraps2::frmt(min(%s))",
                                            "mean and sd" = "~ qwraps2::mean_sd(%s)",
                                            "cv"  = "~ qwraps2::frmt(cv(%s))"),
                   n_perc_args = list(digits = 4, show_symbol = TRUE))
    qs
    #> $mpg
    #> $mpg$min
    #> ~qwraps2::frmt(min(mpg))
    #> 
    #> $mpg$`mean and sd`
    #> ~qwraps2::mean_sd(mpg)
    #> 
    #> $mpg$cv
    #> ~qwraps2::frmt(cv(mpg))
    #> 
    #> 
    #> $cyl
    #> $cyl$min
    #> ~qwraps2::frmt(min(cyl))
    #> 
    #> $cyl$`mean and sd`
    #> ~qwraps2::mean_sd(cyl)
    #> 
    #> $cyl$cv
    #> ~qwraps2::frmt(cv(cyl))
    #> 
    #> 
    #> $am
    #> $am$min
    #> ~qwraps2::frmt(min(am))
    #> 
    #> $am$`mean and sd`
    #> ~qwraps2::mean_sd(am)
    #> 
    #> $am$cv
    #> ~qwraps2::frmt(cv(am))
    #> 
    #> 
    #> $gear
    #> $gear$`Three Gears`
    #> ~qwraps2::n_perc(gear == "Three Gears", digits = 4, show_symbol = TRUE)
    #> 
    #> $gear$`Four Gears`
    #> ~qwraps2::n_perc(gear == "Four Gears", digits = 4, show_symbol = TRUE)
    #> 
    #> $gear$`Five Gears`
    #> ~qwraps2::n_perc(gear == "Five Gears", digits = 4, show_symbol = TRUE)
    #> 
    #> 
    #> $hp
    #> $hp$min
    #> ~qwraps2::frmt(min(hp))
    #> 
    #> $hp$`mean and sd`
    #> ~qwraps2::mean_sd(hp)
    #> 
    #> $hp$cv
    #> ~qwraps2::frmt(cv(hp))
    #> 
    #> 
    #> $wt
    #> $wt$min
    #> ~qwraps2::frmt(min(wt))
    #> 
    #> $wt$`mean and sd`
    #> ~qwraps2::mean_sd(wt)
    #> 
    #> $wt$cv
    #> ~qwraps2::frmt(cv(wt))
    #> 
    #> 
    #> $`MPG > 25`
    #> $`MPG > 25`[[1]]
    #> ~qwraps2::n_perc(`MPG > 25`, digits = 4, show_symbol = TRUE)

A simple table:

    summary_table(mtcars2, qs)

<table>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">mtcars2 (N = 32)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;"><strong>mpg</strong></td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">10.40</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean and sd</td>
<td style="text-align: left;">20.09 ± 6.03</td>
</tr>
<tr class="even">
<td style="text-align: left;">   cv</td>
<td style="text-align: left;">0.30</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>cyl</strong></td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">4.00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean and sd</td>
<td style="text-align: left;">6.19 ± 1.79</td>
</tr>
<tr class="even">
<td style="text-align: left;">   cv</td>
<td style="text-align: left;">0.29</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>am</strong></td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">0.00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean and sd</td>
<td style="text-align: left;">0.41 ± 0.50</td>
</tr>
<tr class="even">
<td style="text-align: left;">   cv</td>
<td style="text-align: left;">1.23</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>gear</strong></td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">   Three Gears</td>
<td style="text-align: left;">15 (46.8750%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   Four Gears</td>
<td style="text-align: left;">12 (37.5000%)</td>
</tr>
<tr class="even">
<td style="text-align: left;">   Five Gears</td>
<td style="text-align: left;">5 (15.6250%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>hp</strong></td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">52.00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean and sd</td>
<td style="text-align: left;">146.69 ± 68.56</td>
</tr>
<tr class="even">
<td style="text-align: left;">   cv</td>
<td style="text-align: left;">0.47</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>wt</strong></td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">1.51</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean and sd</td>
<td style="text-align: left;">3.22 ± 0.98</td>
</tr>
<tr class="even">
<td style="text-align: left;">   cv</td>
<td style="text-align: left;">0.30</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>MPG &gt; 25</strong></td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">  </td>
<td style="text-align: left;">6 (18.7500%)</td>
</tr>
</tbody>
</table>

When grouping, I would drop the summary of the grouping var

    summary_table(mtcars2, qs[-which(names(qs) == "cyl")], by = c("cyl"))

<table>
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
<td style="text-align: left;">21.40</td>
<td style="text-align: left;">17.80</td>
<td style="text-align: left;">10.40</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean and sd</td>
<td style="text-align: left;">26.66 ± 4.51</td>
<td style="text-align: left;">19.74 ± 1.45</td>
<td style="text-align: left;">15.10 ± 2.56</td>
</tr>
<tr class="even">
<td style="text-align: left;">   cv</td>
<td style="text-align: left;">0.17</td>
<td style="text-align: left;">0.07</td>
<td style="text-align: left;">0.17</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>am</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">0.00</td>
<td style="text-align: left;">0.00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean and sd</td>
<td style="text-align: left;">0.73 ± 0.47</td>
<td style="text-align: left;">0.43 ± 0.53</td>
<td style="text-align: left;">0.14 ± 0.36</td>
</tr>
<tr class="even">
<td style="text-align: left;">   cv</td>
<td style="text-align: left;">0.64</td>
<td style="text-align: left;">1.25</td>
<td style="text-align: left;">2.54</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>gear</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">   Three Gears</td>
<td style="text-align: left;">1 (9.0909%)</td>
<td style="text-align: left;">2 (28.5714%)</td>
<td style="text-align: left;">12 (85.7143%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   Four Gears</td>
<td style="text-align: left;">8 (72.7273%)</td>
<td style="text-align: left;">4 (57.1429%)</td>
<td style="text-align: left;">0 (0.0000%)</td>
</tr>
<tr class="even">
<td style="text-align: left;">   Five Gears</td>
<td style="text-align: left;">2 (18.1818%)</td>
<td style="text-align: left;">1 (14.2857%)</td>
<td style="text-align: left;">2 (14.2857%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>hp</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">52.00</td>
<td style="text-align: left;">105.00</td>
<td style="text-align: left;">150.00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean and sd</td>
<td style="text-align: left;">82.64 ± 20.93</td>
<td style="text-align: left;">122.29 ± 24.26</td>
<td style="text-align: left;">209.21 ± 50.98</td>
</tr>
<tr class="even">
<td style="text-align: left;">   cv</td>
<td style="text-align: left;">0.25</td>
<td style="text-align: left;">0.20</td>
<td style="text-align: left;">0.24</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>wt</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">1.51</td>
<td style="text-align: left;">2.62</td>
<td style="text-align: left;">3.17</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean and sd</td>
<td style="text-align: left;">2.29 ± 0.57</td>
<td style="text-align: left;">3.12 ± 0.36</td>
<td style="text-align: left;">4.00 ± 0.76</td>
</tr>
<tr class="even">
<td style="text-align: left;">   cv</td>
<td style="text-align: left;">0.25</td>
<td style="text-align: left;">0.11</td>
<td style="text-align: left;">0.19</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>MPG &gt; 25</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">  </td>
<td style="text-align: left;">6 (54.5455%)</td>
<td style="text-align: left;">0 (0.0000%)</td>
<td style="text-align: left;">0 (0.0000%)</td>
</tr>
</tbody>
</table>

    summary_table(mtcars2, qs[-which(names(qs) %in% c("cyl", "am"))], by = c("cyl", "am"))

<table>
<colgroup>
<col style="width: 16%" />
<col style="width: 13%" />
<col style="width: 13%" />
<col style="width: 14%" />
<col style="width: 13%" />
<col style="width: 14%" />
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
<td style="text-align: left;">21.50</td>
<td style="text-align: left;">17.80</td>
<td style="text-align: left;">10.40</td>
<td style="text-align: left;">21.40</td>
<td style="text-align: left;">19.70</td>
<td style="text-align: left;">15.00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean and sd</td>
<td style="text-align: left;">22.90 ± 1.45</td>
<td style="text-align: left;">19.12 ± 1.63</td>
<td style="text-align: left;">15.05 ± 2.77</td>
<td style="text-align: left;">28.07 ± 4.48</td>
<td style="text-align: left;">20.57 ± 0.75</td>
<td style="text-align: left;">15.40 ± 0.57</td>
</tr>
<tr class="even">
<td style="text-align: left;">   cv</td>
<td style="text-align: left;">0.06</td>
<td style="text-align: left;">0.09</td>
<td style="text-align: left;">0.18</td>
<td style="text-align: left;">0.16</td>
<td style="text-align: left;">0.04</td>
<td style="text-align: left;">0.04</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>gear</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">   Three Gears</td>
<td style="text-align: left;">1 (33.3333%)</td>
<td style="text-align: left;">2 (50.0000%)</td>
<td style="text-align: left;">12 (100.0000%)</td>
<td style="text-align: left;">0 (0.0000%)</td>
<td style="text-align: left;">0 (0.0000%)</td>
<td style="text-align: left;">0 (0.0000%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   Four Gears</td>
<td style="text-align: left;">2 (66.6667%)</td>
<td style="text-align: left;">2 (50.0000%)</td>
<td style="text-align: left;">0 (0.0000%)</td>
<td style="text-align: left;">6 (75.0000%)</td>
<td style="text-align: left;">2 (66.6667%)</td>
<td style="text-align: left;">0 (0.0000%)</td>
</tr>
<tr class="even">
<td style="text-align: left;">   Five Gears</td>
<td style="text-align: left;">0 (0.0000%)</td>
<td style="text-align: left;">0 (0.0000%)</td>
<td style="text-align: left;">0 (0.0000%)</td>
<td style="text-align: left;">2 (25.0000%)</td>
<td style="text-align: left;">1 (33.3333%)</td>
<td style="text-align: left;">2 (100.0000%)</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>hp</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">62.00</td>
<td style="text-align: left;">105.00</td>
<td style="text-align: left;">150.00</td>
<td style="text-align: left;">52.00</td>
<td style="text-align: left;">110.00</td>
<td style="text-align: left;">264.00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean and sd</td>
<td style="text-align: left;">84.67 ± 19.66</td>
<td style="text-align: left;">115.25 ± 9.18</td>
<td style="text-align: left;">194.17 ± 33.36</td>
<td style="text-align: left;">81.88 ± 22.66</td>
<td style="text-align: left;">131.67 ± 37.53</td>
<td style="text-align: left;">299.50 ± 50.20</td>
</tr>
<tr class="even">
<td style="text-align: left;">   cv</td>
<td style="text-align: left;">0.23</td>
<td style="text-align: left;">0.08</td>
<td style="text-align: left;">0.17</td>
<td style="text-align: left;">0.28</td>
<td style="text-align: left;">0.29</td>
<td style="text-align: left;">0.17</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>wt</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">   min</td>
<td style="text-align: left;">2.46</td>
<td style="text-align: left;">3.21</td>
<td style="text-align: left;">3.44</td>
<td style="text-align: left;">1.51</td>
<td style="text-align: left;">2.62</td>
<td style="text-align: left;">3.17</td>
</tr>
<tr class="odd">
<td style="text-align: left;">   mean and sd</td>
<td style="text-align: left;">2.94 ± 0.41</td>
<td style="text-align: left;">3.39 ± 0.12</td>
<td style="text-align: left;">4.10 ± 0.77</td>
<td style="text-align: left;">2.04 ± 0.41</td>
<td style="text-align: left;">2.75 ± 0.13</td>
<td style="text-align: left;">3.37 ± 0.28</td>
</tr>
<tr class="even">
<td style="text-align: left;">   cv</td>
<td style="text-align: left;">0.14</td>
<td style="text-align: left;">0.03</td>
<td style="text-align: left;">0.19</td>
<td style="text-align: left;">0.20</td>
<td style="text-align: left;">0.05</td>
<td style="text-align: left;">0.08</td>
</tr>
<tr class="odd">
<td style="text-align: left;"><strong>MPG &gt; 25</strong></td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
<td style="text-align: left;">  </td>
</tr>
<tr class="even">
<td style="text-align: left;">  </td>
<td style="text-align: left;">0 (0.0000%)</td>
<td style="text-align: left;">0 (0.0000%)</td>
<td style="text-align: left;">0 (0.0000%)</td>
<td style="text-align: left;">6 (75.0000%)</td>
<td style="text-align: left;">0 (0.0000%)</td>
<td style="text-align: left;">0 (0.0000%)</td>
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
    #>  ellipsis      0.3.1      2020-05-15 [1] CRAN (R 4.0.0)
    #>  evaluate      0.14       2019-05-28 [1] CRAN (R 4.0.0)
    #>  fansi         0.4.1      2020-01-08 [1] CRAN (R 4.0.0)
    #>  fs            1.5.0      2020-07-31 [1] CRAN (R 4.0.2)
    #>  glue          1.4.1      2020-05-13 [1] CRAN (R 4.0.0)
    #>  highr         0.8        2019-03-20 [1] CRAN (R 4.0.0)
    #>  htmltools     0.5.0      2020-06-16 [1] CRAN (R 4.0.0)
    #>  knitr         1.29       2020-06-23 [1] CRAN (R 4.0.0)
    #>  magrittr      1.5        2014-11-22 [1] CRAN (R 4.0.0)
    #>  memoise       1.1.0      2017-04-21 [1] CRAN (R 4.0.0)
    #>  pkgbuild      1.1.0      2020-07-13 [1] CRAN (R 4.0.2)
    #>  pkgload       1.1.0      2020-05-29 [1] CRAN (R 4.0.0)
    #>  prettyunits   1.1.1      2020-01-24 [1] CRAN (R 4.0.0)
    #>  processx      3.4.3      2020-07-05 [1] CRAN (R 4.0.0)
    #>  ps            1.3.4      2020-08-11 [1] CRAN (R 4.0.2)
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
    #>  usethis       1.6.1      2020-04-29 [1] CRAN (R 4.0.0)
    #>  withr         2.2.0      2020-04-20 [1] CRAN (R 4.0.0)
    #>  xfun          0.16       2020-07-24 [1] CRAN (R 4.0.2)
    #>  yaml          2.2.1      2020-02-01 [1] CRAN (R 4.0.0)
    #> 
    #> [1] /Library/Frameworks/R.framework/Versions/4.0/Resources/library

</details>
