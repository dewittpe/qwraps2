```r
library(qwraps2)
options(qwraps2_markup = "markdown")

my_data <- mtcars[c("mpg", "wt")]

summ_stats <-
  qsummary(my_data,
           numeric_summaries = list("mean (sd)" = "~ qwraps2::mean_sd(%s)",
                                    "min"       = "~ qwraps2::frmt(min(%s))",
                                    "max"       = "~ qwraps2::frmt(max(%s))",
                                    "median (IQR)" = "~ qwraps2::median_iqr(%s)"))
names(summ_stats)
# [1] "mpg" "wt" 

names(summ_stats)[2] <- "Weight"
names(summ_stats)
# [1] "mpg"    "Weight"

summary_table(my_data, summ_stats)
```


|                          |my_data (N = 32)     |
|:-------------------------|:--------------------|
|**mpg**                   |&nbsp;&nbsp;         |
|&nbsp;&nbsp; mean (sd)    |20.09 &plusmn; 6.03  |
|&nbsp;&nbsp; min          |10.40                |
|&nbsp;&nbsp; max          |33.90                |
|&nbsp;&nbsp; median (IQR) |19.20 (15.43, 22.80) |
|**Weight**                |&nbsp;&nbsp;         |
|&nbsp;&nbsp; mean (sd)    |3.22 &plusmn; 0.98   |
|&nbsp;&nbsp; min          |1.51                 |
|&nbsp;&nbsp; max          |5.42                 |
|&nbsp;&nbsp; median (IQR) |3.33 (2.58, 3.61)    |



