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

names(summ_stats)[2] <- "Weight"
names(summ_stats)

summary_table(my_data, summ_stats)
