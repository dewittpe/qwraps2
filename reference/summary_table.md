# Data Summary Tables

Tools useful for building data summary tables.

## Usage

``` r
summary_table(x, summaries = qsummary(x), by = NULL, qable_args = list(), ...)

qsummary(x, numeric_summaries, n_perc_args, env = parent.frame())
```

## Arguments

- x:

  a `data.frame`.

- summaries:

  a list of lists of formulas for summarizing the data set. See Details
  and examples.

- by:

  a character vector of variable names to generate the summary by, that
  is, one column for each unique value or combination of values of the
  variables specified.

- qable_args:

  additional values passed to
  [`qable`](http://www.peteredewitt.com/qwraps2/reference/qable.md)

- ...:

  pass through

- numeric_summaries:

  a list of functions to use for summarizing numeric variables. The
  functions need to be provided as character strings with the single
  argument defined by the `%s` symbol.

- n_perc_args:

  a list of arguments to pass to
  [`n_perc`](http://www.peteredewitt.com/qwraps2/reference/n_perc.md) to
  be used with `character` or `factor` variables within `x`.

- env:

  environment to assign to the resulting formulae

## Value

a `qwraps2_summary_table` object.

## Details

`summary_table` can be used to generate good-looking, simple tables in
LaTeX or markdown. Functions like `xtable::print.xtable` and
`Hmisc::latex` provide many more tools for formatting tables. The
purpose of `summary_table` is to generate readable tables quickly while
summarizing a data set.

Creating a list-of-lists of summary functions to apply to a data set
will allow the exploration of the whole data set and grouped data sets.
In the example provided on this page we see a set of summary measures
for the [`mtcars`](https://rdrr.io/r/datasets/mtcars.html) data set and
the construction of a table for the whole data set and for a grouped
data set.

The list-of-lists should be thought of as follows: the outer list
defines row groups, the inner lists define the rows within each row
group.

More detailed use of these functions can be found in the
[`vignette("qwraps2-summary-table", package = "qwraps2")`](http://www.peteredewitt.com/qwraps2/articles/qwraps2-summary-table.md).

The `print` method for the `qwraps2_summary_table` objects is just a
simple wrapper for
[`qable`](http://www.peteredewitt.com/qwraps2/reference/qable.md).

## See also

`qsummary` for generating the summaries,
[`qable`](http://www.peteredewitt.com/qwraps2/reference/qable.md) for
marking up `qwraps2_summary_table` objects. The
[`vignette("qwraps2-summary-table", package = "qwraps2")`](http://www.peteredewitt.com/qwraps2/articles/qwraps2-summary-table.md)
for detailed use of these functions and caveats.

## Examples

``` r
summary_table(mtcars[, c("mpg", "cyl", "am")])
#> 
#> \begin{tabular}{l|l}
#> \hline
#>  & mtcars[, c("mpg", "cyl", "am")] (N = 32)\\
#> \hline
#> \bf{mpg} & ~\\
#> \hline
#> ~~ minimum & 10.40\\
#> \hline
#> ~~ median (IQR) & 19.20 (15.43, 22.80)\\
#> \hline
#> ~~ mean (sd) & 20.09 $\pm$ 6.03\\
#> \hline
#> ~~ maximum & 33.90\\
#> \hline
#> \bf{cyl} & ~\\
#> \hline
#> ~~ minimum & 4.00\\
#> \hline
#> ~~ median (IQR) & 6.00 (4.00, 8.00)\\
#> \hline
#> ~~ mean (sd) & 6.19 $\pm$ 1.79\\
#> \hline
#> ~~ maximum & 8.00\\
#> \hline
#> \bf{am} & ~\\
#> \hline
#> ~~ minimum & 0.00\\
#> \hline
#> ~~ median (IQR) & 0.00 (0.00, 1.00)\\
#> \hline
#> ~~ mean (sd) & 0.41 $\pm$ 0.50\\
#> \hline
#> ~~ maximum & 1.00\\
#> \hline
#> \end{tabular}
summary_table(mtcars[, c("mpg", "cyl", "am")], by = "am")
#> 
#> \begin{tabular}{l|l|l}
#> \hline
#>  & 0 (N = 19) & 1 (N = 13)\\
#> \hline
#> \bf{mpg} & ~ & ~\\
#> \hline
#> ~~ minimum & 10.40 & 15.00\\
#> \hline
#> ~~ median (IQR) & 17.30 (14.95, 19.20) & 22.80 (21.00, 30.40)\\
#> \hline
#> ~~ mean (sd) & 17.15 $\pm$ 3.83 & 24.39 $\pm$ 6.17\\
#> \hline
#> ~~ maximum & 24.40 & 33.90\\
#> \hline
#> \bf{cyl} & ~ & ~\\
#> \hline
#> ~~ minimum & 4.00 & 4.00\\
#> \hline
#> ~~ median (IQR) & 8.00 (6.00, 8.00) & 4.00 (4.00, 6.00)\\
#> \hline
#> ~~ mean (sd) & 6.95 $\pm$ 1.54 & 5.08 $\pm$ 1.55\\
#> \hline
#> ~~ maximum & 8.00 & 8.00\\
#> \hline
#> \bf{am} & ~ & ~\\
#> \hline
#> ~~ minimum & 0.00 & 1.00\\
#> \hline
#> ~~ median (IQR) & 0.00 (0.00, 0.00) & 1.00 (1.00, 1.00)\\
#> \hline
#> ~~ mean (sd) & 0.00 $\pm$ 0.00 & 1.00 $\pm$ 0.00\\
#> \hline
#> ~~ maximum & 0.00 & 1.00\\
#> \hline
#> \end{tabular}

# A list-of-lists for the summaries arg.  This object is of the basic form:
#
# list("row group A" =
#      list("row 1A" = ~ <summary function>,
#           "row 2A" = ~ <summary function>),
#      "row group B" =
#      list("row 1B" = ~ <summary function>,
#           "row 2B" = ~ <summary function>,
#           "row 3B" = ~ <summary function>))

our_summaries <-
  list("Miles Per Gallon" =
         list("min"  = ~ min(mpg),
              "mean" = ~ mean(mpg),
              "mean &plusmn; sd" = ~ qwraps2::mean_sd(mpg),
              "max"  = ~ max(mpg)),
       "Weight" =
         list("median" = ~ median(wt)),
       "Cylinders" =
         list("4 cyl: n (%)" = ~ qwraps2::n_perc0(cyl == 4),
              "6 cyl: n (%)" = ~ qwraps2::n_perc0(cyl == 6),
              "8 cyl: n (%)" = ~ qwraps2::n_perc0(cyl == 8)))

# Going to use markdown for the markup language in this example,  the original
# option will be reset at the end of the example.
orig_opt <- options()$qwraps2_markup
options(qwraps2_markup = "markdown")

# The summary table for the whole mtcars data set
whole_table <- summary_table(mtcars, our_summaries)
whole_table
#> 
#> 
#> |                              |mtcars (N = 32)     |
#> |:-----------------------------|:-------------------|
#> |**Miles Per Gallon**          |&nbsp;&nbsp;        |
#> |&nbsp;&nbsp; min              |10.4                |
#> |&nbsp;&nbsp; mean             |20.090625           |
#> |&nbsp;&nbsp; mean &plusmn; sd |20.09 &plusmn; 6.03 |
#> |&nbsp;&nbsp; max              |33.9                |
#> |**Weight**                    |&nbsp;&nbsp;        |
#> |&nbsp;&nbsp; median           |3.325               |
#> |**Cylinders**                 |&nbsp;&nbsp;        |
#> |&nbsp;&nbsp; 4 cyl: n (%)     |11 (34)             |
#> |&nbsp;&nbsp; 6 cyl: n (%)     |7 (22)              |
#> |&nbsp;&nbsp; 8 cyl: n (%)     |14 (44)             |

# The summary table for mtcars grouped by am (automatic or manual transmission)
# This will generate one column for each level of mtcars$am
grouped_by_table <-
  summary_table(mtcars, our_summaries, by = "am")
grouped_by_table
#> 
#> 
#> |                              |0 (N = 19)          |1 (N = 13)          |
#> |:-----------------------------|:-------------------|:-------------------|
#> |**Miles Per Gallon**          |&nbsp;&nbsp;        |&nbsp;&nbsp;        |
#> |&nbsp;&nbsp; min              |10.4                |15                  |
#> |&nbsp;&nbsp; mean             |17.1473684210526    |24.3923076923077    |
#> |&nbsp;&nbsp; mean &plusmn; sd |17.15 &plusmn; 3.83 |24.39 &plusmn; 6.17 |
#> |&nbsp;&nbsp; max              |24.4                |33.9                |
#> |**Weight**                    |&nbsp;&nbsp;        |&nbsp;&nbsp;        |
#> |&nbsp;&nbsp; median           |3.52                |2.32                |
#> |**Cylinders**                 |&nbsp;&nbsp;        |&nbsp;&nbsp;        |
#> |&nbsp;&nbsp; 4 cyl: n (%)     |3 (16)              |8 (62)              |
#> |&nbsp;&nbsp; 6 cyl: n (%)     |4 (21)              |3 (23)              |
#> |&nbsp;&nbsp; 8 cyl: n (%)     |12 (63)             |2 (15)              |

# an equivalent call if you are using the tidyverse:
summary_table(dplyr::group_by(mtcars, am), our_summaries)
#> Warning: grouped_df detected. Setting `by` argument to
#>   c('am')
#> 
#> 
#> |                              |0 (N = 19)          |1 (N = 13)          |
#> |:-----------------------------|:-------------------|:-------------------|
#> |**Miles Per Gallon**          |&nbsp;&nbsp;        |&nbsp;&nbsp;        |
#> |&nbsp;&nbsp; min              |10.4                |15                  |
#> |&nbsp;&nbsp; mean             |17.1473684210526    |24.3923076923077    |
#> |&nbsp;&nbsp; mean &plusmn; sd |17.15 &plusmn; 3.83 |24.39 &plusmn; 6.17 |
#> |&nbsp;&nbsp; max              |24.4                |33.9                |
#> |**Weight**                    |&nbsp;&nbsp;        |&nbsp;&nbsp;        |
#> |&nbsp;&nbsp; median           |3.52                |2.32                |
#> |**Cylinders**                 |&nbsp;&nbsp;        |&nbsp;&nbsp;        |
#> |&nbsp;&nbsp; 4 cyl: n (%)     |3 (16)              |8 (62)              |
#> |&nbsp;&nbsp; 6 cyl: n (%)     |4 (21)              |3 (23)              |
#> |&nbsp;&nbsp; 8 cyl: n (%)     |12 (63)             |2 (15)              |

# To build a table with a column for the whole data set and each of the am
# levels
cbind(whole_table, grouped_by_table)
#> 
#> 
#> |                              |mtcars (N = 32)     |0 (N = 19)          |1 (N = 13)          |
#> |:-----------------------------|:-------------------|:-------------------|:-------------------|
#> |**Miles Per Gallon**          |&nbsp;&nbsp;        |&nbsp;&nbsp;        |&nbsp;&nbsp;        |
#> |&nbsp;&nbsp; min              |10.4                |10.4                |15                  |
#> |&nbsp;&nbsp; mean             |20.090625           |17.1473684210526    |24.3923076923077    |
#> |&nbsp;&nbsp; mean &plusmn; sd |20.09 &plusmn; 6.03 |17.15 &plusmn; 3.83 |24.39 &plusmn; 6.17 |
#> |&nbsp;&nbsp; max              |33.9                |24.4                |33.9                |
#> |**Weight**                    |&nbsp;&nbsp;        |&nbsp;&nbsp;        |&nbsp;&nbsp;        |
#> |&nbsp;&nbsp; median           |3.325               |3.52                |2.32                |
#> |**Cylinders**                 |&nbsp;&nbsp;        |&nbsp;&nbsp;        |&nbsp;&nbsp;        |
#> |&nbsp;&nbsp; 4 cyl: n (%)     |11 (34)             |3 (16)              |8 (62)              |
#> |&nbsp;&nbsp; 6 cyl: n (%)     |7 (22)              |4 (21)              |3 (23)              |
#> |&nbsp;&nbsp; 8 cyl: n (%)     |14 (44)             |12 (63)             |2 (15)              |

# Adding a caption for a LaTeX table
print(whole_table, caption = "Hello world", markup = "latex")
#> 
#> 
#> Table: Hello world
#> 
#> |                              |mtcars (N = 32)     |
#> |:-----------------------------|:-------------------|
#> |**Miles Per Gallon**          |&nbsp;&nbsp;        |
#> |&nbsp;&nbsp; min              |10.4                |
#> |&nbsp;&nbsp; mean             |20.090625           |
#> |&nbsp;&nbsp; mean &plusmn; sd |20.09 &plusmn; 6.03 |
#> |&nbsp;&nbsp; max              |33.9                |
#> |**Weight**                    |&nbsp;&nbsp;        |
#> |&nbsp;&nbsp; median           |3.325               |
#> |**Cylinders**                 |&nbsp;&nbsp;        |
#> |&nbsp;&nbsp; 4 cyl: n (%)     |11 (34)             |
#> |&nbsp;&nbsp; 6 cyl: n (%)     |7 (22)              |
#> |&nbsp;&nbsp; 8 cyl: n (%)     |14 (44)             |

# A **warning** about grouped_df objects.
# If you use dplyr::group_by or
# dplyr::rowwise to manipulate a data set and fail to use dplyr::ungroup you
# might find a table that takes a long time to create and does not summarize the
# data as expected.  For example, let's build a data set with twenty subjects
# and injury severity scores for head and face injuries.  We'll clean the data
# by finding the max ISS score for each subject and then reporting summary
# statistics thereof.
set.seed(42)
dat <- data.frame(id = letters[1:20],
                  head_iss = sample(1:6, 20, replace = TRUE, prob = 10 * (6:1)),
                  face_iss = sample(1:6, 20, replace = TRUE, prob = 10 * (6:1)))
dat <- dplyr::group_by(dat, id)
dat <- dplyr::mutate(dat, iss = max(head_iss, face_iss))

iss_summary <-
  list("Head ISS" =
       list("min"    = ~ min(head_iss),
            "median" = ~ median(head_iss),
            "max"    = ~ max(head_iss)),
       "Face ISS" =
       list("min"    = ~ min(face_iss),
            "median" = ~ median(face_iss),
            "max"    = ~ max(face_iss)),
       "Max ISS" =
       list("min"    = ~ min(iss),
            "median" = ~ median(iss),
            "max"    = ~ max(iss)))

# Want: a table with one column for all subjects with nine rows divided up into
# three row groups.  However, the following call will create a table with 20
# columns, one for each subject because dat is a grouped_df
summary_table(dat, iss_summary)
#> Warning: grouped_df detected. Setting `by` argument to
#>   c('id')
#> 
#> 
#> |                    |a (N = 1)    |b (N = 1)    |c (N = 1)    |d (N = 1)    |e (N = 1)    |f (N = 1)    |g (N = 1)    |h (N = 1)    |i (N = 1)    |j (N = 1)    |k (N = 1)    |l (N = 1)    |m (N = 1)    |n (N = 1)    |o (N = 1)    |p (N = 1)    |q (N = 1)    |r (N = 1)    |s (N = 1)    |t (N = 1)    |
#> |:-------------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|
#> |**Head ISS**        |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |
#> |&nbsp;&nbsp; min    |5            |5            |2            |4            |3            |2            |4            |1            |3            |3            |2            |4            |5            |1            |2            |5            |6            |1            |2            |3            |
#> |&nbsp;&nbsp; median |5            |5            |2            |4            |3            |2            |4            |1            |3            |3            |2            |4            |5            |1            |2            |5            |6            |1            |2            |3            |
#> |&nbsp;&nbsp; max    |5            |5            |2            |4            |3            |2            |4            |1            |3            |3            |2            |4            |5            |1            |2            |5            |6            |1            |2            |3            |
#> |**Face ISS**        |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |
#> |&nbsp;&nbsp; min    |5            |1            |6            |5            |1            |2            |2            |5            |2            |4            |4            |4            |2            |3            |1            |4            |1            |1            |5            |3            |
#> |&nbsp;&nbsp; median |5            |1            |6            |5            |1            |2            |2            |5            |2            |4            |4            |4            |2            |3            |1            |4            |1            |1            |5            |3            |
#> |&nbsp;&nbsp; max    |5            |1            |6            |5            |1            |2            |2            |5            |2            |4            |4            |4            |2            |3            |1            |4            |1            |1            |5            |3            |
#> |**Max ISS**         |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |&nbsp;&nbsp; |
#> |&nbsp;&nbsp; min    |5            |5            |6            |5            |3            |2            |4            |5            |3            |4            |4            |4            |5            |3            |2            |5            |6            |1            |5            |3            |
#> |&nbsp;&nbsp; median |5            |5            |6            |5            |3            |2            |4            |5            |3            |4            |4            |4            |5            |3            |2            |5            |6            |1            |5            |3            |
#> |&nbsp;&nbsp; max    |5            |5            |6            |5            |3            |2            |4            |5            |3            |4            |4            |4            |5            |3            |2            |5            |6            |1            |5            |3            |

# Ungroup the data.frame to get the correct output
summary_table(dplyr::ungroup(dat), iss_summary)
#> 
#> 
#> |                    |dplyr::ungroup(dat) (N = 20) |
#> |:-------------------|:----------------------------|
#> |**Head ISS**        |&nbsp;&nbsp;                 |
#> |&nbsp;&nbsp; min    |1                            |
#> |&nbsp;&nbsp; median |3                            |
#> |&nbsp;&nbsp; max    |6                            |
#> |**Face ISS**        |&nbsp;&nbsp;                 |
#> |&nbsp;&nbsp; min    |1                            |
#> |&nbsp;&nbsp; median |3                            |
#> |&nbsp;&nbsp; max    |6                            |
#> |**Max ISS**         |&nbsp;&nbsp;                 |
#> |&nbsp;&nbsp; min    |1                            |
#> |&nbsp;&nbsp; median |4                            |
#> |&nbsp;&nbsp; max    |6                            |


################################################################################
# The Default call will work with non-syntactically valid names and will
# generate a table with statistics defined by the qsummary call.
summary_table(mtcars, by = "cyl")
#> 
#> 
#> |                          |4 (N = 11)             |6 (N = 7)               |8 (N = 14)              |
#> |:-------------------------|:----------------------|:-----------------------|:-----------------------|
#> |**mpg**                   |&nbsp;&nbsp;           |&nbsp;&nbsp;            |&nbsp;&nbsp;            |
#> |&nbsp;&nbsp; minimum      |21.40                  |17.80                   |10.40                   |
#> |&nbsp;&nbsp; median (IQR) |26.00 (22.80, 30.40)   |19.70 (18.65, 21.00)    |15.20 (14.40, 16.25)    |
#> |&nbsp;&nbsp; mean (sd)    |26.66 &plusmn; 4.51    |19.74 &plusmn; 1.45     |15.10 &plusmn; 2.56     |
#> |&nbsp;&nbsp; maximum      |33.90                  |21.40                   |19.20                   |
#> |**cyl**                   |&nbsp;&nbsp;           |&nbsp;&nbsp;            |&nbsp;&nbsp;            |
#> |&nbsp;&nbsp; minimum      |4.00                   |6.00                    |8.00                    |
#> |&nbsp;&nbsp; median (IQR) |4.00 (4.00, 4.00)      |6.00 (6.00, 6.00)       |8.00 (8.00, 8.00)       |
#> |&nbsp;&nbsp; mean (sd)    |4.00 &plusmn; 0.00     |6.00 &plusmn; 0.00      |8.00 &plusmn; 0.00      |
#> |&nbsp;&nbsp; maximum      |4.00                   |6.00                    |8.00                    |
#> |**disp**                  |&nbsp;&nbsp;           |&nbsp;&nbsp;            |&nbsp;&nbsp;            |
#> |&nbsp;&nbsp; minimum      |71.10                  |145.00                  |275.80                  |
#> |&nbsp;&nbsp; median (IQR) |108.00 (78.85, 120.65) |167.60 (160.00, 196.30) |350.50 (301.75, 390.00) |
#> |&nbsp;&nbsp; mean (sd)    |105.14 &plusmn; 26.87  |183.31 &plusmn; 41.56   |353.10 &plusmn; 67.77   |
#> |&nbsp;&nbsp; maximum      |146.70                 |258.00                  |472.00                  |
#> |**hp**                    |&nbsp;&nbsp;           |&nbsp;&nbsp;            |&nbsp;&nbsp;            |
#> |&nbsp;&nbsp; minimum      |52.00                  |105.00                  |150.00                  |
#> |&nbsp;&nbsp; median (IQR) |91.00 (65.50, 96.00)   |110.00 (110.00, 123.00) |192.50 (176.25, 241.25) |
#> |&nbsp;&nbsp; mean (sd)    |82.64 &plusmn; 20.93   |122.29 &plusmn; 24.26   |209.21 &plusmn; 50.98   |
#> |&nbsp;&nbsp; maximum      |113.00                 |175.00                  |335.00                  |
#> |**drat**                  |&nbsp;&nbsp;           |&nbsp;&nbsp;            |&nbsp;&nbsp;            |
#> |&nbsp;&nbsp; minimum      |3.69                   |2.76                    |2.76                    |
#> |&nbsp;&nbsp; median (IQR) |4.08 (3.81, 4.17)      |3.90 (3.35, 3.91)       |3.12 (3.07, 3.22)       |
#> |&nbsp;&nbsp; mean (sd)    |4.07 &plusmn; 0.37     |3.59 &plusmn; 0.48      |3.23 &plusmn; 0.37      |
#> |&nbsp;&nbsp; maximum      |4.93                   |3.92                    |4.22                    |
#> |**wt**                    |&nbsp;&nbsp;           |&nbsp;&nbsp;            |&nbsp;&nbsp;            |
#> |&nbsp;&nbsp; minimum      |1.51                   |2.62                    |3.17                    |
#> |&nbsp;&nbsp; median (IQR) |2.20 (1.89, 2.62)      |3.21 (2.82, 3.44)       |3.75 (3.53, 4.01)       |
#> |&nbsp;&nbsp; mean (sd)    |2.29 &plusmn; 0.57     |3.12 &plusmn; 0.36      |4.00 &plusmn; 0.76      |
#> |&nbsp;&nbsp; maximum      |3.19                   |3.46                    |5.42                    |
#> |**qsec**                  |&nbsp;&nbsp;           |&nbsp;&nbsp;            |&nbsp;&nbsp;            |
#> |&nbsp;&nbsp; minimum      |16.70                  |15.50                   |14.50                   |
#> |&nbsp;&nbsp; median (IQR) |18.90 (18.56, 19.95)   |18.30 (16.74, 19.17)    |17.18 (16.10, 17.55)    |
#> |&nbsp;&nbsp; mean (sd)    |19.14 &plusmn; 1.68    |17.98 &plusmn; 1.71     |16.77 &plusmn; 1.20     |
#> |&nbsp;&nbsp; maximum      |22.90                  |20.22                   |18.00                   |
#> |**vs**                    |&nbsp;&nbsp;           |&nbsp;&nbsp;            |&nbsp;&nbsp;            |
#> |&nbsp;&nbsp; minimum      |0.00                   |0.00                    |0.00                    |
#> |&nbsp;&nbsp; median (IQR) |1.00 (1.00, 1.00)      |1.00 (0.00, 1.00)       |0.00 (0.00, 0.00)       |
#> |&nbsp;&nbsp; mean (sd)    |0.91 &plusmn; 0.30     |0.57 &plusmn; 0.53      |0.00 &plusmn; 0.00      |
#> |&nbsp;&nbsp; maximum      |1.00                   |1.00                    |0.00                    |
#> |**am**                    |&nbsp;&nbsp;           |&nbsp;&nbsp;            |&nbsp;&nbsp;            |
#> |&nbsp;&nbsp; minimum      |0.00                   |0.00                    |0.00                    |
#> |&nbsp;&nbsp; median (IQR) |1.00 (0.50, 1.00)      |0.00 (0.00, 1.00)       |0.00 (0.00, 0.00)       |
#> |&nbsp;&nbsp; mean (sd)    |0.73 &plusmn; 0.47     |0.43 &plusmn; 0.53      |0.14 &plusmn; 0.36      |
#> |&nbsp;&nbsp; maximum      |1.00                   |1.00                    |1.00                    |
#> |**gear**                  |&nbsp;&nbsp;           |&nbsp;&nbsp;            |&nbsp;&nbsp;            |
#> |&nbsp;&nbsp; minimum      |3.00                   |3.00                    |3.00                    |
#> |&nbsp;&nbsp; median (IQR) |4.00 (4.00, 4.00)      |4.00 (3.50, 4.00)       |3.00 (3.00, 3.00)       |
#> |&nbsp;&nbsp; mean (sd)    |4.09 &plusmn; 0.54     |3.86 &plusmn; 0.69      |3.29 &plusmn; 0.73      |
#> |&nbsp;&nbsp; maximum      |5.00                   |5.00                    |5.00                    |
#> |**carb**                  |&nbsp;&nbsp;           |&nbsp;&nbsp;            |&nbsp;&nbsp;            |
#> |&nbsp;&nbsp; minimum      |1.00                   |1.00                    |2.00                    |
#> |&nbsp;&nbsp; median (IQR) |2.00 (1.00, 2.00)      |4.00 (2.50, 4.00)       |3.50 (2.25, 4.00)       |
#> |&nbsp;&nbsp; mean (sd)    |1.55 &plusmn; 0.52     |3.43 &plusmn; 1.81      |3.50 &plusmn; 1.56      |
#> |&nbsp;&nbsp; maximum      |2.00                   |6.00                    |8.00                    |

# Another example from the diamonds data
data("diamonds", package = "ggplot2")
diamonds["The Price"] <- diamonds$price
diamonds["A Logical"] <- sample(c(TRUE, FALSE), size = nrow(diamonds), replace = TRUE)

# the next two lines are equivalent.
summary_table(diamonds)
#> 
#> 
#> |                          |diamonds (N = 53,940)       |
#> |:-------------------------|:---------------------------|
#> |**carat**                 |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; minimum      |0.20                        |
#> |&nbsp;&nbsp; median (IQR) |0.70 (0.40, 1.04)           |
#> |&nbsp;&nbsp; mean (sd)    |0.80 &plusmn; 0.47          |
#> |&nbsp;&nbsp; maximum      |5.01                        |
#> |**cut**                   |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; Fair         |1,610 (3)                   |
#> |&nbsp;&nbsp; Good         |4,906 (9)                   |
#> |&nbsp;&nbsp; Very Good    |12,082 (22)                 |
#> |&nbsp;&nbsp; Premium      |13,791 (26)                 |
#> |&nbsp;&nbsp; Ideal        |21,551 (40)                 |
#> |**color**                 |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; D            |6,775 (13)                  |
#> |&nbsp;&nbsp; E            |9,797 (18)                  |
#> |&nbsp;&nbsp; F            |9,542 (18)                  |
#> |&nbsp;&nbsp; G            |11,292 (21)                 |
#> |&nbsp;&nbsp; H            |8,304 (15)                  |
#> |&nbsp;&nbsp; I            |5,422 (10)                  |
#> |&nbsp;&nbsp; J            |2,808 (5)                   |
#> |**clarity**               |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; I1           |741 (1)                     |
#> |&nbsp;&nbsp; SI2          |9,194 (17)                  |
#> |&nbsp;&nbsp; SI1          |13,065 (24)                 |
#> |&nbsp;&nbsp; VS2          |12,258 (23)                 |
#> |&nbsp;&nbsp; VS1          |8,171 (15)                  |
#> |&nbsp;&nbsp; VVS2         |5,066 (9)                   |
#> |&nbsp;&nbsp; VVS1         |3,655 (7)                   |
#> |&nbsp;&nbsp; IF           |1,790 (3)                   |
#> |**depth**                 |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; minimum      |43.00                       |
#> |&nbsp;&nbsp; median (IQR) |61.80 (61.00, 62.50)        |
#> |&nbsp;&nbsp; mean (sd)    |61.75 &plusmn; 1.43         |
#> |&nbsp;&nbsp; maximum      |79.00                       |
#> |**table**                 |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; minimum      |43.00                       |
#> |&nbsp;&nbsp; median (IQR) |57.00 (56.00, 59.00)        |
#> |&nbsp;&nbsp; mean (sd)    |57.46 &plusmn; 2.23         |
#> |&nbsp;&nbsp; maximum      |95.00                       |
#> |**price**                 |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; minimum      |326                         |
#> |&nbsp;&nbsp; median (IQR) |2,401.00 (950.00, 5,324.25) |
#> |&nbsp;&nbsp; mean (sd)    |3,932.80 &plusmn; 3,989.44  |
#> |&nbsp;&nbsp; maximum      |18,823                      |
#> |**x**                     |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; minimum      |0.00                        |
#> |&nbsp;&nbsp; median (IQR) |5.70 (4.71, 6.54)           |
#> |&nbsp;&nbsp; mean (sd)    |5.73 &plusmn; 1.12          |
#> |&nbsp;&nbsp; maximum      |10.74                       |
#> |**y**                     |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; minimum      |0.00                        |
#> |&nbsp;&nbsp; median (IQR) |5.71 (4.72, 6.54)           |
#> |&nbsp;&nbsp; mean (sd)    |5.73 &plusmn; 1.14          |
#> |&nbsp;&nbsp; maximum      |58.90                       |
#> |**z**                     |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; minimum      |0.00                        |
#> |&nbsp;&nbsp; median (IQR) |3.53 (2.91, 4.04)           |
#> |&nbsp;&nbsp; mean (sd)    |3.54 &plusmn; 0.71          |
#> |&nbsp;&nbsp; maximum      |31.80                       |
#> |**The Price**             |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; minimum      |326                         |
#> |&nbsp;&nbsp; median (IQR) |2,401.00 (950.00, 5,324.25) |
#> |&nbsp;&nbsp; mean (sd)    |3,932.80 &plusmn; 3,989.44  |
#> |&nbsp;&nbsp; maximum      |18,823                      |
#> |**A Logical**             |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp;              |27,214 (50)                 |
summary_table(diamonds, qsummary(diamonds))
#> 
#> 
#> |                          |diamonds (N = 53,940)       |
#> |:-------------------------|:---------------------------|
#> |**carat**                 |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; minimum      |0.20                        |
#> |&nbsp;&nbsp; median (IQR) |0.70 (0.40, 1.04)           |
#> |&nbsp;&nbsp; mean (sd)    |0.80 &plusmn; 0.47          |
#> |&nbsp;&nbsp; maximum      |5.01                        |
#> |**cut**                   |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; Fair         |1,610 (3)                   |
#> |&nbsp;&nbsp; Good         |4,906 (9)                   |
#> |&nbsp;&nbsp; Very Good    |12,082 (22)                 |
#> |&nbsp;&nbsp; Premium      |13,791 (26)                 |
#> |&nbsp;&nbsp; Ideal        |21,551 (40)                 |
#> |**color**                 |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; D            |6,775 (13)                  |
#> |&nbsp;&nbsp; E            |9,797 (18)                  |
#> |&nbsp;&nbsp; F            |9,542 (18)                  |
#> |&nbsp;&nbsp; G            |11,292 (21)                 |
#> |&nbsp;&nbsp; H            |8,304 (15)                  |
#> |&nbsp;&nbsp; I            |5,422 (10)                  |
#> |&nbsp;&nbsp; J            |2,808 (5)                   |
#> |**clarity**               |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; I1           |741 (1)                     |
#> |&nbsp;&nbsp; SI2          |9,194 (17)                  |
#> |&nbsp;&nbsp; SI1          |13,065 (24)                 |
#> |&nbsp;&nbsp; VS2          |12,258 (23)                 |
#> |&nbsp;&nbsp; VS1          |8,171 (15)                  |
#> |&nbsp;&nbsp; VVS2         |5,066 (9)                   |
#> |&nbsp;&nbsp; VVS1         |3,655 (7)                   |
#> |&nbsp;&nbsp; IF           |1,790 (3)                   |
#> |**depth**                 |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; minimum      |43.00                       |
#> |&nbsp;&nbsp; median (IQR) |61.80 (61.00, 62.50)        |
#> |&nbsp;&nbsp; mean (sd)    |61.75 &plusmn; 1.43         |
#> |&nbsp;&nbsp; maximum      |79.00                       |
#> |**table**                 |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; minimum      |43.00                       |
#> |&nbsp;&nbsp; median (IQR) |57.00 (56.00, 59.00)        |
#> |&nbsp;&nbsp; mean (sd)    |57.46 &plusmn; 2.23         |
#> |&nbsp;&nbsp; maximum      |95.00                       |
#> |**price**                 |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; minimum      |326                         |
#> |&nbsp;&nbsp; median (IQR) |2,401.00 (950.00, 5,324.25) |
#> |&nbsp;&nbsp; mean (sd)    |3,932.80 &plusmn; 3,989.44  |
#> |&nbsp;&nbsp; maximum      |18,823                      |
#> |**x**                     |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; minimum      |0.00                        |
#> |&nbsp;&nbsp; median (IQR) |5.70 (4.71, 6.54)           |
#> |&nbsp;&nbsp; mean (sd)    |5.73 &plusmn; 1.12          |
#> |&nbsp;&nbsp; maximum      |10.74                       |
#> |**y**                     |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; minimum      |0.00                        |
#> |&nbsp;&nbsp; median (IQR) |5.71 (4.72, 6.54)           |
#> |&nbsp;&nbsp; mean (sd)    |5.73 &plusmn; 1.14          |
#> |&nbsp;&nbsp; maximum      |58.90                       |
#> |**z**                     |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; minimum      |0.00                        |
#> |&nbsp;&nbsp; median (IQR) |3.53 (2.91, 4.04)           |
#> |&nbsp;&nbsp; mean (sd)    |3.54 &plusmn; 0.71          |
#> |&nbsp;&nbsp; maximum      |31.80                       |
#> |**The Price**             |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp; minimum      |326                         |
#> |&nbsp;&nbsp; median (IQR) |2,401.00 (950.00, 5,324.25) |
#> |&nbsp;&nbsp; mean (sd)    |3,932.80 &plusmn; 3,989.44  |
#> |&nbsp;&nbsp; maximum      |18,823                      |
#> |**A Logical**             |&nbsp;&nbsp;                |
#> |&nbsp;&nbsp;              |27,214 (50)                 |

summary_table(diamonds, by = "cut")
#> 
#> 
#> |                          |Fair (N = 1610)               |Good (N = 4906)               |Very Good (N = 12082)       |Premium (N = 13791)        |Ideal (N = 21551)          |
#> |:-------------------------|:-----------------------------|:-----------------------------|:---------------------------|:--------------------------|:--------------------------|
#> |**carat**                 |&nbsp;&nbsp;                  |&nbsp;&nbsp;                  |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |
#> |&nbsp;&nbsp; minimum      |0.22                          |0.23                          |0.20                        |0.20                       |0.20                       |
#> |&nbsp;&nbsp; median (IQR) |1.00 (0.70, 1.20)             |0.82 (0.50, 1.01)             |0.71 (0.41, 1.02)           |0.86 (0.41, 1.20)          |0.54 (0.35, 1.01)          |
#> |&nbsp;&nbsp; mean (sd)    |1.05 &plusmn; 0.52            |0.85 &plusmn; 0.45            |0.81 &plusmn; 0.46          |0.89 &plusmn; 0.52         |0.70 &plusmn; 0.43         |
#> |&nbsp;&nbsp; maximum      |5.01                          |3.01                          |4.00                        |4.01                       |3.50                       |
#> |**cut**                   |&nbsp;&nbsp;                  |&nbsp;&nbsp;                  |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |
#> |&nbsp;&nbsp; Fair         |1,610 (100)                   |0 (0)                         |0 (0)                       |0 (0)                      |0 (0)                      |
#> |&nbsp;&nbsp; Good         |0 (0)                         |4,906 (100)                   |0 (0)                       |0 (0)                      |0 (0)                      |
#> |&nbsp;&nbsp; Very Good    |0 (0)                         |0 (0)                         |12,082 (100)                |0 (0)                      |0 (0)                      |
#> |&nbsp;&nbsp; Premium      |0 (0)                         |0 (0)                         |0 (0)                       |13,791 (100)               |0 (0)                      |
#> |&nbsp;&nbsp; Ideal        |0 (0)                         |0 (0)                         |0 (0)                       |0 (0)                      |21,551 (100)               |
#> |**color**                 |&nbsp;&nbsp;                  |&nbsp;&nbsp;                  |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |
#> |&nbsp;&nbsp; D            |163 (10)                      |662 (13)                      |1,513 (13)                  |1,603 (12)                 |2,834 (13)                 |
#> |&nbsp;&nbsp; E            |224 (14)                      |933 (19)                      |2,400 (20)                  |2,337 (17)                 |3,903 (18)                 |
#> |&nbsp;&nbsp; F            |312 (19)                      |909 (19)                      |2,164 (18)                  |2,331 (17)                 |3,826 (18)                 |
#> |&nbsp;&nbsp; G            |314 (20)                      |871 (18)                      |2,299 (19)                  |2,924 (21)                 |4,884 (23)                 |
#> |&nbsp;&nbsp; H            |303 (19)                      |702 (14)                      |1,824 (15)                  |2,360 (17)                 |3,115 (14)                 |
#> |&nbsp;&nbsp; I            |175 (11)                      |522 (11)                      |1,204 (10)                  |1,428 (10)                 |2,093 (10)                 |
#> |&nbsp;&nbsp; J            |119 (7)                       |307 (6)                       |678 (6)                     |808 (6)                    |896 (4)                    |
#> |**clarity**               |&nbsp;&nbsp;                  |&nbsp;&nbsp;                  |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |
#> |&nbsp;&nbsp; I1           |210 (13)                      |96 (2)                        |84 (1)                      |205 (1)                    |146 (1)                    |
#> |&nbsp;&nbsp; SI2          |466 (29)                      |1,081 (22)                    |2,100 (17)                  |2,949 (21)                 |2,598 (12)                 |
#> |&nbsp;&nbsp; SI1          |408 (25)                      |1,560 (32)                    |3,240 (27)                  |3,575 (26)                 |4,282 (20)                 |
#> |&nbsp;&nbsp; VS2          |261 (16)                      |978 (20)                      |2,591 (21)                  |3,357 (24)                 |5,071 (24)                 |
#> |&nbsp;&nbsp; VS1          |170 (11)                      |648 (13)                      |1,775 (15)                  |1,989 (14)                 |3,589 (17)                 |
#> |&nbsp;&nbsp; VVS2         |69 (4)                        |286 (6)                       |1,235 (10)                  |870 (6)                    |2,606 (12)                 |
#> |&nbsp;&nbsp; VVS1         |17 (1)                        |186 (4)                       |789 (7)                     |616 (4)                    |2,047 (9)                  |
#> |&nbsp;&nbsp; IF           |9 (1)                         |71 (1)                        |268 (2)                     |230 (2)                    |1,212 (6)                  |
#> |**depth**                 |&nbsp;&nbsp;                  |&nbsp;&nbsp;                  |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |
#> |&nbsp;&nbsp; minimum      |43.00                         |54.30                         |56.80                       |58.00                      |43.00                      |
#> |&nbsp;&nbsp; median (IQR) |65.00 (64.40, 65.90)          |63.40 (61.30, 63.80)          |62.10 (60.90, 62.90)        |61.40 (60.50, 62.20)       |61.80 (61.30, 62.20)       |
#> |&nbsp;&nbsp; mean (sd)    |64.04 &plusmn; 3.64           |62.37 &plusmn; 2.17           |61.82 &plusmn; 1.38         |61.26 &plusmn; 1.16        |61.71 &plusmn; 0.72        |
#> |&nbsp;&nbsp; maximum      |79.00                         |67.00                         |64.90                       |63.00                      |66.70                      |
#> |**table**                 |&nbsp;&nbsp;                  |&nbsp;&nbsp;                  |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |
#> |&nbsp;&nbsp; minimum      |49.00                         |51.00                         |44.00                       |51.00                      |43.00                      |
#> |&nbsp;&nbsp; median (IQR) |58.00 (56.00, 61.00)          |58.00 (56.00, 61.00)          |58.00 (56.00, 59.00)        |59.00 (58.00, 60.00)       |56.00 (55.00, 57.00)       |
#> |&nbsp;&nbsp; mean (sd)    |59.05 &plusmn; 3.95           |58.69 &plusmn; 2.85           |57.96 &plusmn; 2.12         |58.75 &plusmn; 1.48        |55.95 &plusmn; 1.25        |
#> |&nbsp;&nbsp; maximum      |95.00                         |66.00                         |66.00                       |62.00                      |63.00                      |
#> |**price**                 |&nbsp;&nbsp;                  |&nbsp;&nbsp;                  |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |
#> |&nbsp;&nbsp; minimum      |337                           |327                           |336                         |326                        |326                        |
#> |&nbsp;&nbsp; median (IQR) |3,282.00 (2,050.25, 5,205.50) |3,050.50 (1,145.00, 5,028.00) |2,648.00 (912.00, 5,372.75) |3,185 (1,046.00, 6,296.00) |1,810 (878.00, 4,678.50)   |
#> |&nbsp;&nbsp; mean (sd)    |4,358.76 &plusmn; 3,560.39    |3,928.86 &plusmn; 3,681.59    |3,981.76 &plusmn; 3,935.86  |4,584.26 &plusmn; 4,349.20 |3,457.54 &plusmn; 3,808.40 |
#> |&nbsp;&nbsp; maximum      |18,574                        |18,788                        |18,818                      |18,823                     |18,806                     |
#> |**x**                     |&nbsp;&nbsp;                  |&nbsp;&nbsp;                  |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |
#> |&nbsp;&nbsp; minimum      |0.00                          |0.00                          |0.00                        |0.00                       |0.00                       |
#> |&nbsp;&nbsp; median (IQR) |6.17 (5.63, 6.70)             |5.98 (5.02, 6.42)             |5.74 (4.75, 6.47)           |6.11 (4.80, 6.80)          |5.25 (4.54, 6.44)          |
#> |&nbsp;&nbsp; mean (sd)    |6.25 &plusmn; 0.96            |5.84 &plusmn; 1.06            |5.74 &plusmn; 1.10          |5.97 &plusmn; 1.19         |5.51 &plusmn; 1.06         |
#> |&nbsp;&nbsp; maximum      |10.74                         |9.44                          |10.01                       |10.14                      |9.65                       |
#> |**y**                     |&nbsp;&nbsp;                  |&nbsp;&nbsp;                  |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |
#> |&nbsp;&nbsp; minimum      |0.00                          |0.00                          |0.00                        |0.00                       |0.00                       |
#> |&nbsp;&nbsp; median (IQR) |6.10 (5.57, 6.64)             |5.99 (5.02, 6.44)             |5.77 (4.77, 6.51)           |6.06 (4.79, 6.76)          |5.26 (4.55, 6.45)          |
#> |&nbsp;&nbsp; mean (sd)    |6.18 &plusmn; 0.96            |5.85 &plusmn; 1.05            |5.77 &plusmn; 1.10          |5.94 &plusmn; 1.26         |5.52 &plusmn; 1.07         |
#> |&nbsp;&nbsp; maximum      |10.54                         |9.38                          |9.94                        |58.90                      |31.80                      |
#> |**z**                     |&nbsp;&nbsp;                  |&nbsp;&nbsp;                  |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |
#> |&nbsp;&nbsp; minimum      |0.00                          |0.00                          |0.00                        |0.00                       |0.00                       |
#> |&nbsp;&nbsp; median (IQR) |3.97 (3.61, 4.28)             |3.70 (3.07, 4.03)             |3.56 (2.95, 4.02)           |3.72 (2.94, 4.16)          |3.23 (2.80, 3.98)          |
#> |&nbsp;&nbsp; mean (sd)    |3.98 &plusmn; 0.65            |3.64 &plusmn; 0.65            |3.56 &plusmn; 0.73          |3.65 &plusmn; 0.73         |3.40 &plusmn; 0.66         |
#> |&nbsp;&nbsp; maximum      |6.98                          |5.79                          |31.80                       |8.06                       |6.03                       |
#> |**The Price**             |&nbsp;&nbsp;                  |&nbsp;&nbsp;                  |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |
#> |&nbsp;&nbsp; minimum      |337                           |327                           |336                         |326                        |326                        |
#> |&nbsp;&nbsp; median (IQR) |3,282.00 (2,050.25, 5,205.50) |3,050.50 (1,145.00, 5,028.00) |2,648.00 (912.00, 5,372.75) |3,185 (1,046.00, 6,296.00) |1,810 (878.00, 4,678.50)   |
#> |&nbsp;&nbsp; mean (sd)    |4,358.76 &plusmn; 3,560.39    |3,928.86 &plusmn; 3,681.59    |3,981.76 &plusmn; 3,935.86  |4,584.26 &plusmn; 4,349.20 |3,457.54 &plusmn; 3,808.40 |
#> |&nbsp;&nbsp; maximum      |18,574                        |18,788                        |18,818                      |18,823                     |18,806                     |
#> |**A Logical**             |&nbsp;&nbsp;                  |&nbsp;&nbsp;                  |&nbsp;&nbsp;                |&nbsp;&nbsp;               |&nbsp;&nbsp;               |
#> |&nbsp;&nbsp;              |785 (49)                      |2,490 (51)                    |6,102 (51)                  |6,981 (51)                 |10,856 (50)                |

summary_table(diamonds,
              summaries =
              list("My Summary of Price" =
                   list("min price" = ~ min(price),
                        "IQR"       = ~ stats::IQR(price))),
              by = "cut")
#> 
#> 
#> |                        |Fair (N = 1610) |Good (N = 4906) |Very Good (N = 12082) |Premium (N = 13791) |Ideal (N = 21551) |
#> |:-----------------------|:---------------|:---------------|:---------------------|:-------------------|:-----------------|
#> |**My Summary of Price** |&nbsp;&nbsp;    |&nbsp;&nbsp;    |&nbsp;&nbsp;          |&nbsp;&nbsp;        |&nbsp;&nbsp;      |
#> |&nbsp;&nbsp; min price  |337             |327             |336                   |326                 |326               |
#> |&nbsp;&nbsp; IQR        |3155.25         |3883            |4460.75               |5250                |3800.5            |

################################################################################
# Data sets with missing values
temp <- mtcars
temp$cyl[5] <- NA
temp$am[c(1, 5, 10)] <- NA
temp$am <- factor(temp$am, levels = 0:1, labels = c("Automatic", "Manual"))
temp$vs <- as.logical(temp$vs)
temp$vs[c(2, 6)] <- NA
qsummary(temp[, c("cyl", "am", "vs")])
#> $cyl
#> $cyl$minimum
#> ~qwraps2::frmt(min(na.omit(cyl)))
#> <environment: 0x55de51497820>
#> 
#> $cyl$`median (IQR)`
#> ~qwraps2::median_iqr(na.omit(cyl))
#> <environment: 0x55de51497820>
#> 
#> $cyl$`mean (sd)`
#> ~qwraps2::mean_sd(na.omit(cyl))
#> <environment: 0x55de51497820>
#> 
#> $cyl$maximum
#> ~qwraps2::frmt(max(na.omit(cyl)))
#> <environment: 0x55de51497820>
#> 
#> $cyl$`Unknown/Missing`
#> ~qwraps2::n_perc(is.na(cyl))
#> <environment: 0x55de51497820>
#> 
#> 
#> $am
#> $am$Automatic
#> ~qwraps2::n_perc(na.omit(am) == "Automatic", digits = 0, show_symbol = FALSE)
#> <environment: 0x55de51497820>
#> 
#> $am$Manual
#> ~qwraps2::n_perc(na.omit(am) == "Manual", digits = 0, show_symbol = FALSE)
#> <environment: 0x55de51497820>
#> 
#> $am$`Unknown/Missing`
#> ~qwraps2::n_perc(is.na(am))
#> <environment: 0x55de51497820>
#> 
#> 
#> $vs
#> $vs[[1]]
#> ~qwraps2::n_perc(na.omit(vs), digits = 0, show_symbol = FALSE)
#> <environment: 0x55de51497820>
#> 
#> $vs$`Unknown/Missing`
#> ~qwraps2::n_perc(is.na(vs))
#> <environment: 0x55de51497820>
#> 
#> 
summary_table(temp[, c("cyl", "am", "vs")])
#> 
#> 
#> |                             |temp[, c("cyl", "am", "vs")] (N = 32) |
#> |:----------------------------|:-------------------------------------|
#> |**cyl**                      |&nbsp;&nbsp;                          |
#> |&nbsp;&nbsp; minimum         |4.00                                  |
#> |&nbsp;&nbsp; median (IQR)    |6.00 (4.00, 8.00)                     |
#> |&nbsp;&nbsp; mean (sd)       |6.13 &plusmn; 1.78                    |
#> |&nbsp;&nbsp; maximum         |8.00                                  |
#> |&nbsp;&nbsp; Unknown/Missing |1 (3.12%)                             |
#> |**am**                       |&nbsp;&nbsp;                          |
#> |&nbsp;&nbsp; Automatic       |17 (59)                               |
#> |&nbsp;&nbsp; Manual          |12 (41)                               |
#> |&nbsp;&nbsp; Unknown/Missing |3 (9.38%)                             |
#> |**vs**                       |&nbsp;&nbsp;                          |
#> |&nbsp;&nbsp;                 |13 (43)                               |
#> |&nbsp;&nbsp; Unknown/Missing |2 (6.25%)                             |

################################################################################
# Group by Multiple Variables
temp <- mtcars
temp$trans <- factor(temp$am, 0:1, c("Manual", "Auto"))
temp$engine <- factor(temp$vs, 0:1, c("V-Shaped", "Straight"))
summary_table(temp, our_summaries, by = c("trans", "engine"))
#> 
#> 
#> |                              |Manual.V-Shaped (N = 12) |Auto.V-Shaped (N = 6) |Manual.Straight (N = 7) |Auto.Straight (N = 7) |
#> |:-----------------------------|:------------------------|:---------------------|:-----------------------|:---------------------|
#> |**Miles Per Gallon**          |&nbsp;&nbsp;             |&nbsp;&nbsp;          |&nbsp;&nbsp;            |&nbsp;&nbsp;          |
#> |&nbsp;&nbsp; min              |10.4                     |15                    |17.8                    |21.4                  |
#> |&nbsp;&nbsp; mean             |15.05                    |19.75                 |20.7428571428571        |28.3714285714286      |
#> |&nbsp;&nbsp; mean &plusmn; sd |15.05 &plusmn; 2.77      |19.75 &plusmn; 4.01   |20.74 &plusmn; 2.47     |28.37 &plusmn; 4.76   |
#> |&nbsp;&nbsp; max              |19.2                     |26                    |24.4                    |33.9                  |
#> |**Weight**                    |&nbsp;&nbsp;             |&nbsp;&nbsp;          |&nbsp;&nbsp;            |&nbsp;&nbsp;          |
#> |&nbsp;&nbsp; median           |3.81                     |2.8225                |3.215                   |1.935                 |
#> |**Cylinders**                 |&nbsp;&nbsp;             |&nbsp;&nbsp;          |&nbsp;&nbsp;            |&nbsp;&nbsp;          |
#> |&nbsp;&nbsp; 4 cyl: n (%)     |0 (0)                    |1 (17)                |3 (43)                  |7 (100)               |
#> |&nbsp;&nbsp; 6 cyl: n (%)     |0 (0)                    |3 (50)                |4 (57)                  |0 (0)                 |
#> |&nbsp;&nbsp; 8 cyl: n (%)     |12 (100)                 |2 (33)                |0 (0)                   |0 (0)                 |

################################################################################
# binding tables together.  The original design and expected use of
# summary_table did not require a rbind, as all rows are defined in the
# summaries argument.  That said, here are examples of using cbind and rbind to
# build several different tables.
our_summary1 <-
  list("Miles Per Gallon" =
       list("min" = ~ min(mpg),
            "max" = ~ max(mpg),
            "mean (sd)" = ~ qwraps2::mean_sd(mpg)),
       "Displacement" =
       list("min" = ~ min(disp),
            "max" = ~ max(disp),
            "mean (sd)" = ~ qwraps2::mean_sd(disp)))

our_summary2 <-
  list(
       "Weight (1000 lbs)" =
       list("min" = ~ min(wt),
            "max" = ~ max(wt),
            "mean (sd)" = ~ qwraps2::mean_sd(wt)),
       "Forward Gears" =
       list("Three" = ~ qwraps2::n_perc0(gear == 3),
            "Four"  = ~ qwraps2::n_perc0(gear == 4),
            "Five"  = ~ qwraps2::n_perc0(gear == 5))
       )

tab1 <- summary_table(mtcars, our_summary1)
tab2 <- summary_table(dplyr::group_by(mtcars, am), our_summary1)
#> Warning: grouped_df detected. Setting `by` argument to
#>   c('am')
tab3 <- summary_table(dplyr::group_by(mtcars, vs), our_summary1)
#> Warning: grouped_df detected. Setting `by` argument to
#>   c('vs')

tab4 <- summary_table(mtcars, our_summary2)
tab5 <- summary_table(dplyr::group_by(mtcars, am), our_summary2)
#> Warning: grouped_df detected. Setting `by` argument to
#>   c('am')
tab6 <- summary_table(dplyr::group_by(mtcars, vs), our_summary2)
#> Warning: grouped_df detected. Setting `by` argument to
#>   c('vs')

cbind(tab1, tab2, tab3)
#> 
#> 
#> |                       |mtcars (N = 32)        |0 (N = 19)             |1 (N = 13)            |0 (N = 18)             |1 (N = 14)            |
#> |:----------------------|:----------------------|:----------------------|:---------------------|:----------------------|:---------------------|
#> |**Miles Per Gallon**   |&nbsp;&nbsp;           |&nbsp;&nbsp;           |&nbsp;&nbsp;          |&nbsp;&nbsp;           |&nbsp;&nbsp;          |
#> |&nbsp;&nbsp; min       |10.4                   |10.4                   |15                    |10.4                   |17.8                  |
#> |&nbsp;&nbsp; max       |33.9                   |24.4                   |33.9                  |26                     |33.9                  |
#> |&nbsp;&nbsp; mean (sd) |20.09 &plusmn; 6.03    |17.15 &plusmn; 3.83    |24.39 &plusmn; 6.17   |16.62 &plusmn; 3.86    |24.56 &plusmn; 5.38   |
#> |**Displacement**       |&nbsp;&nbsp;           |&nbsp;&nbsp;           |&nbsp;&nbsp;          |&nbsp;&nbsp;           |&nbsp;&nbsp;          |
#> |&nbsp;&nbsp; min       |71.1                   |120.1                  |71.1                  |120.3                  |71.1                  |
#> |&nbsp;&nbsp; max       |472                    |472                    |351                   |472                    |258                   |
#> |&nbsp;&nbsp; mean (sd) |230.72 &plusmn; 123.94 |290.38 &plusmn; 110.17 |143.53 &plusmn; 87.20 |307.15 &plusmn; 106.77 |132.46 &plusmn; 56.89 |
cbind(tab4, tab5, tab6)
#> 
#> 
#> |                       |mtcars (N = 32)    |0 (N = 19)         |1 (N = 13)         |0 (N = 18)         |1 (N = 14)         |
#> |:----------------------|:------------------|:------------------|:------------------|:------------------|:------------------|
#> |**Weight (1000 lbs)**  |&nbsp;&nbsp;       |&nbsp;&nbsp;       |&nbsp;&nbsp;       |&nbsp;&nbsp;       |&nbsp;&nbsp;       |
#> |&nbsp;&nbsp; min       |1.513              |2.465              |1.513              |2.14               |1.513              |
#> |&nbsp;&nbsp; max       |5.424              |5.424              |3.57               |5.424              |3.46               |
#> |&nbsp;&nbsp; mean (sd) |3.22 &plusmn; 0.98 |3.77 &plusmn; 0.78 |2.41 &plusmn; 0.62 |3.69 &plusmn; 0.90 |2.61 &plusmn; 0.72 |
#> |**Forward Gears**      |&nbsp;&nbsp;       |&nbsp;&nbsp;       |&nbsp;&nbsp;       |&nbsp;&nbsp;       |&nbsp;&nbsp;       |
#> |&nbsp;&nbsp; Three     |15 (47)            |15 (79)            |0 (0)              |12 (67)            |3 (21)             |
#> |&nbsp;&nbsp; Four      |12 (38)            |4 (21)             |8 (62)             |2 (11)             |10 (71)            |
#> |&nbsp;&nbsp; Five      |5 (16)             |0 (0)              |5 (38)             |4 (22)             |1 (7)              |

# row bind is possible, but it is recommended to extend the summary instead.
rbind(tab1, tab4)
#> 
#> 
#> |                       |mtcars (N = 32)        |
#> |:----------------------|:----------------------|
#> |**Miles Per Gallon**   |&nbsp;&nbsp;           |
#> |&nbsp;&nbsp; min       |10.4                   |
#> |&nbsp;&nbsp; max       |33.9                   |
#> |&nbsp;&nbsp; mean (sd) |20.09 &plusmn; 6.03    |
#> |**Displacement**       |&nbsp;&nbsp;           |
#> |&nbsp;&nbsp; min       |71.1                   |
#> |&nbsp;&nbsp; max       |472                    |
#> |&nbsp;&nbsp; mean (sd) |230.72 &plusmn; 123.94 |
#> |**Weight (1000 lbs)**  |&nbsp;&nbsp;           |
#> |&nbsp;&nbsp; min       |1.513                  |
#> |&nbsp;&nbsp; max       |5.424                  |
#> |&nbsp;&nbsp; mean (sd) |3.22 &plusmn; 0.98     |
#> |**Forward Gears**      |&nbsp;&nbsp;           |
#> |&nbsp;&nbsp; Three     |15 (47)                |
#> |&nbsp;&nbsp; Four      |12 (38)                |
#> |&nbsp;&nbsp; Five      |5 (16)                 |
summary_table(mtcars, summaries = c(our_summary1, our_summary2))
#> 
#> 
#> |                       |mtcars (N = 32)        |
#> |:----------------------|:----------------------|
#> |**Miles Per Gallon**   |&nbsp;&nbsp;           |
#> |&nbsp;&nbsp; min       |10.4                   |
#> |&nbsp;&nbsp; max       |33.9                   |
#> |&nbsp;&nbsp; mean (sd) |20.09 &plusmn; 6.03    |
#> |**Displacement**       |&nbsp;&nbsp;           |
#> |&nbsp;&nbsp; min       |71.1                   |
#> |&nbsp;&nbsp; max       |472                    |
#> |&nbsp;&nbsp; mean (sd) |230.72 &plusmn; 123.94 |
#> |**Weight (1000 lbs)**  |&nbsp;&nbsp;           |
#> |&nbsp;&nbsp; min       |1.513                  |
#> |&nbsp;&nbsp; max       |5.424                  |
#> |&nbsp;&nbsp; mean (sd) |3.22 &plusmn; 0.98     |
#> |**Forward Gears**      |&nbsp;&nbsp;           |
#> |&nbsp;&nbsp; Three     |15 (47)                |
#> |&nbsp;&nbsp; Four      |12 (38)                |
#> |&nbsp;&nbsp; Five      |5 (16)                 |

if (FALSE) { # \dontrun{
  cbind(tab1, tab4) # error because rows are not the same
  rbind(tab1, tab2) # error because columns are not the same
} # }

################################################################################
# reset the original markup option that was used before this example was
# evaluated.
options(qwraps2_markup = orig_opt)

# Detailed examples in the vignette
# vignette("qwraps2-summary-table", package = "qwraps2")

```
