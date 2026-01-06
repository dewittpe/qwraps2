# qwraps2: Summary Table

``` r
set.seed(42)
library(qwraps2)
options(qwraps2_markup = "markdown")
```

The `summary_table` method appears to be the most popular and widely
used feature of the
*[qwraps2](https://cran.r-project.org/package=qwraps2)* package. As
such, this vignette is provided to give as much detail on the use of the
method, and the underlying `qable` method for quickly building well
formatted summary tables.

## qable

`qable` builds a formatted character matrix from inputs and then renders
a table via knitr::kable. The primary objective of this function is to
allow for easy construction of row groups.

### kable vs qable

For a simple example we will use the following data set with a grouping
variable, subject id, and two variables, V2, and V3. For simplicity, we
will order the data by group and id as well.

``` r
d <- data.frame(
       group = sample(size = 15, paste0("grp", 1:5), replace = TRUE)
     , id = sample(size = 15, x = LETTERS)
     , V2 = rnorm(15)
     , V3 = rep(c(1, 2, NA), times = 5)
     )
d <- d[order(d$group, d$id), ]
```

Making a simple table via kable:

``` r
knitr::kable(d, row.names = FALSE)
```

| group | id  |         V2 |  V3 |
|:------|:----|-----------:|----:|
| grp1  | D   |  0.3584021 |   1 |
| grp1  | H   | -0.9491808 |  NA |
| grp1  | I   |  1.7232308 |  NA |
| grp1  | J   |  2.1157556 |   2 |
| grp1  | O   | -0.1616986 |   1 |
| grp2  | B   |  0.6707038 |   2 |
| grp2  | E   |  0.3024309 |   2 |
| grp2  | K   | -0.7045514 |  NA |
| grp2  | R   |  0.9469132 |   2 |
| grp2  | V   |  0.7881406 |   1 |
| grp4  | M   | -0.3941145 |  NA |
| grp4  | P   | -0.8798365 |   1 |
| grp4  | Y   |  0.0361357 |   1 |
| grp5  | A   |  0.1674409 |  NA |
| grp5  | C   |  1.9355718 |   2 |

The group column is great for data analysis, but is not the best for
human readability. This is where `qable` can be useful. Start by
building a named numeric column with the name being the row group name
and the value the number of rows. For the *ordered* data this is a
simple call to table:

``` r
c(table(d$group))
## grp1 grp2 grp4 grp5 
##    5    5    3    2
```

If we pass that named vector to `qable` as the rgroup and with specify
the id column as the row names we have the same information but in
format that is better for humans:

``` r
qable(  x = d[, c("V2", "V3")]
      , rgroup = c(table(d$group)) # row group
      , rnames = d$id              # row names
)
```

|          | V2                 | V3  |
|:---------|:-------------------|:----|
| **grp1** |                    |     |
|    D     | 0.358402056802064  | 1   |
|    H     | -0.949180809687611 | NA  |
|    I     | 1.72323079854894   | NA  |
|    J     | 2.11575561323695   | 2   |
|    O     | -0.161698647607024 | 1   |
| **grp2** |                    |     |
|    B     | 0.67070382675052   | 2   |
|    E     | 0.3024309248682    | 2   |
|    K     | -0.704551365955043 | NA  |
|    R     | 0.946913174943256  | 2   |
|    V     | 0.788140622823556  | 1   |
| **grp4** |                    |     |
|    M     | -0.394114506412192 | NA  |
|    P     | -0.879836528531105 | 1   |
|    Y     | 0.0361357384849679 | 1   |
| **grp5** |                    |     |
|    A     | 0.167440904355584  | NA  |
|    C     | 1.93557176599585   | 2   |

The return object from `qable` is a character matrix. Also, when a
data.frame is passed to `qable` it is coerced to a matrix before
anything else, as such, any formatting of numeric values or other
strings should be done before calling `qable`.

To pass arguments to knitr::kable do so via the `kable_args` argument.

### Example: Regression Model Summary Table

We will build a summary table for a regression model with row groups for
conceptually similar predictors.

``` r
model <-
  glm(spam ~
        word_freq_your + word_freq_conference + word_freq_business +
        char_freq_semicolon + char_freq_exclamation_point +
        capital_run_length_total + capital_run_length_longest
    , data = spambase
    , family = binomial()
  )
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

model_summary <-
  data.frame(
    parameter = names(coef(model))
  , odd_ratio = frmt(exp(coef(model)), digits = 3)
  , lcl       = frmt(exp(coef(model) + qnorm(0.025) * sqrt(diag(vcov(model)))), digits = 3)
  , ucl       = frmt(exp(coef(model) + qnorm(0.975) * sqrt(diag(vcov(model)))), digits = 3)
  , pval      = frmtp(summary(model)$coef[, 4])
  )

qable(model_summary[-1, c('odd_ratio', 'lcl', 'ucl', 'pval')]
      , rtitle = "Parameter"
      , rgroup = c("Word Frequency" = 3, "Character Frequency" = 2, "Capital Run Length" = 2)
      , rnames = c("Your", "Conference", "Business", ";", "!", "Total", "Longest")
      , kable_args = list(align = "lrrrr", caption = "Regression Model Summary")
      , cnames = c("Odds Ratio", "Lower Conf. Limit", "Upper Conf. Limit", "P-value")
      )
```

| Parameter               | Odds Ratio | Lower Conf. Limit | Upper Conf. Limit |       P-value |
|:------------------------|-----------:|------------------:|------------------:|--------------:|
| **Word Frequency**      |            |                   |                   |               |
|    Your                 |      1.802 |             1.678 |             1.934 | *P* \< 0.0001 |
|    Conference           |      0.001 |             0.000 |             0.012 | *P* \< 0.0001 |
|    Business             |      3.573 |             2.721 |             4.693 | *P* \< 0.0001 |
| **Character Frequency** |            |                   |                   |               |
|    ;                    |      0.325 |             0.136 |             0.779 |  *P* = 0.0117 |
|    !                    |      4.148 |             3.336 |             5.158 | *P* \< 0.0001 |
| **Capital Run Length**  |            |                   |                   |               |
|    Total                |      1.000 |             1.000 |             1.001 | *P* \< 0.0001 |
|    Longest              |      1.017 |             1.014 |             1.019 | *P* \< 0.0001 |

Regression Model Summary

## summary_table

`summary_table` was developed with the primary objective to build well
formatted and easy to read data summary tables. Conceptually, the
construction of these tables start by building a “list-of-lists” of
summaries and then generating these summaries for specific groupings of
the data set.

### Defining a Summary

We will use the `mtcars2` data set for these examples. We’ll start with
something very simple and build up to something bigger.

Let’s report the min, max, and mean (sd) for continuous variables and n
(%) for categorical variables. We will report mpg, displacement (disp),
wt (weight), and gear overall and by number of cylinders and
transmission type.

The use of the `summary_table` use to define a summary, that is, a
list-of-lists of formulas for summarizing the data.frame.

The inner lists are named formulae defining the wanted summary. The
names are important, as they are used to label row groups and row names
in the table.

``` r
our_summary1 <-
  list("Miles Per Gallon" =
       list("min"       = ~ min(mpg),
            "max"       = ~ max(mpg),
            "mean (sd)" = ~ qwraps2::mean_sd(mpg)),
       "Displacement" =
       list("min"       = ~ min(disp),
            "median"    = ~ median(disp),
            "max"       = ~ max(disp),
            "mean (sd)" = ~ qwraps2::mean_sd(disp)),
       "Weight (1000 lbs)" =
       list("min"       = ~ min(wt),
            "max"       = ~ max(wt),
            "mean (sd)" = ~ qwraps2::mean_sd(wt)),
       "Forward Gears" =
       list("Three" = ~ qwraps2::n_perc0(gear == 3),
            "Four"  = ~ qwraps2::n_perc0(gear == 4),
            "Five"  = ~ qwraps2::n_perc0(gear == 5))
       )
```

Building the table is done with a call to `summary_table` and rendered
in Table @ref(tab:mtcars_whole).

``` r
whole <-
  summary_table(
    x = mtcars2
  , summaries = our_summary1
  , qable_args = list(kable_args = list(caption = "mtcars2 data summary"))
  )
whole
```

|                       | mtcars2 (N = 32) |
|:----------------------|:-----------------|
| **Miles Per Gallon**  |                  |
|    min                | 10.4             |
|    max                | 33.9             |
|    mean (sd)          | 20.09 ± 6.03     |
| **Displacement**      |                  |
|    min                | 71.1             |
|    median             | 196.3            |
|    max                | 472              |
|    mean (sd)          | 230.72 ± 123.94  |
| **Weight (1000 lbs)** |                  |
|    min                | 1.513            |
|    max                | 5.424            |
|    mean (sd)          | 3.22 ± 0.98      |
| **Forward Gears**     |                  |
|    Three              | 15 (47)          |
|    Four               | 12 (38)          |
|    Five               | 5 (16)           |

mtcars2 data summary

### Summarize by

Use the `by` argument to specify a grouping variable and generate the
same summary as above but for subsets of the data. When the `by` column
is a factor, the columns will be in the order of the levels of the
factor. In comparison, the column order is alphabetical if the variable
is just a character.

``` r
by_cylf <-
  summary_table(
    x = mtcars2
  , summaries = our_summary1
  , by = c("cyl_factor")
  , qable_args = list(rtitle = "Summary Statistics"
                      , kable_args = list(caption = "mtcars2 data summary by cyl_factor"))
  )
by_cylf
```

| Summary Statistics    | 6 cylinders (N = 7) | 4 cylinders (N = 11) | 8 cylinders (N = 14) |
|:----------------------|:--------------------|:---------------------|:---------------------|
| **Miles Per Gallon**  |                     |                      |                      |
|    min                | 17.8                | 21.4                 | 10.4                 |
|    max                | 21.4                | 33.9                 | 19.2                 |
|    mean (sd)          | 19.74 ± 1.45        | 26.66 ± 4.51         | 15.10 ± 2.56         |
| **Displacement**      |                     |                      |                      |
|    min                | 145                 | 71.1                 | 275.8                |
|    median             | 167.6               | 108                  | 350.5                |
|    max                | 258                 | 146.7                | 472                  |
|    mean (sd)          | 183.31 ± 41.56      | 105.14 ± 26.87       | 353.10 ± 67.77       |
| **Weight (1000 lbs)** |                     |                      |                      |
|    min                | 2.62                | 1.513                | 3.17                 |
|    max                | 3.46                | 3.19                 | 5.424                |
|    mean (sd)          | 3.12 ± 0.36         | 2.29 ± 0.57          | 4.00 ± 0.76          |
| **Forward Gears**     |                     |                      |                      |
|    Three              | 2 (29)              | 1 (9)                | 12 (86)              |
|    Four               | 4 (57)              | 8 (73)               | 0 (0)                |
|    Five               | 1 (14)              | 2 (18)               | 2 (14)               |

mtcars2 data summary by cyl_factor

``` r
by_cylc <-
  summary_table(
    x = mtcars2
  , summaries = our_summary1
  , by = c("cyl_character")
  , qable_args = list(rtitle = "Summary Statistics"
                      , kable_args = list(caption = "mtcars2 data summary by cyl_character"))
  )
by_cylc
```

| Summary Statistics    | 4 cylinders (N = 11) | 6 cylinders (N = 7) | 8 cylinders (N = 14) |
|:----------------------|:---------------------|:--------------------|:---------------------|
| **Miles Per Gallon**  |                      |                     |                      |
|    min                | 21.4                 | 17.8                | 10.4                 |
|    max                | 33.9                 | 21.4                | 19.2                 |
|    mean (sd)          | 26.66 ± 4.51         | 19.74 ± 1.45        | 15.10 ± 2.56         |
| **Displacement**      |                      |                     |                      |
|    min                | 71.1                 | 145                 | 275.8                |
|    median             | 108                  | 167.6               | 350.5                |
|    max                | 146.7                | 258                 | 472                  |
|    mean (sd)          | 105.14 ± 26.87       | 183.31 ± 41.56      | 353.10 ± 67.77       |
| **Weight (1000 lbs)** |                      |                     |                      |
|    min                | 1.513                | 2.62                | 3.17                 |
|    max                | 3.19                 | 3.46                | 5.424                |
|    mean (sd)          | 2.29 ± 0.57          | 3.12 ± 0.36         | 4.00 ± 0.76          |
| **Forward Gears**     |                      |                     |                      |
|    Three              | 1 (9)                | 2 (29)              | 12 (86)              |
|    Four               | 8 (73)               | 4 (57)              | 0 (0)                |
|    Five               | 2 (18)               | 1 (14)              | 2 (14)               |

mtcars2 data summary by cyl_character

You are also able to generate summaries by multiple columns. For
example, Table @ref(tab:mtcars2_by_cyl_transmission) reports the summary
by the combination of the number of cylinders and the type of
transmission.

``` r
by_cyl_am <-
  summary_table(
    x = mtcars2
  , summaries = our_summary1
  , by = c("cyl_factor", "transmission")
  )
by_cyl_am
```

|                       | 6 cylinders.Automatic (N = 4) | 4 cylinders.Automatic (N = 3) | 8 cylinders.Automatic (N = 12) | 6 cylinders.Manual (N = 3) | 4 cylinders.Manual (N = 8) | 8 cylinders.Manual (N = 2) |
|:----------------------|:------------------------------|:------------------------------|:-------------------------------|:---------------------------|:---------------------------|:---------------------------|
| **Miles Per Gallon**  |                               |                               |                                |                            |                            |                            |
|    min                | 17.8                          | 21.5                          | 10.4                           | 19.7                       | 21.4                       | 15                         |
|    max                | 21.4                          | 24.4                          | 19.2                           | 21                         | 33.9                       | 15.8                       |
|    mean (sd)          | 19.12 ± 1.63                  | 22.90 ± 1.45                  | 15.05 ± 2.77                   | 20.57 ± 0.75               | 28.07 ± 4.48               | 15.40 ± 0.57               |
| **Displacement**      |                               |                               |                                |                            |                            |                            |
|    min                | 167.6                         | 120.1                         | 275.8                          | 145                        | 71.1                       | 301                        |
|    median             | 196.3                         | 140.8                         | 355                            | 160                        | 87.05                      | 326                        |
|    max                | 258                           | 146.7                         | 472                            | 160                        | 121                        | 351                        |
|    mean (sd)          | 204.55 ± 44.74                | 135.87 ± 13.97                | 357.62 ± 71.82                 | 155.00 ± 8.66              | 93.61 ± 20.48              | 326.00 ± 35.36             |
| **Weight (1000 lbs)** |                               |                               |                                |                            |                            |                            |
|    min                | 3.215                         | 2.465                         | 3.435                          | 2.62                       | 1.513                      | 3.17                       |
|    max                | 3.46                          | 3.19                          | 5.424                          | 2.875                      | 2.78                       | 3.57                       |
|    mean (sd)          | 3.39 ± 0.12                   | 2.94 ± 0.41                   | 4.10 ± 0.77                    | 2.75 ± 0.13                | 2.04 ± 0.41                | 3.37 ± 0.28                |
| **Forward Gears**     |                               |                               |                                |                            |                            |                            |
|    Three              | 2 (50)                        | 1 (33)                        | 12 (100)                       | 0 (0)                      | 0 (0)                      | 0 (0)                      |
|    Four               | 2 (50)                        | 2 (67)                        | 0 (0)                          | 2 (67)                     | 6 (75)                     | 0 (0)                      |
|    Five               | 0 (0)                         | 0 (0)                         | 0 (0)                          | 1 (33)                     | 2 (25)                     | 2 (100)                    |

### cbind summary_table

It is common that I will want to have a summary table with the first
column reporting for the whole data sets and the additional columns for
subsets of the data set. The returned objects from `summary_table` can
be joined together via `cbind` assuming that the row groupings
(summaries) are the same.

Note: the `kable_args` of the first item passed to `cbind` will be
assigned to the resulting object (Table @ref(tab:mtcars2_cbind)).
However, there is an easy way to modify the qable_args and kable_args
via the print method.

``` r
both <- cbind(whole, by_cylf)
both
```

|                       | mtcars2 (N = 32) | 6 cylinders (N = 7) | 4 cylinders (N = 11) | 8 cylinders (N = 14) |
|:----------------------|:-----------------|:--------------------|:---------------------|:---------------------|
| **Miles Per Gallon**  |                  |                     |                      |                      |
|    min                | 10.4             | 17.8                | 21.4                 | 10.4                 |
|    max                | 33.9             | 21.4                | 33.9                 | 19.2                 |
|    mean (sd)          | 20.09 ± 6.03     | 19.74 ± 1.45        | 26.66 ± 4.51         | 15.10 ± 2.56         |
| **Displacement**      |                  |                     |                      |                      |
|    min                | 71.1             | 145                 | 71.1                 | 275.8                |
|    median             | 196.3            | 167.6               | 108                  | 350.5                |
|    max                | 472              | 258                 | 146.7                | 472                  |
|    mean (sd)          | 230.72 ± 123.94  | 183.31 ± 41.56      | 105.14 ± 26.87       | 353.10 ± 67.77       |
| **Weight (1000 lbs)** |                  |                     |                      |                      |
|    min                | 1.513            | 2.62                | 1.513                | 3.17                 |
|    max                | 5.424            | 3.46                | 3.19                 | 5.424                |
|    mean (sd)          | 3.22 ± 0.98      | 3.12 ± 0.36         | 2.29 ± 0.57          | 4.00 ± 0.76          |
| **Forward Gears**     |                  |                     |                      |                      |
|    Three              | 15 (47)          | 2 (29)              | 1 (9)                | 12 (86)              |
|    Four               | 12 (38)          | 4 (57)              | 8 (73)               | 0 (0)                |
|    Five               | 5 (16)           | 1 (14)              | 2 (18)               | 2 (14)               |

mtcars2 data summary

If you want to update how a summary table is printed, you can do so by
calling the print method explicitly while passing a new set of
`qable_args`, see Table @ref(tab:updated_both).

``` r

print(both,
      qable_args = list(
        rtitle = "ROW-TITLE",
        cnames = c("Col 0", "Col 1", "Col 2", "Col 3"),
        kable_args = list(
          align = "lcrcr",
          caption = "mtcars2 data summary - new caption"
        )
      ))
```

| ROW-TITLE             |      Col 0      |          Col 1 |     Col 2      |          Col 3 |
|:----------------------|:---------------:|---------------:|:--------------:|---------------:|
| **Miles Per Gallon**  |                 |                |                |                |
|    min                |      10.4       |           17.8 |      21.4      |           10.4 |
|    max                |      33.9       |           21.4 |      33.9      |           19.2 |
|    mean (sd)          |  20.09 ± 6.03   |   19.74 ± 1.45 |  26.66 ± 4.51  |   15.10 ± 2.56 |
| **Displacement**      |                 |                |                |                |
|    min                |      71.1       |            145 |      71.1      |          275.8 |
|    median             |      196.3      |          167.6 |      108       |          350.5 |
|    max                |       472       |            258 |     146.7      |            472 |
|    mean (sd)          | 230.72 ± 123.94 | 183.31 ± 41.56 | 105.14 ± 26.87 | 353.10 ± 67.77 |
| **Weight (1000 lbs)** |                 |                |                |                |
|    min                |      1.513      |           2.62 |     1.513      |           3.17 |
|    max                |      5.424      |           3.46 |      3.19      |          5.424 |
|    mean (sd)          |   3.22 ± 0.98   |    3.12 ± 0.36 |  2.29 ± 0.57   |    4.00 ± 0.76 |
| **Forward Gears**     |                 |                |                |                |
|    Three              |     15 (47)     |         2 (29) |     1 (9)      |        12 (86) |
|    Four               |     12 (38)     |         4 (57) |     8 (73)     |          0 (0) |
|    Five               |     5 (16)      |         1 (14) |     2 (18)     |         2 (14) |

mtcars2 data summary - new caption

### Adding P-values to a Summary Table

There are many different ways to format data summary tables. Adding
p-values to a table is just one thing that can be done in more than one
way. For example, if a row group reports the counts and percentages for
each level of a categorical variable across multiple (column) groups,
then I would argue that the p-value resulting from a chi square test or
a Fisher exact test would be best placed on the line of the table
labeling the row group. However, say we reported the minimum, median,
mean, and maximum with in a row group for one variable. The p-value from
a t-test, or other meaningful test for the difference in mean, I would
suggest should be reported on the line of the summary table for the
mean, not the row group itself.

With so many possibilities I have reserved construction of a p-value
column to be ad hoc. Perhaps an additional column wouldn’t be used and
the p-values are edited into row group labels, for example.

If you want to add a p-value column, or any other column(s) to a
`qwraps2_summary_table` object you can with some degree of ease. Note
that `qwraps2_summary_table` objects are just character matrices with
additional attributes.

``` r
str(both)
##  'qwraps2_summary_table' chr [1:17, 1:5] "**Miles Per Gallon**" ...
##  - attr(*, "dimnames")=List of 2
##   ..$ : NULL
##   ..$ : chr [1:5] "" "mtcars2 (N = 32)" "6 cylinders (N = 7)" "4 cylinders (N = 11)" ...
##  - attr(*, "qable_args")=List of 6
##   ..$ rtitle    : chr ""
##   ..$ rgroup    : Named int [1:4] 3 4 3 3
##   .. ..- attr(*, "names")= chr [1:4] "Miles Per Gallon" "Displacement" "Weight (1000 lbs)" "Forward Gears"
##   ..$ rnames    : chr [1:13] "min" "max" "mean (sd)" "min" ...
##   ..$ cnames    : chr [1:5] "" "mtcars2 (N = 32)" "6 cylinders (N = 7)" "4 cylinders (N = 11)" ...
##   ..$ markup    : chr "markdown"
##   ..$ kable_args:List of 1
##   .. ..$ caption: chr "mtcars2 data summary"
```

For this example, we will added p-values for testing the difference in
the mean between the three cylinder groups and the distribution of
forward gears by cylinder groups.

``` r
# difference in means
mpvals <-
  sapply(
         list(mpg  = lm(mpg  ~ cyl_factor, data = mtcars2),
              disp = lm(disp ~ cyl_factor, data = mtcars2),
              wt   = lm(wt   ~ cyl_factor, data = mtcars2)),
         extract_fpvalue)

# Fisher test
fpval <- frmtp(fisher.test(table(mtcars2$gear, mtcars2$cyl_factor))$p.value)
```

In this case, adding the p-value column, is done by creating a empty
column and then writing in the needed p-value on the wanted rows. This
could be within a row group (tests for means) or for a row group (Fisher
test).

``` r
both <- cbind(both, "P-value" = "")
both[grepl("mean \\(sd\\)", both[, 1]), "P-value"] <- mpvals
both[grepl("Forward Gears", both[, 1]), "P-value"] <- fpval
```

``` r
print(both, qable_args = list(kable_args = list(caption = "mtcars2 summary with p-values")))
```

|                       | mtcars2 (N = 32) | 6 cylinders (N = 7) | 4 cylinders (N = 11) | 8 cylinders (N = 14) | P-value       |
|:----------------------|:-----------------|:--------------------|:---------------------|:---------------------|:--------------|
| **Miles Per Gallon**  |                  |                     |                      |                      |               |
|    min                | 10.4             | 17.8                | 21.4                 | 10.4                 |               |
|    max                | 33.9             | 21.4                | 33.9                 | 19.2                 |               |
|    mean (sd)          | 20.09 ± 6.03     | 19.74 ± 1.45        | 26.66 ± 4.51         | 15.10 ± 2.56         | *P* \< 0.0001 |
| **Displacement**      |                  |                     |                      |                      |               |
|    min                | 71.1             | 145                 | 71.1                 | 275.8                |               |
|    median             | 196.3            | 167.6               | 108                  | 350.5                |               |
|    max                | 472              | 258                 | 146.7                | 472                  |               |
|    mean (sd)          | 230.72 ± 123.94  | 183.31 ± 41.56      | 105.14 ± 26.87       | 353.10 ± 67.77       | *P* \< 0.0001 |
| **Weight (1000 lbs)** |                  |                     |                      |                      |               |
|    min                | 1.513            | 2.62                | 1.513                | 3.17                 |               |
|    max                | 5.424            | 3.46                | 3.19                 | 5.424                |               |
|    mean (sd)          | 3.22 ± 0.98      | 3.12 ± 0.36         | 2.29 ± 0.57          | 4.00 ± 0.76          | *P* \< 0.0001 |
| **Forward Gears**     |                  |                     |                      |                      | *P* \< 0.0001 |
|    Three              | 15 (47)          | 2 (29)              | 1 (9)                | 12 (86)              |               |
|    Four               | 12 (38)          | 4 (57)              | 8 (73)               | 0 (0)                |               |
|    Five               | 5 (16)           | 1 (14)              | 2 (18)               | 2 (14)               |               |

mtcars2 summary with p-values

Another option you might consider is to have the p-value in the row
group name. Consider the following construction. The p-values are added
to the names of the row groups when building the summary table.

``` r
gear_summary <-
  list("Forward Gears" =
       list("Three" = ~ qwraps2::n_perc0(gear == 3),
            "Four"  = ~ qwraps2::n_perc0(gear == 4),
            "Five"  = ~ qwraps2::n_perc0(gear == 5)),
       "Transmission" =
       list("Automatic" = ~ qwraps2::n_perc0(am == 0),
            "Manual"    = ~ qwraps2::n_perc0(am == 1))
       )

gear_summary <-
setNames(gear_summary,
         c(
         paste("Forward Gears: ", frmtp(fisher.test(xtabs( ~ gear + cyl_factor, data = mtcars2))$p.value)),
         paste("Transmission: ",  frmtp(fisher.test(xtabs( ~ am + cyl_factor, data = mtcars2))$p.value)))
         )

summary_table(mtcars2, gear_summary, by = "cyl_factor")
```

|                                  | 6 cylinders (N = 7) | 4 cylinders (N = 11) | 8 cylinders (N = 14) |
|:---------------------------------|:--------------------|:---------------------|:---------------------|
| **Forward Gears: *P* \< 0.0001** |                     |                      |                      |
|    Three                         | 2 (29)              | 1 (9)                | 12 (86)              |
|    Four                          | 4 (57)              | 8 (73)               | 0 (0)                |
|    Five                          | 1 (14)              | 2 (18)               | 2 (14)               |
| **Transmission: *P* = 0.0091**   |                     |                      |                      |
|    Automatic                     | 4 (57)              | 3 (27)               | 12 (86)              |
|    Manual                        | 3 (43)              | 8 (73)               | 2 (14)               |

### rbind summary_table

There is a rbind method of summary tables. This can be useful when
building a large a table in smaller sections would be advantageous. For
example, it might be helpful to add p-values to a summary table with
just one row group and then rbind all the tables together for printing.
Consider that in the above example for adding p-values we have made an
assumption that the order of the summary and the `mpvals` will be
static. Remembering to make the sequence changes in more than one
location can be more difficult than we would like to admit. Writing code
to be robust to such changes is preferable.

``` r
t_mpg  <- summary_table(mtcars2, summaries = our_summary1["Miles Per Gallon"], by = "cyl_factor")
t_disp <- summary_table(mtcars2, summaries = our_summary1["Displacement"], by = "cyl_factor")
t_wt   <- summary_table(mtcars2, summaries = our_summary1["Weight (1000 lbs)"], by = "cyl_factor")

t_mpg  <- cbind(t_mpg,  "pvalue" = "")
t_disp <- cbind(t_disp, "pvalue" = "")
t_wt   <- cbind(t_wt,   "pvalue" = "")

t_mpg[ grepl("mean", t_mpg[, 1]),  "pvalue"] <- "mpg-pvalue"
t_disp[grepl("mean", t_disp[, 1]), "pvalue"] <- "disp-pvalue"
t_wt[  grepl("mean", t_wt[, 1]),   "pvalue"] <- "wt-pvalue"
```

Calling rbind now will let us have the table in different sequences
without having to worry about the alignment of rows between different
elements:

``` r
rbind(t_mpg, t_disp, t_wt)
## 
## 
## |                       |6 cylinders (N = 7)   |4 cylinders (N = 11)  |8 cylinders (N = 14)  |pvalue      |
## |:----------------------|:---------------------|:---------------------|:---------------------|:-----------|
## |**Miles Per Gallon**   |&nbsp;&nbsp;          |&nbsp;&nbsp;          |&nbsp;&nbsp;          |            |
## |&nbsp;&nbsp; min       |17.8                  |21.4                  |10.4                  |            |
## |&nbsp;&nbsp; max       |21.4                  |33.9                  |19.2                  |            |
## |&nbsp;&nbsp; mean (sd) |19.74 &plusmn; 1.45   |26.66 &plusmn; 4.51   |15.10 &plusmn; 2.56   |mpg-pvalue  |
## |**Displacement**       |&nbsp;&nbsp;          |&nbsp;&nbsp;          |&nbsp;&nbsp;          |            |
## |&nbsp;&nbsp; min       |145                   |71.1                  |275.8                 |            |
## |&nbsp;&nbsp; median    |167.6                 |108                   |350.5                 |            |
## |&nbsp;&nbsp; max       |258                   |146.7                 |472                   |            |
## |&nbsp;&nbsp; mean (sd) |183.31 &plusmn; 41.56 |105.14 &plusmn; 26.87 |353.10 &plusmn; 67.77 |disp-pvalue |
## |**Weight (1000 lbs)**  |&nbsp;&nbsp;          |&nbsp;&nbsp;          |&nbsp;&nbsp;          |            |
## |&nbsp;&nbsp; min       |2.62                  |1.513                 |3.17                  |            |
## |&nbsp;&nbsp; max       |3.46                  |3.19                  |5.424                 |            |
## |&nbsp;&nbsp; mean (sd) |3.12 &plusmn; 0.36    |2.29 &plusmn; 0.57    |4.00 &plusmn; 0.76    |wt-pvalue   |
rbind(t_wt, t_disp, t_mpg)
## 
## 
## |                       |6 cylinders (N = 7)   |4 cylinders (N = 11)  |8 cylinders (N = 14)  |pvalue      |
## |:----------------------|:---------------------|:---------------------|:---------------------|:-----------|
## |**Weight (1000 lbs)**  |&nbsp;&nbsp;          |&nbsp;&nbsp;          |&nbsp;&nbsp;          |            |
## |&nbsp;&nbsp; min       |2.62                  |1.513                 |3.17                  |            |
## |&nbsp;&nbsp; max       |3.46                  |3.19                  |5.424                 |            |
## |&nbsp;&nbsp; mean (sd) |3.12 &plusmn; 0.36    |2.29 &plusmn; 0.57    |4.00 &plusmn; 0.76    |wt-pvalue   |
## |**Displacement**       |&nbsp;&nbsp;          |&nbsp;&nbsp;          |&nbsp;&nbsp;          |            |
## |&nbsp;&nbsp; min       |145                   |71.1                  |275.8                 |            |
## |&nbsp;&nbsp; median    |167.6                 |108                   |350.5                 |            |
## |&nbsp;&nbsp; max       |258                   |146.7                 |472                   |            |
## |&nbsp;&nbsp; mean (sd) |183.31 &plusmn; 41.56 |105.14 &plusmn; 26.87 |353.10 &plusmn; 67.77 |disp-pvalue |
## |**Miles Per Gallon**   |&nbsp;&nbsp;          |&nbsp;&nbsp;          |&nbsp;&nbsp;          |            |
## |&nbsp;&nbsp; min       |17.8                  |21.4                  |10.4                  |            |
## |&nbsp;&nbsp; max       |21.4                  |33.9                  |19.2                  |            |
## |&nbsp;&nbsp; mean (sd) |19.74 &plusmn; 1.45   |26.66 &plusmn; 4.51   |15.10 &plusmn; 2.56   |mpg-pvalue  |
```

### Using Variable Labels

Some data management paradigms will use attributes to keep a label
associated with a variable in a data.frame. Notable examples are the
*[Hmisc](https://cran.r-project.org/package=Hmisc)* and
*[sjPlot](https://cran.r-project.org/package=sjPlot)*. If you associate
a label with a variable in the data frame the that label will be used
when building a summary table. This feature was suggested
<https://github.com/dewittpe/qwraps2/issues/74> and implemented thusly:

``` r
new_data_frame <-
  data.frame(age = c(18, 20, 24, 17, 43),
             edu = c(1, 3, 1, 5, 2),
             rt  = c(0.01, 0.04, 0.02, 0.10, 0.06))

# Set a label for the variables
attr(new_data_frame$age, "label") <- "Age in years"
attr(new_data_frame$rt,  "label") <- "Reaction time"

# mistakenly set the attribute to name instead of label
attr(new_data_frame$edu, "name") <- "Education"
```

When calling `qsummary` the provide labels for the age and rt variables
will be used. Since the attribute “label” does not exist for the edu
variable, edu will be used in the output.

``` r
qsummary(new_data_frame)
## $`Age in years`
## $`Age in years`$minimum
## ~qwraps2::frmt(min(age))
## 
## $`Age in years`$`median (IQR)`
## ~qwraps2::median_iqr(age)
## 
## $`Age in years`$`mean (sd)`
## ~qwraps2::mean_sd(age)
## 
## $`Age in years`$maximum
## ~qwraps2::frmt(max(age))
## 
## 
## $edu
## $edu$minimum
## ~qwraps2::frmt(min(edu))
## 
## $edu$`median (IQR)`
## ~qwraps2::median_iqr(edu)
## 
## $edu$`mean (sd)`
## ~qwraps2::mean_sd(edu)
## 
## $edu$maximum
## ~qwraps2::frmt(max(edu))
## 
## 
## $`Reaction time`
## $`Reaction time`$minimum
## ~qwraps2::frmt(min(rt))
## 
## $`Reaction time`$`median (IQR)`
## ~qwraps2::median_iqr(rt)
## 
## $`Reaction time`$`mean (sd)`
## ~qwraps2::mean_sd(rt)
## 
## $`Reaction time`$maximum
## ~qwraps2::frmt(max(rt))
```

This behavior is also seen with the `summary_table` call.

``` r
summary_table(new_data_frame)
```

|                   | new_data_frame (N = 5) |
|:------------------|:-----------------------|
| **Age in years**  |                        |
|    minimum        | 17.00                  |
|    median (IQR)   | 20.00 (18.00, 24.00)   |
|    mean (sd)      | 24.40 ± 10.74          |
|    maximum        | 43.00                  |
| **edu**           |                        |
|    minimum        | 1.00                   |
|    median (IQR)   | 2.00 (1.00, 3.00)      |
|    mean (sd)      | 2.40 ± 1.67            |
|    maximum        | 5.00                   |
| **Reaction time** |                        |
|    minimum        | 0.01                   |
|    median (IQR)   | 0.04 (0.02, 0.06)      |
|    mean (sd)      | 0.05 ± 0.04            |
|    maximum        | 0.10                   |

### Alternative building of the summaries

The task of building the `summaries` list-of-lists can be tedious. The
function `qummaries` is designed to make it easier. `qummaries` will use
a set of predefined functions to summarize numeric columns of a
data.frame, a set of arguments to pass to `n_perc` for categorical
(character and factor) variables.

By default, calling `summary_table` will use the default summary metrics
defined by `qsummary`. The purpose of `qsummary` is to provide the same
summary for all numeric variables within a data.frame and a single style
of summary for categorical variables within the data.frame. For example,
the default summary for a set of variables from the `mtcars2` data set
is

``` r
qsummary(mtcars2[, c("mpg", "cyl_factor", "wt")])
## $mpg
## $mpg$minimum
## ~qwraps2::frmt(min(mpg))
## 
## $mpg$`median (IQR)`
## ~qwraps2::median_iqr(mpg)
## 
## $mpg$`mean (sd)`
## ~qwraps2::mean_sd(mpg)
## 
## $mpg$maximum
## ~qwraps2::frmt(max(mpg))
## 
## 
## $cyl_factor
## $cyl_factor$`6 cylinders`
## ~qwraps2::n_perc(cyl_factor == "6 cylinders", digits = 0, show_symbol = FALSE)
## 
## $cyl_factor$`4 cylinders`
## ~qwraps2::n_perc(cyl_factor == "4 cylinders", digits = 0, show_symbol = FALSE)
## 
## $cyl_factor$`8 cylinders`
## ~qwraps2::n_perc(cyl_factor == "8 cylinders", digits = 0, show_symbol = FALSE)
## 
## 
## $wt
## $wt$minimum
## ~qwraps2::frmt(min(wt))
## 
## $wt$`median (IQR)`
## ~qwraps2::median_iqr(wt)
## 
## $wt$`mean (sd)`
## ~qwraps2::mean_sd(wt)
## 
## $wt$maximum
## ~qwraps2::frmt(max(wt))
```

That default summary is used for a table as follows:

``` r
summary_table(mtcars2[, c("mpg", "cyl_factor", "wt")])
```

|                 | mtcars2\[, c(“mpg”, “cyl_factor”, “wt”)\] (N = 32) |
|:----------------|:---------------------------------------------------|
| **mpg**         |                                                    |
|    minimum      | 10.40                                              |
|    median (IQR) | 19.20 (15.43, 22.80)                               |
|    mean (sd)    | 20.09 ± 6.03                                       |
|    maximum      | 33.90                                              |
| **cyl_factor**  |                                                    |
|    6 cylinders  | 7 (22)                                             |
|    4 cylinders  | 11 (34)                                            |
|    8 cylinders  | 14 (44)                                            |
| **wt**          |                                                    |
|    minimum      | 1.51                                               |
|    median (IQR) | 3.33 (2.58, 3.61)                                  |
|    mean (sd)    | 3.22 ± 0.98                                        |
|    maximum      | 5.42                                               |

Now, say we want to only report the minimum and maximum for each of the
numeric variables and for the categorical variables we want two show the
denominator for each category and for the percentage, to one digit with
the percent symbol in the table. Note that when defining the list of
numeric_summaries that the argument place holder is the `%s%` character.

``` r
new_summary <-
  qsummary(mtcars2[, c("mpg", "cyl_factor", "wt")],
           numeric_summaries = list("Minimum" = "~ min(%s)",
                                    "Maximum" = "~ max(%s)"),
           n_perc_args = list(digits = 1, show_symbol = TRUE, show_denom = "always"))
str(new_summary)
## List of 3
##  $ mpg       :List of 2
##   ..$ Minimum:Class 'formula'  language ~min(mpg)
##   .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
##   ..$ Maximum:Class 'formula'  language ~max(mpg)
##   .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
##  $ cyl_factor:List of 3
##   ..$ 6 cylinders:Class 'formula'  language ~qwraps2::n_perc(cyl_factor == "6 cylinders", digits = 1, show_symbol = TRUE,      show_denom = "always")
##   .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
##   ..$ 4 cylinders:Class 'formula'  language ~qwraps2::n_perc(cyl_factor == "4 cylinders", digits = 1, show_symbol = TRUE,      show_denom = "always")
##   .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
##   ..$ 8 cylinders:Class 'formula'  language ~qwraps2::n_perc(cyl_factor == "8 cylinders", digits = 1, show_symbol = TRUE,      show_denom = "always")
##   .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
##  $ wt        :List of 2
##   ..$ Minimum:Class 'formula'  language ~min(wt)
##   .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
##   ..$ Maximum:Class 'formula'  language ~max(wt)
##   .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv>
```

The resulting table is:

``` r
summary_table(mtcars2, new_summary)
```

|                | mtcars2 (N = 32) |
|:---------------|:-----------------|
| **mpg**        |                  |
|    Minimum     | 10.4             |
|    Maximum     | 33.9             |
| **cyl_factor** |                  |
|    6 cylinders | 7/32 (21.9%)     |
|    4 cylinders | 11/32 (34.4%)    |
|    8 cylinders | 14/32 (43.8%)    |
| **wt**         |                  |
|    Minimum     | 1.513            |
|    Maximum     | 5.424            |

## Session Info

``` r
print(sessionInfo(), local = FALSE)
## R version 4.5.2 (2025-10-31)
## Platform: x86_64-pc-linux-gnu
## Running under: Ubuntu 24.04.3 LTS
## 
## Matrix products: default
## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] qwraps2_0.6.2
## 
## loaded via a namespace (and not attached):
##  [1] digest_0.6.39     desc_1.4.3        R6_2.6.1          fastmap_1.2.0    
##  [5] xfun_0.55         cachem_1.1.0      knitr_1.51        htmltools_0.5.9  
##  [9] rmarkdown_2.30    lifecycle_1.0.4   cli_3.6.5         sass_0.4.10      
## [13] pkgdown_2.2.0     textshaping_1.0.4 jquerylib_0.1.4   systemfonts_1.3.1
## [17] compiler_4.5.2    tools_4.5.2       ragg_1.5.0        evaluate_1.0.5   
## [21] bslib_0.9.0       Rcpp_1.1.0        yaml_2.3.12       jsonlite_2.0.0   
## [25] rlang_1.1.6       fs_1.6.6
```
