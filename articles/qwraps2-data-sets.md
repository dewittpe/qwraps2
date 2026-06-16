# qwraps2: Data Sets

``` r

library(qwraps2)
packageVersion("qwraps2")
## [1] '0.6.3'
```

## mtcars2

The base R package datasets provides the mtcars data set. The
information in mtcars is the fuel consumption and automobile
characteristics of 32 automobiles as reported in the March, April, June
and July 1974 issues of *Motor Trend* magazine (Hocking 1976).

That dataset is modified and extended to provide support for examples
within the qwraps2 package documentation. This vignette documents the
construction of mtcars2.

## Construction of mtcars2

Starting with the original mtcars:

``` r

mtcars2 <- mtcars
str(mtcars2)
## 'data.frame':    32 obs. of  11 variables:
##  $ mpg : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
##  $ cyl : num  6 6 4 6 8 6 8 4 4 6 ...
##  $ disp: num  160 160 108 258 360 ...
##  $ hp  : num  110 110 93 110 175 105 245 62 95 123 ...
##  $ drat: num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
##  $ wt  : num  2.62 2.88 2.32 3.21 3.44 ...
##  $ qsec: num  16.5 17 18.6 19.4 17 ...
##  $ vs  : num  0 0 1 1 0 1 0 1 1 1 ...
##  $ am  : num  1 1 1 0 0 0 0 0 0 0 ...
##  $ gear: num  4 4 4 3 3 3 3 4 4 4 ...
##  $ carb: num  4 4 1 1 2 1 4 2 2 4 ...
```

The cyl column provides the number of cylinders in the engine of the
automobiles. We will use two additional versions of this information,
one as character column and one as a factor. Please note that the order
of the factor levels is intentionally set to be non-sequential. This
will help to illustrate the ordering of results when using a factor or a
character vector as a grouping variable.

``` r

mtcars2$cyl_character <- paste(mtcars2$cyl, "cylinders")
mtcars2$cyl_factor    <- factor(mtcars2$cyl,
                                levels = c(6, 4, 8),
                                labels = paste( c(6, 4, 8), "cylinders"))
```

Create other factor variables.

``` r

mtcars2$gear_factor <-
  factor(mtcars2$gear, levels = c(3, 4, 5), labels = paste(c(3, 4, 5), "forward gears"))
```

Engine configuration: the `vs` column is an integer vector indicating
V-shaped or straight. The constructed column engine is a factor with the
same information as a labeled factor.

``` r

mtcars2$engine <-
  factor(mtcars2$vs, levels = c(0, 1), labels = c("V-shaped", "straight"))
```

Transmission: the `am` column is an integer vector indicating if the
transmission is automatic or manual. We construct a `transmission`
column to provide the same information as a factor.

``` r

mtcars2$transmission <-
  factor(mtcars2$am, levels = c(0, 1), labels = c("Automatic", "Manual"))
```

The rownames of the mtcars2 data set provide the make and model of the
automobiles. Here we will create columns for make and model and then
omit the rownames.

``` r

mtcars2$make  <- sub("^(\\w+)\\s(.+)", "\\1", rownames(mtcars2))
mtcars2$model <- sub("^(\\w+)\\s(.+)", "\\2", rownames(mtcars2))
rownames(mtcars2) <- NULL
```

To have some dates to use in examples we are going to add a mostly
arbitrary date column to mtcars2. Given that the data came from the
March through July issues of *Motor Trend* in 1974, we will create a
`test_date` column starting in January 1974 forward with one to three
tests per week through May 1974. This assumes the data is in
chronological order.

``` r

set.seed(42)
mtcars2$test_date <-
  as.POSIXct("1974-01-03", tz = "GMT") +
  cumsum(sample(c(2, 3, 4, 7) * 3600 * 24, size = nrow(mtcars2), replace = TRUE))
```

Lastly we will order the columns of mtcars2 so similar columns are next
to each other.

``` r

mtcars2 <-
  mtcars2[, c("make", "model", "mpg", "disp", "hp", "drat", "wt", "qsec",
              "cyl", "cyl_character", "cyl_factor",
              "vs", "engine",
              "am", "transmission",
              "gear", "gear_factor",
              "carb",
              "test_date")]
```

## Summary of mtcars2

mtcars2 is a data frame with 32 observations with 19 variables. Some of
the variables tell us the same information, but in different formats.

``` r

str(mtcars2)
## 'data.frame':    32 obs. of  19 variables:
##  $ make         : chr  "Mazda" "Mazda" "Datsun" "Hornet" ...
##  $ model        : chr  "RX4" "RX4 Wag" "710" "4 Drive" ...
##  $ mpg          : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
##  $ disp         : num  160 160 108 258 360 ...
##  $ hp           : num  110 110 93 110 175 105 245 62 95 123 ...
##  $ drat         : num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
##  $ wt           : num  2.62 2.88 2.32 3.21 3.44 ...
##  $ qsec         : num  16.5 17 18.6 19.4 17 ...
##  $ cyl          : num  6 6 4 6 8 6 8 4 4 6 ...
##  $ cyl_character: chr  "6 cylinders" "6 cylinders" "4 cylinders" "6 cylinders" ...
##  $ cyl_factor   : Factor w/ 3 levels "6 cylinders",..: 1 1 2 1 3 1 3 2 2 1 ...
##  $ vs           : num  0 0 1 1 0 1 0 1 1 1 ...
##  $ engine       : Factor w/ 2 levels "V-shaped","straight": 1 1 2 2 1 2 1 2 2 2 ...
##  $ am           : num  1 1 1 0 0 0 0 0 0 0 ...
##  $ transmission : Factor w/ 2 levels "Automatic","Manual": 2 2 2 1 1 1 1 1 1 1 ...
##  $ gear         : num  4 4 4 3 3 3 3 4 4 4 ...
##  $ gear_factor  : Factor w/ 3 levels "3 forward gears",..: 2 2 2 1 1 1 1 2 2 2 ...
##  $ carb         : num  4 4 1 1 2 1 4 2 2 4 ...
##  $ test_date    : POSIXct, format: "1974-01-05" "1974-01-07" ...
```

| Element | Name | Description |
|---:|:---|:---|
| \[, 1\] | make | Vehicle Manufacturer |
| \[, 2\] | model | Vehicle model |
| \[, 3\] | mpg | Miles/(US) gallon |
| \[, 4\] | disp | Displacement (cu.in.) |
| \[, 5\] | hp | Gross horsepower |
| \[, 6\] | drat | Rear axle ratio |
| \[, 7\] | wt | Weight (1000 lbs) |
| \[, 8\] | qsec | 1/4 mile time |
| \[, 9\] | cyl | Number of cylinders |
| \[, 10\] | cyl_character | Number of cylinders as a character string |
| \[, 11\] | cyl_factor | Number of cylinders as a factor |
| \[, 12\] | vs | Engine (0 = V-shaped, 1 = straight) |
| \[, 13\] | engine | same info as vs, but as a factor |
| \[, 14\] | am | Transmission (0 = automatic, 1 = manual) |
| \[, 15\] | transmission | same info as am as a factor |
| \[, 16\] | gear | Number of forward gears |
| \[, 17\] | gear_factor | Number of forward gears as a factor |
| \[, 18\] | carb | Number of carburetors |
| \[, 19\] | test_date | arbitrary date - created to approximate when the vehicle would have been assessed. |

## pefr

Peak expiratory flow rate (pefr) data is used for examples within the
qwraps2 package. The data has been transcribed from (Bland and Altman
1986).

> The sample comprised colleagues and family of J.M.B. chosen to give a
> wide range of PEFR but in no way representative of any defined
> population. Two measurements were made with a Wright peak flow meter
> and two with a mini Wright meter, in random order. All measurements
> were taken by J.M.B., using the same two instruments. (These data were
> collected to demonstrate the statistical method and provide no
> evidence on the comparability of these two instruments.) We did not
> repeat suspect readings and took a single reading as our measurement
> of PEFR. Only the first measurement by each method is used to
> illustrate the comparison of methods, the second measurements being
> used in the study of repeatability.

The units of measure for the pefr are liters per minute (L/min).

``` r

# copied text from the manuscript
pefr_table <-
  read.delim(
             header = FALSE,
             text = "
1   494 490 512 525
2   395 397 430 415
3   516 512 520 508
4   434 401 428 444
5   476 470 500 500
6   557 611 600 625
7   413 415 364 460
8   442 431 380 390
9   650 638 658 642
10  433 429 445 432
11  417 420 432 420
12  656 633 626 605
13  267 275 260 227
14  478 492 477 467
15  178 165 259 268
16  423 372 350 370
17  427 421 451 443")
```

Build the data set

``` r

pefr <-
  expand.grid(subject = 1:17,
              measurement = 1:2,
              meter   = c("Wright peak flow meter", "Mini Wright peak flow meter"),
              KEEP.OUT.ATTRS = FALSE,
              stringsAsFactors = FALSE)
pefr$pefr <- do.call(c, pefr_table[, 2:5])

head(pefr)
##   subject measurement                  meter pefr
## 1       1           1 Wright peak flow meter  494
## 2       2           1 Wright peak flow meter  395
## 3       3           1 Wright peak flow meter  516
## 4       4           1 Wright peak flow meter  434
## 5       5           1 Wright peak flow meter  476
## 6       6           1 Wright peak flow meter  557
```

See
[`vignette("qwraps2-graphics", package = "qwraps2")`](http://www.peteredewitt.com/qwraps2/articles/qwraps2-graphics.md)
for examples using this data set, specifically in the construction and
use of Bland-Altman plots via `qblandaltman`.

## Spambase

Spambase (Hopkins and Suermondt 1999) is a useful data set for examples
needing a binary outcome and several possible predictors. The data set
and documentation can be found in this package in the directory on your
machine at:

``` r

system.file("spambase", package = "qwraps2")
## [1] "/home/runner/work/_temp/Library/qwraps2/spambase"
```

The data set `spambase` was generated thusly:

``` r

nms <-
  scan(system.file("spambase", "spambase.names", package = "qwraps2")
       , what = character()
       , skip = 33
       , sep = "\n"
       , quiet = TRUE
  )
nms <- sapply(strsplit(nms, split = ":"), getElement, 1)
nms <- c(nms, "spam")

# clean up char_freq names
nms <-
  nms |>
  sub(";", "semicolon",         x = _, fixed = TRUE) |>
  sub("(", "parenthesis",       x = _, fixed = TRUE) |>
  sub("[", "square_bracket",    x = _, fixed = TRUE) |>
  sub("!", "exclamation_point", x = _, fixed = TRUE) |>
  sub("$", "dollar_sign",       x = _, fixed = TRUE) |>
  sub("#", "pound",             x = _, fixed = TRUE)

spambase <- read.csv(
    file = system.file("spambase", "spambase.data", package = "qwraps2")
    , header = FALSE
    , col.names = nms
)
```

There are 4,601 rows of data with 57 predictors for the binary outcome
`spam`

``` r

n_perc(spambase$spam) # count and percent of spam messages
## [1] "1,813 (39.40\\%)"
```

## References

Bland, J Martin, and DouglasG Altman. 1986. “Statistical Methods for
Assessing Agreement Between Two Methods of Clinical Measurement.” *The
Lancet* 327 (8476): 307–10.

Hocking, Ronald R. 1976. “A Biometrics Invited Paper. The Analysis and
Selection of Variables in Linear Regression.” *Biometrics* 32 (1): 1–49.

Hopkins, Reeber, Mark, and Jaap Suermondt. 1999. *Spambase*. UCI Machine
Learning Repository.

## Session Info

``` r

sessionInfo()
## R version 4.6.0 (2026-04-24)
## Platform: x86_64-pc-linux-gnu
## Running under: Ubuntu 24.04.4 LTS
## 
## Matrix products: default
## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
## 
## locale:
##  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
##  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
##  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
## [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
## 
## time zone: UTC
## tzcode source: system (glibc)
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] qwraps2_0.6.3
## 
## loaded via a namespace (and not attached):
##  [1] digest_0.6.39     desc_1.4.3        R6_2.6.1          fastmap_1.2.0    
##  [5] xfun_0.58         cachem_1.1.0      knitr_1.51        htmltools_0.5.9  
##  [9] rmarkdown_2.31    lifecycle_1.0.5   cli_3.6.6         sass_0.4.10      
## [13] pkgdown_2.2.0     textshaping_1.0.5 jquerylib_0.1.4   systemfonts_1.3.2
## [17] compiler_4.6.0    tools_4.6.0       ragg_1.5.2        bslib_0.11.0     
## [21] evaluate_1.0.5    Rcpp_1.1.1-1.1    yaml_2.3.12       otel_0.2.0       
## [25] jsonlite_2.0.0    rlang_1.2.0       fs_2.1.0          htmlwidgets_1.6.4
```
