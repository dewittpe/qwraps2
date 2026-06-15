# qwraps2: Formatted Summary Statistics

``` r

set.seed(42)
library(qwraps2)
# define the markup language we are working in.
# options(qwraps2_markup = "latex") is also supported.
options(qwraps2_markup = "markdown")
```

## Introduction

It is common for a manuscript to require a data summary table. The table
might include simple summary statistics for the whole sample and for
subgroups. There are several tools available to build such tables. In my
opinion, though, most of those tools have nuances imposed by the
creators/authors such that other users need not only understand the
tool, but also think like the authors. I wrote this package to be as
flexible and general as possible. I hope you like these tools and will
be able to use them in your work.

This vignette presents the use of the `summary_table`, `qsummary`, and
`qable` functions for quickly building data summary tables. We will be
using summary statistic functions, `mean_sd`, `median_iqr`, `n_perc`,
and others, from *[qwraps2](https://cran.r-project.org/package=qwraps2)*
as well.

### Prerequisites Example Data Set

``` r

library(qwraps2)
```

We will use the data set `mtcars2` for the examples throughout this
vignette. It is a modified and extended version of the base R data set
`mtcars` . For details on the construction of the `mtcars2` data set
please view the vignette:
[`vignette("qwraps2-data-sets", package = "qwraps2")`](http://www.peteredewitt.com/qwraps2/articles/qwraps2-data-sets.md)

``` r

data(mtcars2)
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

## Review of Summary Statistic Functions and Formatting

### Means and Standard Deviations

`mean_sd` returns the (arithmetic) mean and standard deviation for
numeric vector as a formatted character string. For example,
`mean_sd(mtcars2$mpg)` returns the formatted string 20.09 ± 6.03. There
are other options for formatting the character string:

``` r

mean_sd(mtcars2$mpg)
## [1] "20.09 &plusmn; 6.03"
mean_sd(mtcars2$mpg, denote_sd = "paren")
## [1] "20.09 (6.03)"
```

### Mean and Confidence intervals

If you need the mean and a confidence interval there is the function
`mean_ci`. which returns a `qwraps2_mean_ci` object which is a named
vector with the mean, lower confidence limit, and the upper confidence
limit. The printing method for `qwraps2_mean_ci` objects is a call to
the `frmtci` function. You can modify the formatting of the printed
result by adjusting the arguments passed to `frmtci`.

``` r

mci <- mean_ci(mtcars2$mpg)
str(mci)
##  'qwraps2_mean_ci' Named num [1:3] 20.1 18 22.2
##  - attr(*, "names")= chr [1:3] "mean" "lcl" "ucl"
##  - attr(*, "alpha")= num 0.05
mci
## [1] "20.09 (18.00, 22.18)"
print(mci, show_level = TRUE)
## [1] "20.09 (95% CI: 18.00, 22.18)"
```

### Median and Inner Quartile Range

Similar to the `mean_sd` function, the `median_iqr` returns the median
and the inner quartile range (IQR) of a data vector.

``` r

median_iqr(mtcars2$mpg)
## [1] "19.20 (15.43, 22.80)"
```

### Count and Percentages

The `n_perc` function is the workhorse. `n_perc0` is also provided for
ease of use in the same way that base R has `paste` and `paste0` .
`n_perc` returns the n (%) with the percentage sign in the string,
`n_perc0` omits the percentage sign from the string. The latter is good
for tables, the former for in-line text.

``` r

n_perc(mtcars2$cyl == 4)
## [1] "11 (34.38%)"
n_perc0(mtcars2$cyl == 4)
## [1] "11 (34)"

n_perc(mtcars2$cyl_factor == 4)  # this returns 0 (0.00%)
## [1] "0 (0.00%)"
n_perc(mtcars2$cyl_factor == "4 cylinders")
## [1] "11 (34.38%)"
n_perc(mtcars2$cyl_factor == levels(mtcars2$cyl_factor)[2])
## [1] "11 (34.38%)"

# The count and percentage of 4 or 6 cylinder vehicles in the data set is
n_perc(mtcars2$cyl %in% c(4, 6))
## [1] "18 (56.25%)"
```

### Geometric Means and Standard Deviations

Let $`\left\{x_1, x_2, x_3, \ldots, x_n \right\}`$ be a sample of size
$`n`$ with $`x_i > 0`$ for all $`i.`$ Then the geometric mean,
$`\mu_g,`$ and geometric standard deviation are

``` math
\begin{equation}
  \mu_g = \left( \prod_{i = 1}^{n} x_i \right)^{\frac{1}{n}} =
  b^{ \frac{1}{n} \sum_{i = 1}^{n} \log_{b} x_i },
\end{equation}
```
and
``` math
\begin{equation}
  \sigma_g = b ^ {
  \sqrt{
    \frac{\sum_{i = 1}^{n} \left( \log_{b} \frac{x_i}{\mu_g} \right)^2}{n}
  }
  }
\end{equation}
```
or, for clarity,
``` math
\begin{equation}
  \log_{b} \sigma_g =
  \sqrt{ \frac{\sum_{i = 1}^{n} \left( \log_{b} \frac{x_i}{\mu_g}
  \right)^2}{n}}
\end{equation}
```

When looking for the geometric standard deviation in R, the simple
`exp(sd(log(x)))` does not match the population-denominator definition
used here. The geometric standard deviation above uses $`n,`$ the full
sample size, in the denominator, whereas the `sd` and `var` functions in
R use the denominator $`n - 1.`$ To get the geometric standard deviation
one should adjust the result by multiplying the variance by
$`(n - 1) / n`$ or the standard deviation by $`\sqrt{(n - 1) / n}.`$ See
the example below.

``` r

x <- runif(6, min = 4, max = 70)

# geometric mean
mu_g <- prod(x) ** (1 / length(x))
mu_g
## [1] 46.50714
exp(mean(log(x)))
## [1] 46.50714
1.2 ** mean(log(x, base = 1.2))
## [1] 46.50714

# geometric standard deviation
exp(sd(log(x)))  ## Uses the sample denominator, n - 1
## [1] 1.500247

# these equations are correct
sigma_g <- exp(sqrt(sum(log(x / mu_g) ** 2) / length(x)))
sigma_g
## [1] 1.448151

exp(sqrt((length(x) - 1) / length(x)) * sd(log(x)))
## [1] 1.448151

# geometric variance
geometric_variance <- exp(log(sigma_g) ** 2)
geometric_variance
## [1] 1.146958
```

The functions `gmean`, `gvar`, and `gsd` provide the geometric mean,
variance, and standard deviation for a numeric vector. Here, geometric
variance is defined as $`\exp((\log \sigma_g)^2),`$ not as
$`\sigma_g^2.`$

``` r

gmean(x)
## [1] 46.50714
all.equal(gmean(x), mu_g)
## [1] TRUE

gvar(x)
## [1] 1.146958
all.equal(gvar(x), sigma_g^2)       # This is supposed to be FALSE
## [1] "Mean relative difference: 0.8284385"
all.equal(gvar(x), geometric_variance)
## [1] TRUE

gsd(x)
## [1] 1.448151
all.equal(gsd(x), sigma_g)
## [1] TRUE
```

`gmean_sd` will provide a quick way for reporting the geometric mean and
geometric standard deviation in the same way that `mean_sd` does for the
arithmetic mean and arithmetic standard deviation:

``` r

gmean_sd(x)
## [1] "46.51 &plusmn; 1.45"
```

## Building a Data Summary Table

The function `summary_table` appears to be the most widely used tool
provided by the qwraps2 package. As such, that function has earned its
own vignette.

``` r

vignette("qwraps2-summary-table")
```

## Session Info

``` r

print(sessionInfo(), local = FALSE)
## R version 4.6.0 (2026-04-24)
## Platform: x86_64-pc-linux-gnu
## Running under: Ubuntu 24.04.4 LTS
## 
## Matrix products: default
## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
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
