# Geometric Mean, Variance, and Standard Deviation

Return the geometric mean, variance, and standard deviation,

## Usage

``` r
gmean(x, na_rm = FALSE)

gvar(x, na_rm = FALSE)

gsd(x, na_rm = FALSE)
```

## Arguments

- x:

  a numeric vector

- na_rm:

  a logical value indicating whether `NA` values should be stripped
  before the computation proceeds.

## Value

a numeric value

## See also

[`gmean_sd`](http://www.peteredewitt.com/qwraps2/reference/gmean_sd.md)
for easy formatting of the geometric mean and standard deviation.
`vignette("summary-statistics", package = "qwraps2")`.

## Examples

``` r
gmean(mtcars$mpg)
#> [1] 19.25006
identical(gmean(mtcars$mpg), exp(mean(log(mtcars$mpg))))
#> [1] TRUE

gvar(mtcars$mpg)
#> [1] 1.089695
identical(gvar(mtcars$mpg),
          exp(var(log(mtcars$mpg)) * (nrow(mtcars) - 1) / nrow(mtcars)))
#> [1] TRUE

gsd(mtcars$mpg)
#> [1] 1.340555
identical(gsd(mtcars$mpg),
          exp(sqrt( var(log(mtcars$mpg)) * (nrow(mtcars) - 1) / nrow(mtcars))))
#> [1] TRUE

#############################################################################
set.seed(42)
x <- runif(14, min = 4, max = 70)

# geometric mean - four equivalent ways to get the same result
prod(x) ^ (1 / length(x))
#> [1] 41.1274
exp(mean(log(x)))
#> [1] 41.1274
1.2 ^ mean(log(x, base = 1.2))
#> [1] 41.1274
gmean(x)
#> [1] 41.1274

# geometric variance
gvar(x)
#> [1] 1.252071

# geometric sd
exp(sd(log(x)))                                     ## This is wrong (incorrect sample size)
#> [1] 1.63563
exp(sqrt((length(x) - 1) / length(x)) * sd(log(x))) ## Correct calculation
#> [1] 1.606616
gsd(x)
#> [1] 1.606616

# Missing data will result in and NA being returned
x[c(2, 4, 7)] <- NA
gmean(x)
#> [1] NA
gmean(x, na_rm = TRUE)
#> [1] 37.2996
gvar(x, na_rm = TRUE)
#> [1] 1.270338
gsd(x, na_rm = TRUE)
#> [1] 1.630955
```
