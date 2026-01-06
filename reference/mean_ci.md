# Means and Confidence Intervals

A function for calculating and formatting means and confidence interval.

## Usage

``` r
mean_ci(
  x,
  na_rm = FALSE,
  alpha = getOption("qwraps2_alpha", 0.05),
  qdist = stats::qnorm,
  qdist.args = list(),
  ...
)

# S3 method for class 'qwraps2_mean_ci'
print(x, ...)
```

## Arguments

- x:

  a numeric vector

- na_rm:

  if true, omit NA values

- alpha:

  defaults to `getOption('qwraps2_alpha', 0.05)`. The symmetric
  100(1-alpha)% CI will be determined.

- qdist:

  defaults to `qnorm`. use `qt` for a Student t intervals.

- qdist.args:

  list of arguments passed to `qdist`

- ...:

  arguments passed to `frmtci`.

## Value

a vector with the mean, lower confidence limit (LCL), and the upper
confidence limit (UCL).

## Details

Given a numeric vector, `mean_ci` will return a vector with the mean,
LCL, and UCL. Using `frmtci` will be helpful for reporting the results
in print.

## See also

[`frmtci`](http://www.peteredewitt.com/qwraps2/reference/frmt.md)

## Examples

``` r
# using the standard normal for the CI
mean_ci(mtcars$mpg)
#> [1] "20.09 (18.00, 22.18)"

# print it nicely
qwraps2::frmtci(mean_ci(mtcars$mpg))
#> [1] "20.09 (18.00, 22.18)"
qwraps2::frmtci(mean_ci(mtcars$mpg), show_level = TRUE)
#> [1] "20.09 (95% CI: 18.00, 22.18)"
qwraps2::frmtci(mean_ci(mtcars$mpg, alpha = 0.01), show_level = TRUE)
#> [1] "20.09 (99% CI: 17.35, 22.83)"

# Compare to the ci that comes form t.test
t.test(mtcars$mpg)
#> 
#>  One Sample t-test
#> 
#> data:  mtcars$mpg
#> t = 18.857, df = 31, p-value < 2.2e-16
#> alternative hypothesis: true mean is not equal to 0
#> 95 percent confidence interval:
#>  17.91768 22.26357
#> sample estimates:
#> mean of x 
#>  20.09062 
#> 
t.test(mtcars$mpg)$conf.int
#> [1] 17.91768 22.26357
#> attr(,"conf.level")
#> [1] 0.95
mean_ci(mtcars$mpg, qdist = stats::qt, qdist.args = list(df = 31))
#> [1] "20.09 (17.92, 22.26)"
```
