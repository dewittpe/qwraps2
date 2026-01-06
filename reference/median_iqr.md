# Median and Inner Quartile Range

A function for calculating and formatting the median and inner quartile
range of a data vector.

## Usage

``` r
median_iqr(
  x,
  digits = getOption("qwraps2_frmt_digits", 2),
  na_rm = FALSE,
  show_n = "ifNA",
  markup = getOption("qwraps2_markup", "latex"),
  ...
)
```

## Arguments

- x:

  a numeric vector

- digits:

  digits to the right of the decimal point to return.

- na_rm:

  if true, omit NA values

- show_n:

  defaults to "ifNA". Other options are "always" or "never".

- markup:

  latex or markdown

- ...:

  pass through

## Value

a character vector of the formatted values

## Details

Given a numeric vector, `median_iqr` will return a character string with
the median and IQR. Formatting of the output will be extended in future
versions.

## Examples

``` r
set.seed(42)
x <- rnorm(1000, 3, 4)
median(x)
#> [1] 2.947463
quantile(x, probs = c(1, 3)/4)
#>      25%      75% 
#> 0.298158 5.656021 
median_iqr(x)
#> [1] "2.95 (0.30, 5.66)"
median_iqr(x, show_n = "always")
#> [1] "1,000; 2.95 (0.30, 5.66)"

x[187] <- NA
# median_iqr(x) ## Will error
median_iqr(x, na_rm = TRUE)
#> [1] "999; 2.95 (0.28, 5.65)"
```
