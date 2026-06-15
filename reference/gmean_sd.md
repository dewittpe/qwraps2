# Geometric Mean and Standard deviation

A function for calculating and formatting geometric means and standard
deviations.

## Usage

``` r
gmean_sd(
  x,
  digits = getOption("qwraps2_frmt_digits", 2),
  na_rm = FALSE,
  show_n = "ifNA",
  denote_sd = "pm",
  markup = getOption("qwraps2_markup", "latex"),
  ...
)
```

## Arguments

- x:

  a numeric vector

- digits:

  digits to the right of the decimal point to return in the percentage
  estimate.

- na_rm:

  if true, omit NA values

- show_n:

  defaults to “ifNA”. Other options are “always” or “never”.

- denote_sd:

  a character string set to either "pm" or "paren" for reporting 'mean
  \\\pm\\ sd' or 'mean (sd)'

- markup:

  character string with value “latex” or “markdown”

- ...:

  pass through

## Value

a character vector of the formatted values

## Details

Given a numeric vector, `gmean_sd` will return a character string with
the geometric mean and standard deviation. Formatting of the output will
be extended in future versions.

## See also

[`mean_sd`](http://www.peteredewitt.com/qwraps2/reference/mean_sd.md),
[`gmean`](http://www.peteredewitt.com/qwraps2/reference/gmean.md),
[`gsd`](http://www.peteredewitt.com/qwraps2/reference/gmean.md)

## Examples

``` r

gmean_sd(mtcars$mpg, markup = "latex")
#> [1] "19.25 $\\pm$ 1.34"
gmean_sd(mtcars$mpg, markup = "markdown")
#> [1] "19.25 &plusmn; 1.34"
```
