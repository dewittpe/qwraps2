# Mean and Standard Error (of the mean)

A function for calculating and formatting means and standard deviations.

## Usage

``` r
mean_se(
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

  defaults to "ifNA". Other options are "always" or "never".

- denote_sd:

  a character string set to either "pm" or "paren" for reporting 'mean
  \\\pm\\ sd' or 'mean (sd)'

- markup:

  latex or markdown

- ...:

  pass through

## Value

a character vector of the formatted values

## Details

Given a numeric vector, `mean_se` will return a character string with
the mean and standard error of the mean. Formatting of the output will
be extended in future versions.

## Examples

``` r
set.seed(42)
x <- rnorm(1000, 3, 4)
mean(x)
#> [1] 2.896702
sd(x) / sqrt(length(x)) # standard error
#> [1] 0.12681
mean_se(x)
#> [1] "2.90 $\\pm$ 0.13"
mean_se(x, show_n = "always")
#> [1] "1,000; 2.90 $\\pm$ 0.13"
mean_se(x, show_n = "always", denote_sd = "paren")
#> [1] "1,000; 2.90 (0.13)"

x[187] <- NA
mean_se(x, na_rm = TRUE)
#> [1] "999; 2.89 $\\pm$ 0.13"
```
