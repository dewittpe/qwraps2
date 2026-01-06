# Count and Percentage

A function for calculating and formatting counts and percentages.

## Usage

``` r
n_perc(
  x,
  digits = getOption("qwraps2_frmt_digits", 2),
  na_rm = FALSE,
  show_denom = "ifNA",
  show_symbol = TRUE,
  markup = getOption("qwraps2_markup", "latex"),
  ...
)

perc_n(
  x,
  digits = getOption("qwraps2_frmt_digits", 2),
  na_rm = FALSE,
  show_denom = "ifNA",
  show_symbol = FALSE,
  markup = getOption("qwraps2_markup", "latex"),
  ...
)

n_perc0(
  x,
  digits = 0,
  na_rm = FALSE,
  show_denom = "never",
  show_symbol = FALSE,
  markup = getOption("qwraps2_markup", "latex"),
  ...
)
```

## Arguments

- x:

  a 0:1 or boolean vector

- digits:

  digits to the right of the decimal point to return in the percentage
  estimate.

- na_rm:

  if true, omit NA values

- show_denom:

  defaults to "ifNA". Other options are "always" or "never".

- show_symbol:

  if TRUE (default) the percent symbol is shown, else it is suppressed.

- markup:

  latex or markdown

- ...:

  pass through

## Value

a character vector of the formatted values

## Details

Default behavior will return the count of successes and the percentage
as "N (pp can be controlled by setting `na.rm = TRUE`. In this case, the
number of non-missing values will be reported by default. Omission of
the non-missing values can be controlled by setting
`show_denom = "never"`.

The function n_perc0 uses a set of default arguments which may be
advantageous for use in building tables.

## Examples

``` r
n_perc(c(0, 1,1, 1, 0, 0), show_denom = "always")
#> [1] "3/6 (50.00\\%)"
n_perc(c(0, 1,1, 1, 0, 0, NA), na_rm = TRUE)
#> [1] "3/6 (50.00\\%)"

n_perc(mtcars$cyl == 6)
#> [1] "7 (21.88\\%)"

set.seed(42)
x <- rbinom(4269, 1, 0.314)
n_perc(x)
#> [1] "1,383 (32.40\\%)"
n_perc(x, show_denom = "always")
#> [1] "1,383/4,269 (32.40\\%)"
n_perc(x, show_symbol = FALSE)
#> [1] "1,383 (32.40)"

# n_perc0 examples
n_perc0(c(0, 1,1, 1, 0, 0))
#> [1] "3 (50)"
n_perc0(mtcars$cyl == 6)
#> [1] "7 (22)"
```
