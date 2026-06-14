# Backtick

Encapsulate a string in backticks. Very helpful for inline code in
[`spin`](https://rdrr.io/pkg/knitr/man/spin.html) scripts.

## Usage

``` r
backtick(x, dequote = FALSE)
```

## Arguments

- x:

  the thing to be deparsed and encapsulated in backticks

- dequote:

  remove the first and last double or signal quote form `x`

## Examples

``` r
backtick("a quoted string")
#> [1] "`\"a quoted string\"`"
backtick(no-quote)
#> [1] "`no - quote`"
backtick(noquote)
#> [1] "`noquote`"
```
