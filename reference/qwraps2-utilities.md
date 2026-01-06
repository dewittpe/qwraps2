# Operators

A set of helpful operators to make writing and basic data analysis
easier.

## Usage

``` r
e1 %s% e2
```

## Arguments

- e1:

  a character string

- e2:

  a character string

## Examples

``` r
# base R
paste0("A longer string ", "can be ", "built")
#> [1] "A longer string can be built"

# with the %s% operator
"A longer string " %s% "can be " %s% "built"
#> [1] "A longer string can be built"
```
