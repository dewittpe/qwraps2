# Set Differences

Function for testing for unique values between two vectors,
specifically, which values are in vector1, and not in vector2, which
values are not in vector1 and in vector2, which values are in both
vector1 and vector2.

## Usage

``` r
set_diff(x, y)
```

## Arguments

- x, y:

  vectors (of the same mode)

## Value

a qwraps2_set_diff object, a list of set comparisons

- `all_values` = [`union`](https://rdrr.io/r/base/sets.html)`(x, y)`

- `x_only` = [`setdiff`](https://rdrr.io/r/base/sets.html)`(x, y)`

- `y_only` = [`setdiff`](https://rdrr.io/r/base/sets.html)`(y, x)`

- `both` = [`intersect`](https://rdrr.io/r/base/sets.html)`(x, y)`

- `equal` = [`setequal`](https://rdrr.io/r/base/sets.html)`(x, y)`

## Examples

``` r

# example with two sets which as a union are the upper and lower case vowels.
set_a <- c("A", "a", "E",      "I", "i", "O", "o", "U", "u", "E", "I")
set_b <- c("A", "a", "E", "e",      "i",      "o", "U", "u", "u", "a", "e")

set_diff(set_a, set_b)
#> Total number of unique values: 10
#> Number of elements in both set_a and set_b: 7
#> Number of elements only in set_a: 2
#>   unique elements: I, O
#> Number of elements only in set_b: 1
#>   unique elements: e
str(set_diff(set_a, set_b))
#> List of 5
#>  $ all_values: chr [1:10] "A" "a" "E" "I" ...
#>  $ x_only    : chr [1:2] "I" "O"
#>  $ y_only    : chr "e"
#>  $ both      : chr [1:7] "A" "a" "E" "i" ...
#>  $ equal     : logi FALSE
#>  - attr(*, "xname")= chr "set_a"
#>  - attr(*, "yname")= chr "set_b"
#>  - attr(*, "class")= chr "qwraps2_set_diff"

set_diff(set_b, set_a)
#> Total number of unique values: 10
#> Number of elements in both set_b and set_a: 7
#> Number of elements only in set_b: 1
#>   unique elements: e
#> Number of elements only in set_a: 2
#>   unique elements: I, O

# example
set_a <- 1:90
set_b <- set_a[-c(23, 48)]
set_diff(set_a, set_b)
#> Total number of unique values: 90
#> Number of elements in both set_a and set_b: 88
#> Number of elements only in set_a: 2
#>   unique elements: 23, 48
#> Number of elements only in set_b: 0
set_diff(set_b, set_a)
#> Total number of unique values: 90
#> Number of elements in both set_b and set_a: 88
#> Number of elements only in set_b: 0
#> Number of elements only in set_a: 2
#>   unique elements: 23, 48

# example
set_a <- c("A", "A", "B")
set_b <- c("B", "A")
set_diff(set_a, set_b)
#> Total number of unique values: 2
#> Number of elements in both set_a and set_b: 2
#> Number of elements only in set_a: 0
#> Number of elements only in set_b: 0
```
