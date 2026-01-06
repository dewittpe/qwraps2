# List Object Aliases

Aliases for [`ls`](https://rdrr.io/r/base/ls.html) providing additional
details.

## Usage

``` r
ll(
  pos = 1,
  pattern,
  order_by = "size",
  decreasing = order_by %in% c("size", "rows", "columns")
)
```

## Arguments

- pos:

  specifies the environment as a position in the search list

- pattern:

  an optional regular expression. Only names matching `pattern` are
  returned. [`glob2rx`](https://rdrr.io/r/utils/glob2rx.html) can be
  used to convert wildcard patterns to regular expressions.

- order_by:

  a character, order the results by “object”, “size” (default), “class”,
  “rows”, or “columns”.

- decreasing:

  logical, defaults to `TRUE`, decreasing order? passed to
  [`order`](https://rdrr.io/r/base/order.html).

## Value

a data.frame with columns

- object: name of the object

- class: class, or mode if class is not present, of the object

- size: approximate size, in bytes, of the object in memory

- rows: number of rows for data.frames or matrices, or the number of
  elements for a list like structure

- columns: number of columns for data.frames or matrices

## References

The basis for this work came from a Stack Overflow posting:
<https://stackoverflow.com/q/1358003/1104685>

## See also

[`ls`](https://rdrr.io/r/base/ls.html)

## Examples

``` r
# View your current workspace
if (FALSE) { # \dontrun{
ls()
ll()
} # }

# View another environment
e <- new.env()
ll(e)
#> character(0)

e$fit <- lm(mpg ~ wt, mtcars)
e$fit2 <- lm(mpg ~ wt + am + vs, data = mtcars)
e$x <- rnorm(1e5)
e$y <- runif(1e4)
e$z <- with(e, x * y)
e$w <- sum(e$z)

ls(e)
#> [1] "fit"  "fit2" "w"    "x"    "y"    "z"   
ll(e)
#>   object   class   size   rows columns
#> 1      x numeric 800048 100000      NA
#> 2      z numeric 800048 100000      NA
#> 3      y numeric  80048  10000      NA
#> 4   fit2      lm  30976     12      NA
#> 5    fit      lm  25528     12      NA
#> 6      w numeric     56      1      NA
```
