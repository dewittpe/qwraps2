# logit and inverse logit functions

transform `x` either via the logit, or inverse logit.

## Usage

``` r
logit(x)

invlogit(x)
```

## Arguments

- x:

  a numeric vector

## Details

The logit and inverse logit functions are part of R via the logistic
distribution functions in the stats package. Quoting from the
documentation for the logistic distribution

"`qlogis(p)` is the same as the `logit` function,
`logit(p) = log(p/1-p)`, and `plogis(x)` has consequently been called
the 'inverse logit'."

See the examples for benchmarking these functions. The `logit` and
`invlogit` functions are faster than the `qlogis` and `plogis`
functions.

## See also

[`qlogis`](https://rdrr.io/r/stats/Logistic.html)

## Examples

``` r
library(rbenchmark)

# compare logit to qlogis
p <- runif(1e5)
identical(logit(p), qlogis(p))
#> [1] TRUE

if (FALSE) { # \dontrun{
rbenchmark::benchmark(logit(p), qlogis(p))
} # }

# compare invlogit to plogis
x <- runif(1e5, -1000, 1000)
identical(invlogit(x), plogis(x))
#> [1] TRUE

if (FALSE) { # \dontrun{
rbenchmark::benchmark(invlogit(x), plogis(x))
} # }
```
