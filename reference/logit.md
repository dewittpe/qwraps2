# Deprecated logit and invlogit

Deprecated logit and invlogit functions. When originally written they
were faster than the base R
[`qlogis`](https://rdrr.io/r/stats/Logistic.html) and
[`plogis`](https://rdrr.io/r/stats/Logistic.html). However, that is no
longer the case.

## Usage

``` r
logit(x)

invlogit(x)
```

## Arguments

- x:

  a numeric vector
