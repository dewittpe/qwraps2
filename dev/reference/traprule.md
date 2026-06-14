# Trapezoid Rule Numeric Integration

Compute the integral of y with respect to x via trapezoid rule.

## Usage

``` r
traprule(x, y)
```

## Arguments

- x, y:

  numeric vectors of equal length

## Value

a numeric value, the estimated integral

## Examples

``` r
xvec <- seq(-2 * pi, 3 * pi, length = 560)
foo  <- function(x) { sin(x) + x * cos(x) + 12 }
yvec <- foo(xvec)
plot(xvec, yvec, type = "l")


integrate(f = foo, lower = -2 * pi, upper = 3 * pi)
#> 188.4956 with absolute error < 0.00027
traprule(xvec, yvec)
#> [1] 188.4953
```
