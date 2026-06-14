# Extract Summary stats from regression objects

A collection of functions for extracting summary statistics and
reporting regression results from `lm`, `glm` and other regression
objects.

## Usage

``` r
extract_fstat(x)

extract_fpvalue(x)

# S3 method for class 'lm'
extract_fpvalue(x)
```

## Arguments

- x:

  a `lm` object

## Value

a character vector of the formatted numbers

formatted p-value from the F-test

## See also

[`lm`](https://rdrr.io/r/stats/lm.html)

## Examples

``` r
fit <- lm(mpg ~ wt + hp + drat, data = mtcars)
summary(fit)
#> 
#> Call:
#> lm(formula = mpg ~ wt + hp + drat, data = mtcars)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -3.3598 -1.8374 -0.5099  0.9681  5.7078 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 29.394934   6.156303   4.775 5.13e-05 ***
#> wt          -3.227954   0.796398  -4.053 0.000364 ***
#> hp          -0.032230   0.008925  -3.611 0.001178 ** 
#> drat         1.615049   1.226983   1.316 0.198755    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 2.561 on 28 degrees of freedom
#> Multiple R-squared:  0.8369, Adjusted R-squared:  0.8194 
#> F-statistic: 47.88 on 3 and 28 DF,  p-value: 3.768e-11
#> 
extract_fstat(fit)
#> [1] "$F_{3, 28} = 47.88$"
extract_fpvalue(fit)
#> [1] "$P < 0.0001$"
```
