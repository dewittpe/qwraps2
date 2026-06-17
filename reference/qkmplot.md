# Kaplan-Meier Plot

A ggplot2 version of a Kaplan-Meier Plot

## Usage

``` r
qkmplot(x, conf_int = FALSE, ...)

qkmplot_build_data_frame(x)

# S3 method for class 'survfit'
qkmplot_build_data_frame(x)

qrmst(x, tau = Inf)

# S3 method for class 'survfit'
qrmst(x, tau = Inf)

# S3 method for class 'qkmplot_data'
qrmst(x, tau = Inf)
```

## Arguments

- x:

  object

- conf_int:

  logical if TRUE show the CI

- ...:

  Other arguments passed to survival::plot.survfit

- tau:

  upper bound on time for restricted mean survival time estimate

## Value

a ggplot.

## Details

Functions to build data frames explicitly or implicitly and then create
a ggplot2 Kaplan-Meier plot. The misspelled `qkmplot_bulid_data_frame`
is retained as a deprecated backward-compatible wrapper for
`qkmplot_build_data_frame`.

More details and examples for graphics within qwraps2 are in the
vignette(“qwraps2-graphics”, package = “qwraps2”)

## Examples

``` r
if (requireNamespace("survival", quietly = TRUE)) {

  leukemia.surv <- survival::survfit(survival::Surv(time, status) ~ x,
                                     data = survival::aml)

  qkmplot(leukemia.surv, conf_int = TRUE)

  qkmplot_build_data_frame(leukemia.surv)

  qrmst(leukemia.surv) # NaN for rmst.se in Nonmaintained strata as last observation is an event
  qrmst(leukemia.surv, 44)

  # pbc examples
  pbc_fit <-
    survival::survfit(
        formula = survival::Surv(time, status > 0) ~ trt
      , data = survival::pbc
      , subset = !is.na(trt)
    )

  qkmplot(pbc_fit)
  qkmplot(pbc_fit, conf_int = TRUE)

  qrmst(pbc_fit)
  qrmst(pbc_fit)
}
#>       strata     rmst     rmtl  rmst.se  tau
#> trt=1  trt=1 2755.835 1767.165 138.3484 4523
#> trt=2  trt=2 2811.100 1711.900 142.7538 4523
```
