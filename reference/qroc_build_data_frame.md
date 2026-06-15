# Deprecated qroc and qprc building of data frames:

Deprecated methods for building the data sets needed for plotting
Receiver-Operating Curves and Precision-Recall Curves. Use
[`confusion_matrix`](http://www.peteredewitt.com/qwraps2/reference/confusion_matrix.md)
instead.

## Usage

``` r
qroc_build_data_frame(fit, truth = NULL, n_threshold = 200, ...)

# Default S3 method
qroc_build_data_frame(fit, truth = NULL, n_threshold = 200, ...)

# S3 method for class 'glm'
qroc_build_data_frame(fit, truth = NULL, n_threshold = 200, ...)

qprc_build_data_frame(fit, n_threshold = 200, ...)
```

## Arguments

- fit:

  a `glm` fit with `family = binomial()`, or predicted values

- truth:

  ignored if `fit` is a `glm` object. A vector of observations, 0/1 or
  FALSE/TRUE values, of equal length to `fit`

- n_threshold:

  number of thresholds to use to estimate auroc or auprc

- ...:

  passed to [`predict`](https://rdrr.io/r/stats/predict.html)
