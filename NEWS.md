# qwraps2 0.2.4.9000

## New Features
* Added `lazyload_cache_dir` and `lazyload_cache_labels` for (lazy)loading of
  cached chunks.  Very useful for loading cached `knitr` code chunks into an
  interactive R session.

* Added `traprule` for a quick way to integrate a numeric vector

* Added `create_pkg`: a wrapper about `devtools::create` with more defaults that
  I prefer.

* Added a `file_check` (#50) to check for read access and optionally md5sum of files.

* Added a `pkg_check` (#51) to check if a list of packages are available.

## Improvements:
* Extended the documentation for `logit` and `invlogit` to reference bases R
  methods.
* Added to the examples for `summary_table` showing how to add a caption to a
  LaTeX table (#39)

## Bug Fixes
* Factor levels as characters in `summary_table` (#48)


# qwraps2 0.2.4

## New Features
* `frmtci.data.frame` method added.
* for the `tab_summary` methods to new arguments: `n_perc_args` and `envir`.
  The former gives the end user control over the formating options passed to
  `n_perc` and the latter controls the `environment` associated with the
  generated formulae.

## Bug Fixes:
* `frmtci.matrix` using the `est` arg correctly

## Improvements
* extended documentation and examples for `summary_table`

# qwraps2 0.2.3

## New features
* `qacf` plot (#11)
* `ll` an improved version of `ls()`
* `rtitle` option added to `qable` (#35)

## Bug Fixes:
* `perc_n` reports the strings in the correct order.

## Improvements:
* Extended documentation for `qable`
* Minor edits and extensions to the summary-statistics vignette.

# qwraps2 0.2.2

## New Features:
* `summary_table` and `tab_summary` added (#33)
* New vignette `vignette("summary-statistics, package = "qwraps2")`

## Bug Fixes:
* boot strapped statistics in confusion matrices

# qwraps 0.2.1

## New Features:
* `logit` and `invlogit` functions added.  Using Rcpp so they are fast.

## Bug Fixes:
* `format` is correctly passed from `qwraps2::qable` to `knitr::kable`'s
  `format` argument.

# qwraps 0.2.0
This is a long over due version bump.  Many new features and bug fixes have been
made since 0.1.0.

## Bug Fixes:
* `confusion_matrix` produces the correct confusion matrix.  Prior version could
  transpose the result.
* `gmean_sd` displays the correct geometric standard deviation

## Improvements
* `mean_sd` and `gmean_sd` have better `show_n` handeling.

## New Features
* `print.confusion_matrix` provides more detail to make it clear which variable
  is the Truth and which is the Prediction.
* `ggplot2_extract_legend` added (#6)
* `show_n` functionality for `median_iqr` implemented.  (closes #24)
* `frmtci` added (#32)
* `mean_ci` added

# qwraps 0.1.4

## Bug Fixes:
* `confusion_matrix.formula` no longer uses named columns.  The function only
  worked when called from `confusion_matrix.default`.  Fix allows the function
  to be used as intended.

# qwraps 0.1.3

## New features
* `gmean_sd` returns the geometric mean and standard deviation 

## Notable Changes
* The `confusion_matrix` method has been modified.  Please read the
  documentation.  If you used version 0.1.2 these changes will affect your work.
  The `confusion_matrix` is now an S3 generic.

# qwraps 0.1.2

## New features

* methods for finding the sensitivity and specificity of confusion matrices
  (contingency tables) added.
* `median_iqr` returns a formatted string with the median and IQR of a data vector.
* `perc_n` similar to the `n_perc` but returns a string of the form `p% (n = N)`.
* `n_perc` has option to suppress the percent symbol
* `n_perc0` added in 0.1.1.9004 sets defaults which may be preferred for tables.
* `qkmplot` for plotting kaplan meier curves
* `qable` creates `knitr::kable` tables with row groups and names similar to
  `hmisc::latex` or `htmlTable::htmlTable`.

# qwraps 0.1.1
Pushed to CRAN on 20 April 2015.

## New features

* `extract_fstat` extracts the F-statistic from `lm`
* `extract_fpvalue` extracts and formats the omibus F-test p-value from `lm`
* P-value formating for [Obstetrics & Gynecology](http://www.editorialmanager.com/ong/default.aspx)

## New vignettes

* none

## Minor improvements

* Set R dependence to 3.0.2 to match the dependence noted for dplyr
* Default setting for the qwraps2_frmtp_digits is 4 instead of 2.
* Spelling and grammar fixes in documentation (#21)

## Bug fixes

* `mean_sd` passes the `digits` option to `frmt` correctly (#20)

# qwraps2 0.1

Initial release.
