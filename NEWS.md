# qwraps 0.1.1.9003

## New features

* `median_iqr` returns a formatted string with the median and IQR of a data vector.
* `perc_n` similar to the `n_perc` but returns a string of the form `p% (n = N)`.
* `n_perc` has option to supress the percent symbol
* `qkmplot` is under development

## New vignettes

*

## Minor improvements

*

## Bug fixes

*

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
