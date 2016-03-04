# qwraps 0.1.2.9000

## New features
* `gmean_sd` returns the geometric mean and standard deviation 

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
