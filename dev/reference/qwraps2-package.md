# Quick Wraps for Reproducible Reports

qwraps2 is a collection of helper functions for authoring reproducible
analysis reports. The package focuses on formatted summary statistics,
summary tables, regression-result formatting, and ggplot2-based plots.

## Details

Several wrappers for ggplot2-style graphics are provided, including
receiver operating characteristic curves, precision-recall curves,
Bland-Altman plots, autocorrelation plots, and Kaplan-Meier plots. These
functions include
[`qroc`](http://www.peteredewitt.com/qwraps2/dev/reference/qroc-qprc.md),
[`qprc`](http://www.peteredewitt.com/qwraps2/dev/reference/qroc-qprc.md),
[`qacf`](http://www.peteredewitt.com/qwraps2/dev/reference/qacf.md),
[`qblandaltman`](http://www.peteredewitt.com/qwraps2/dev/reference/qblandaltman.md),
and
[`qkmplot`](http://www.peteredewitt.com/qwraps2/dev/reference/qkmplot.md).

Other functions quickly generate meaningful character strings for
reporting results in .Rnw, .Rmd, or similar report files.

## Vignettes

- [`vignette("qwraps2-formatted-summary-statistics", package = "qwraps2")`](http://www.peteredewitt.com/qwraps2/dev/articles/qwraps2-formatted-summary-statistics.md)
  introduces individual summary-statistic and formatting helpers.

- [`vignette("qwraps2-summary-table", package = "qwraps2")`](http://www.peteredewitt.com/qwraps2/dev/articles/qwraps2-summary-table.md)
  demonstrates full summary-table workflows.

- [`vignette("qwraps2-graphics", package = "qwraps2")`](http://www.peteredewitt.com/qwraps2/dev/articles/qwraps2-graphics.md)
  demonstrates the plotting helpers.

- [`vignette("qwraps2-data-sets", package = "qwraps2")`](http://www.peteredewitt.com/qwraps2/dev/articles/qwraps2-data-sets.md)
  documents the bundled example data sets.

## Options

There are several options which can be set via `options` and will be
used via `getOption`. The following lists, in alphabetical order the
different options which are available and what they control.

- `getOption("qwraps2_alpha", 0.05)` significance level, used for
  generating `(1 - getOption("qwraps2_alpha", 0.05)) * 100`% confidence
  intervals, and determining significance for p-value \<
  `getOption("qwraps2_alpha", 0.05)`.

- `getOption("qwraps2_frmt_digits", 2)` Number of digits to the right of
  the decimal point for any value other than p-values.

- `getOption("qwraps2_frmtp_case", "upper")` set to either 'upper' or
  'lower' for the case of the 'P' for reporting p-values.

- `getOption("qwraps2_frmtp_digits", 4)` Number of digits to the right
  of the decimal point to report p-values too. If
  `log10(p-value) < getOption("qwraps2_frmtp_digits", 4)` then the
  output will be "P \< 0.01", to however many digits are correct. Other
  options control other parts of the output p-value format.

- `getOption("qwraps2_frmtp_leading0", TRUE)` to display or not to
  display the leading zero in p-values, i.e., if TRUE p-values are
  reported as 0.02 versus when FALSE p-values are reported as .02.

- `getOption("qwraps2_journal", "default")` if a journal has specific
  formatting for p-values or other statistics, this option will control
  the output. Many other options are ignored if this is any other than
  default. Check the github wiki, or this file, for current lists of
  implemented journal style methods.

- `getOption("qwraps2_markup", "latex")` value set to 'latex' or to
  'markdown'. Output is formatted to meet requirements of either markup
  language.

## Journals with predefined formatting

- Obstetrics & Gynecology

  - <https://www.editorialmanager.com/ong/default.aspx>

  - `options(qwraps2_journal = "obstetrics_gynecology")`

  - P-value formatting as of April 2015:

    Express P values to no more than three decimal places.

    Based on observations of published work, leading 0 will be omitted.

- Pediatric Dentistry:

  - <https://www.aapd.org/publications/>

  - `options(qwraps2_journal = "pediatric_dentistry")`

  - P-value formatting as of March 2015.

    If P \> .01, the actual value for P should be expressed to 2 digits.
    Non-significant values should not be expressed as "NS" whether or
    not P is significant, unless rounding a significant P-value
    expressed to 3 digits would make it non-significant (i.e., P=.049,
    not P=.05). If P\<.01, it should be expressed to 3 digits (e.g.,
    P=.003, not P\<.05). Actual P-values should be expressed unless
    P\<.001, in which case they should be so designated.

## See also

Useful links:

- <https://github.com/dewittpe/qwraps2/>

- <https://www.peteredewitt.com/qwraps2/>

- Report bugs at <https://github.com/dewittpe/qwraps2/issues>

## Author

**Maintainer**: Peter DeWitt <dewittpe@gmail.com>
([ORCID](https://orcid.org/0000-0002-6391-0795))

Authors:

- Peter DeWitt <dewittpe@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-6391-0795))

Other contributors:

- Tell Bennett <tell.bennett@cuanschutz.edu>
  ([ORCID](https://orcid.org/0000-0003-1483-4236)) \[contributor\]
