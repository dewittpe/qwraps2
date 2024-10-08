# qwraps2 <img src="man/figures/qwraps2logo.png" align="right" height="150"/>

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/dewittpe/qwraps2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dewittpe/qwraps2/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/dewittpe/qwraps2/branch/master/graph/badge.svg)](https://codecov.io/gh/dewittpe/qwraps2)

[![OpenSSF Best Practices](https://www.bestpractices.dev/projects/8049/badge)](https://www.bestpractices.dev/projects/8049)

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/qwraps2)](https://cran.r-project.org/package=qwraps2)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/qwraps2)](http://www.r-pkg.org/pkg/qwraps2)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/qwraps2)](http://www.r-pkg.org/pkg/qwraps2)

[![License](https://img.shields.io/badge/licence-GPL--2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg)](https://cran.r-project.org/)

A collection of helpful functions for summarizing data and formatting results.
These or similar functions can be found in other R packages on github.com or
on CRAN.  However, this is a collection of methods I have generated to help with
particular projects I have worked on over the last several years.  Using my
original `qwraps` package as the foundation, this work is aimed at simplicity
and ease of use.

Some of the tools provided by `qwraps2` are:

* Formatting results:
  * `frmt`, `frmtci`, and `frmtp` make it easier to consistently format numeric
    values, confidence intervals, and p-values in reports.
  * `qable` is a wrapper around `knitr::kable` with my preferred defaults.
  * `summary_table` is used to generate data summary tables in markdown or LaTeX
  * `mean_ci`, `mean_sd`, `median_iqr`, `n_perc` make it easy to report formatted
    summary statistics.

* Plotting:
  Tools for building specific types of plots in
  [ggplot2](https://cran.r-project.org/package=ggplot2)
  * `qacf`: Autocorrelation plots
  * `qblandaltman`: Bland Altman plots
  * `qkmplot`: Kaplan-Meier plots
  * `qprc`: Precision Recall Curves
  * `qroc`: Receiver Operator Curves
  * `ggplot2_extract_legend`: extract the legend from a ggplot.

* R Programming and Development
  * `lazyload_cache` Load the cache generated by knitr into an interactive
    session.

* Other Tools:
  * `confusion_matrix`: building and generate statistics for confusion matrices.
  * `logit` and `invlogit`: quick logit and inverse logit functions
  * `ll`: a variant for `ls()`

----

## Contribute!
If you have a particular task or function used for data summaries or for
reporting that you think would be helpful to include in this package please fork
the repo, add the feature, and send me a pull request.

### Building the package
Use the makefile.  RStudio users, you will find a `qwraps2.Rproj` that will set
the default to use the makefile in the build.  My preferred IDE is
[neovim](https://neovim.io) with the
[Nvim-R](https://github.com/jalvesaq/Nvim-R) plug-in and I prefer to work on a
Debian system.  After cloning the repo, a simple call

    make

will build the package.  This includes generating man files via
[roxygen2](https://cran.r-project.org/package=roxygen2), building the vignettes,
and then building the package via `R CMD build .`.
Passing arguments to `R CMD build` can be done too.
For example, building the package without the vignettes is done via:

    make build-options=--no-build-vignettes

---

## Install

### From CRAN
Download and install from
[The Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/).

    install.packages("qwraps2", repo = "http://cran.rstudio.com")

### Developmental
Install the development version of `qwraps2` directly from github via the
[`remotes`](https://github.com/hadley/remotes/) package:

    if (!("remotes" %in% rownames(installed.packages()))) {
      warning("installing remotes from https://cran.rstudio.com")
      install.packages("remotes", repo = "https://cran.rstudio.com")
    }

    remotes::install_github("dewittpe/qwraps2", build_vignettes = TRUE)

*NOTE:* If you are working on a Windows machine you will need to download and
install [`Rtools`](https://cran.r-project.org/bin/windows/Rtools/).

### Cloned repo
Install with GNU `make`

    make install

