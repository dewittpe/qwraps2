# qwraps2

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/dewittpe/qwraps2.svg?branch=master)](https://travis-ci.org/dewittpe/qwraps2)

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/qwraps2)](http://cran.r-project.org/package=qwraps2)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/qwraps2)](http://www.r-pkg.org/pkg/qwraps2)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/qwraps2)](http://www.r-pkg.org/pkg/qwraps2)

[![Licence](https://img.shields.io/badge/licence-GPL--2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.0.2-6666ff.svg)](https://cran.r-project.org/)
[![packageversion](https://img.shields.io/badge/Package%20version-0.2.1-orange.svg?style=flat-square)](commits/master)
[![Last-changedate](https://img.shields.io/badge/last%20change-`r gsub('-', '--', Sys.Date())`-yellowgreen.svg)](/commits/master)

A collection of helpful functions for summarizing data and formating results.
These functions are almost certainly found in other R packages on github.com, or
on CRAN.  This is a collection of methods I have generated to help with
particular projects I have worked on over the last several years.  Using my
origiinal `qwraps` package as the foundation, this work is aimed at simplicity
and ease of use.  

----

## Contribute!
If you have a particular task or function used for data summaries or for
reporting that you think would be helpful to include in this package please fork
the repo, add the feature, and send me a pull request.  

---
 
## Install

### From CRAN
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/qwraps2)](http://cran.r-project.org/package=qwraps2)
is on
[The Comprehensive R Archive Network (CRAN)](http://cran.r-project.org/). You
can install this version via

    install.packages("qwraps2", repo = "http://cran.rstudio.com")

### Developmental
Install the development version (version x.y.z.9000) of `qwraps2`, via the 
[`devtools`](https://github.com/hadley/devtools) package:

    # install.packages("devtools")
    library(devtools)
    install_github("dewittpe/qwraps2")

If you are working on a Windows machine you will need to download and install
[`Rtools`](http://cran.r-project.org/bin/windows/Rtools/) before `devtools` will
work for you.

