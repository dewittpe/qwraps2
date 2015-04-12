# qwraps2

[![Build Status](https://travis-ci.org/dewittpe/qwraps2.svg?branch=master)](https://travis-ci.org/dewittpe/qwraps2)

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


----

## Install

Version 0.1 is on [CRAN](http://cran.r-project.org/) and can be installed via:

    install.packages("qwraps2", repo = "http://cran.rstudio.com")

Install the development version (version x.y.z.9000) of `qwraps2`, via the 
[`devtools`](https://github.com/hadley/devtools) package:

    # install.packages("devtools")
    library(devtools)
    install_github("qwraps2", username = "dewittpe")

If you are working on a Windows machine you will need to download and install
[`Rtools`](http://cran.r-project.org/bin/windows/Rtools/) before `devtools` will
work for you.

