# Contributing to the qwraps2 package

## Style Guide

For the most part, follow the [Style Guide](http://adv-r.had.co.nz/Style.html)
by Hadley Wickham.  Please just try to be consistent with the style you see.

## Cloning the Repo

    git clone git@github.com:dewittpe/qwraps2.git

## Package Structure

There is a `Makefile` at the package root which controls the build process.

The `vignette-spinners` directory contains the editable `.R` spinner files.  The
generated `.Rmd` files live in `vignettes/` and **should not** be edited
manually.  The `.R` files are spun to `.Rmd` via
[`knitr::spin`](https://yihui.name/knitr/demo/stitch/).
