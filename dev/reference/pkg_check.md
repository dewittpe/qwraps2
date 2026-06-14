# Package Checks

Check if a package is available on the local machine and optionally
verify a version.

## Usage

``` r
pkg_check(pkgs, versions, stop = FALSE)
```

## Arguments

- pkgs:

  a character vector of package names to check for

- versions:

  an optional character vector, of the same length of `pkgs` for the
  minimum version of the packages.

- stop:

  if `TRUE` then an error is thrown if any of the checks fail. If
  `FALSE` (default) a logical is returned.

## Details

When writing a script that will be shared it is very likely that the
multiple authors/users will need to have a certain set of packages
available to load. The `pkg_check` function will verify that the
packages are available to load, this includes an optional version test,
and attach the package to the search list if requested.

Testing for package versions will is done as
`packageVersion(x) >= version`. If you need a specific version of a
package you should explicitly use `packageVersion(x) == version` in your
script. In general, `pkg_check` is a handy tool in interactive sessions.
For a package you should have package version documentation in the
DESCRIPTION file. For a script a base R solution of
`stopifnot(packageVersion("pkg") >= "x.y.z")`

## Examples

``` r
# verify that the packages qwraps2, and ggplot2 are available (this should be
# TRUE if you have qwraps2 installed since ggplot2 is imported by qwraps2)
pkg_check(c("qwraps2", "ggplot2"))
#> [1] TRUE

# show that the return is FALSE if a package is not available
pkg_check(c("qwraps2", "ggplot2", "NOT a PCKG"))
#> [1] FALSE
#> attr(,"checks")
#>               package version available installed_version
#> qwraps2       qwraps2      NA      TRUE        0.6.2.9000
#> ggplot2       ggplot2      NA      TRUE             4.0.3
#> NOT a PCKG NOT a PCKG      NA     FALSE              <NA>

# verify the version for just ggplot2
pkg_check(c("qwraps2", "ggplot2"), c(NA, "2.2.0"))
#> [1] TRUE

# verify the version for qwraps2 (this is expected to fail as we are looking for
# version 42.3.14 which is far too advanced for the actual package development.
pkg_check(c("qwraps2", "ggplot2"), c("42.3.14", "2.2.0"))
#> [1] FALSE
#> attr(,"checks")
#>         package version available installed_version
#> qwraps2 qwraps2 42.3.14     FALSE        0.6.2.9000
#> ggplot2 ggplot2   2.2.0      TRUE             4.0.3

if (FALSE) { # \dontrun{
  # You can have the function throw an error is any of the checks fail
  pkg_check(c("qwraps2", "ggplot2"),
            c("42.3.14", "2.2.0"),
            stop = TRUE)
} # }

if (FALSE) { # \dontrun{
  # If you have missing packages that can be installed from CRAN you may find
  # the following helpful.  If this code, with the needed edits, were placed at
  # the top of a script, then if a package is missing then the current version
  # from a target repository will be installed.  Use this set up with
  # discretion, others may not want the automatic install of packages.
  pkgs <- pkg_check("<packages to install>")
  if (!pkgs) {
    install.packages(attr(pkgs, "checks")[!attr(pkgs, "checks")$available][["package"]])
  }
} # }

```
