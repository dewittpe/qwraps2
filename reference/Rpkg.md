# Formatting Style on URLs for packages on CRAN, Github, and Gitlab.

Functions for controlling the look of package names in markdown created
vignettes and easy curating of URLs for the packages.

## Usage

``` r
Rpkg(pkg)

CRANpkg(pkg)

Githubpkg(pkg, username)

GitHubpkg(pkg, username)

Gitlabpkg(pkg, username)
```

## Arguments

- pkg:

  The name of the package, will work as a quoted or raw name.

- username:

  username for Github.com or Gitlab.com

## Examples

``` r

Rpkg(qwraps2)
#> [1] "*qwraps2*"
Rpkg("qwraps2")
#> [1] "*qwraps2*"

CRANpkg(qwraps2)
#> [1] "*[qwraps2](https://cran.r-project.org/package=qwraps2)*"
CRANpkg("qwraps2")
#> [1] "*[qwraps2](https://cran.r-project.org/package=qwraps2)*"

Githubpkg(qwraps2, "dewittpe")
#> [1] "*[qwraps2](https://github.com/dewittpe/package=qwraps2)*"
Githubpkg("qwraps2", dewittpe)
#> [1] "*[qwraps2](https://github.com/dewittpe/package=qwraps2)*"

Gitlabpkg(qwraps2, "dewittpe")
#> [1] "*[qwraps2](https://gitlab.com/dewittpe/package=qwraps2)*"
Gitlabpkg("qwraps2", dewittpe)
#> [1] "*[qwraps2](https://gitlab.com/dewittpe/package=qwraps2)*"
```
