# Spin Comment Check

A tool to help identify the opening and closing of comments in a spin
document. This function is designed to help the user resolve the error
"comments must be put in pairs of start and end delimiters."

## Usage

``` r
spin_comments(hair, comment = c("^[# ]*/[*]", "^.*[*]/ *$"), text = NULL, ...)
```

## Arguments

- hair:

  Path to the R script. The script must be encoded in UTF-8 if it
  contains multi-byte characters.

- comment:

  A pair of regular expressions for the start and end delimiters of
  comments; the lines between a start and an end delimiter will be
  ignored. By default, the delimiters are `/*` at the beginning of a
  line, and `*/` at the end, following the convention of C-style
  comments.

- text:

  A character vector of code, as an alternative way to provide the R
  source. If `text` is not `NULL`, `hair` will be ignored.

- ...:

  additional arguments (not currently used.)

## Examples

``` r
spin_comments(hair = system.file("examples/spinner1.R", package = "qwraps2"))
#> Warning: comments must be put in pairs of start and end delimiters.
#>   * started on line 5; ended on line 7
#>   * no starting delimiter; ended on line 15
#>   * started on line 22; ended on line 24
#>   * started on line 20; no end delimiter
#> [1] FALSE
#> attr(,"notes")
#> [1] "  * started on line 5; ended on line 7"     
#> [2] "  * no starting delimiter; ended on line 15"
#> [3] "  * started on line 22; ended on line 24"   
#> [4] "  * started on line 20; no end delimiter"   
```
