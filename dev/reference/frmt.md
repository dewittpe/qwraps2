# Format Wrappers

Functions for formatting numeric values for consistent display in
reports.

## Usage

``` r
frmt(x, digits = getOption("qwraps2_frmt_digits", 2), append = NULL)

frmtp(
  x,
  style = getOption("qwraps2_journal", "default"),
  digits = getOption("qwraps2_frmtp_digits", 4),
  markup = getOption("qwraps2_markup", "latex"),
  case = getOption("qwraps2_frmtp_case", "upper"),
  leading0 = getOption("qwraps2_frmtp_leading0", TRUE)
)

frmtci(
  x,
  est = 1,
  lcl = 2,
  ucl = 3,
  format = "est (lcl, ucl)",
  show_level = FALSE,
  ...
)
```

## Arguments

- x:

  a vector of numbers or a numeric matrix to format.

- digits:

  number of digits, including trailing zeros, to the right of the
  decimal point. This option is ignored if `is.integer(x) == TRUE)`.

- append:

  a character string to append to the formatted number. This is
  particularly useful for percentages or adding punctuation to the end
  of the formatted number. This should be a vector of length 1, or equal
  to the length of `x`.

- style:

  a character string indicating a specific journal requirements for
  p-value formatting.

- markup:

  a character string indicating if the output should be latex or markup.

- case:

  a character string indicating if the output should be upper case or
  lower case.

- leading0:

  boolean, whether or not the p-value should be reported as 0.0123
  (TRUE, default), or .0123 (FALSE).

- est:

  the numeric index of the vector element or the matrix column
  containing the point estimate.

- lcl:

  the numeric index of the vector element or the matrix column
  containing the lower confidence limit.

- ucl:

  the numeric index of the vector element or the matrix column
  containing the upper confidence limit.

- format:

  a string with "est" "lcl", and "ucl" to denote the location of the
  estimate, lower confidence limit, and upper confidence limit for the
  formatted string. Defaults to "est (lcl, ucl)".

- show_level:

  defaults to FALSE. If TRUE and `format` is the default, then
  "100\*(1-options()\$qwraps2_alpha) parenthesis and the lcl. If set to
  a string, then the given string will be placed between the left
  parenthesis and the lcl. If the `format` is not the default, then this
  argument is ignored.

- ...:

  args passed to frmt

## Value

a character vector of the formatted numbers

## Details

\`frmt\` was originally really just a wrapper for the `formatC`. It has
extended functionality now as I have found common uses cases.

\`frmtp\` formats P-values per journal requirements. As I work on papers
aimed at different journals, the formatting functions will be extended
to match.

Default settings are controlled through the function arguments but
should be set via [`options()`](https://rdrr.io/r/base/options.html).

Default settings report the P-value exactly if P \>
`getOption("qwraps2_frmtp_digits", 4)` and reports P \<
`10^-(getOption("qwraps2_frmtp_digits", 2))` otherwise. By the leading
zero is controlled via `getOption("qwraps2_frmtp_leading0", TRUE)` and a
upper or lower case P is controlled by
`getOption("qwraps2_frmtp_case", "upper")`. These options are ignored if
`style != "default"`.

Journals with predefined P-value formatting are noted in the
[qwraps2](http://www.peteredewitt.com/qwraps2/dev/reference/qwraps2-package.md)
documentation.

\`frmtci\` takes a `matrix`, or `data.frame`, with a point estimate and
the lcl and ucl and formats a string for reporting. est (lcl, ucl) is
the default. The confidence level can be added to the string, e.g., "est
(95 format.

\`frmtcip\` expects four values, est, lcl, ucl, and p-value. The
resulting sting will be of the form "est (lcl, ucl; p-value)".

The \`Rpkg\`, \`CRANpkg\`, and \`Githubpkg\` functions are used to help
make documenting packages stylistically consistent and with valid urls.
These functions were inspired by similar ones found in the BioConductor
BiocStyle package.

## See also

[`formatC`](https://rdrr.io/r/base/formatc.html)

## Examples

``` r

### Formatting numbers
integers <- c(1234L, 9861230L)
numbers  <- c(1234,  9861230)
frmt(integers)  # no decimal point
#> [1] "1,234"     "9,861,230"
frmt(numbers)   # decimal point and zeros to the right
#> [1] "1,234.00"     "9,861,230.00"

numbers <- c(0.1234, 0.1, 1234.4321, 0.365, 0.375)
frmt(numbers)
#> [1] "0.12"     "0.10"     "1,234.43" "0.36"     "0.38"    

# reporting a percentage
frmt(17/19 * 100, digits = 2, append = "%")   # good for markdown
#> [1] "89.47%"
frmt(17/19 * 100, digits = 2, append = "\\%") # good for LaTeX
#> [1] "89.47\\%"

# append one character
frmt(c(1, 2, 3)/19 * 100, digits = 2, append = "%")
#> [1] "5.26%"  "10.53%" "15.79%"

# append different characters
frmt(c(1, 2, 3)/19 * 100, digits = 2, append = c("%;", "%!", "%."))
#> [1] "5.26%;"  "10.53%!" "15.79%."

### Formatting p-values
ps <- c(0.2, 0.001, 0.00092, 0.047, 0.034781, 0.0000872, 0.787, 0.05, 0.043)
# LaTeX is the default markup language
cbind("raw"      = ps,
      "default"  = frmtp(ps),
      "3lower"   = frmtp(ps, digits = 3, case = "lower"),
      "PediDent" = frmtp(ps, style = "pediatric_dentistry"))
#>       raw        default        3lower        PediDent    
#>  [1,] "0.2"      "$P = 0.2000$" "$p = 0.200$" "$P = .20$" 
#>  [2,] "0.001"    "$P = 0.0010$" "$p = 0.001$" "$P = .001$"
#>  [3,] "0.00092"  "$P = 0.0009$" "$p < 0.001$" "$P < .001$"
#>  [4,] "0.047"    "$P = 0.0470$" "$p = 0.047$" "$P = .047$"
#>  [5,] "0.034781" "$P = 0.0348$" "$p = 0.035$" "$P = .03$" 
#>  [6,] "8.72e-05" "$P < 0.0001$" "$p < 0.001$" "$P < .001$"
#>  [7,] "0.787"    "$P = 0.7870$" "$p = 0.787$" "$P = .79$" 
#>  [8,] "0.05"     "$P = 0.0500$" "$p = 0.050$" "$P = .05$" 
#>  [9,] "0.043"    "$P = 0.0430$" "$p = 0.043$" "$P = .04$" 

### Using markdown
cbind("raw"      = ps,
      "default"  = frmtp(ps, markup = "markdown"),
      "3lower"   = frmtp(ps, digits = 3, case = "lower", markup = "markdown"),
      "PediDent" = frmtp(ps, style = "pediatric_dentistry", markup = "markdown"))
#>       raw        default        3lower        PediDent    
#>  [1,] "0.2"      "*P* = 0.2000" "*p* = 0.200" "*P* = .20" 
#>  [2,] "0.001"    "*P* = 0.0010" "*p* = 0.001" "*P* = .001"
#>  [3,] "0.00092"  "*P* = 0.0009" "*p* < 0.001" "*P* < .001"
#>  [4,] "0.047"    "*P* = 0.0470" "*p* = 0.047" "*P* = .047"
#>  [5,] "0.034781" "*P* = 0.0348" "*p* = 0.035" "*P* = .03" 
#>  [6,] "8.72e-05" "*P* < 0.0001" "*p* < 0.001" "*P* < .001"
#>  [7,] "0.787"    "*P* = 0.7870" "*p* = 0.787" "*P* = .79" 
#>  [8,] "0.05"     "*P* = 0.0500" "*p* = 0.050" "*P* = .05" 
#>  [9,] "0.043"    "*P* = 0.0430" "*p* = 0.043" "*P* = .04" 

# Formatting the point estimate and confidence interval
# for a set of three values
temp <- c(a = 1.23, b = .32, CC = 1.78)
frmtci(temp)
#> [1] "1.23 (0.32, 1.78)"

# show level uses getOption("qwraps2_alpha", 0.05)
frmtci(temp, show_level = TRUE)
#> [1] "1.23 (95% CI: 0.32, 1.78)"

# note that the show_level will be ignored in the following
frmtci(temp, format = "est ***lcl, ucl***", show_level = TRUE)
#> [1] "1.23 ***0.32, 1.78***"

# show_level as a character
frmtci(temp, show_level = "confidence between: ")
#> [1] "1.23 (confidence between: 0.32, 1.78)"

# For a matrix: the numbers in this example don't mean anything, but the
# formatting should.
temp2 <- matrix(rnorm(12), nrow = 4,
                dimnames = list(c("A", "B", "C", "D"), c("EST", "LOW", "HIGH")))
temp2
#>            EST        LOW       HIGH
#> A -1.400043517  0.6215527 -0.2441996
#> B  0.255317055  1.1484116 -0.2827054
#> C -2.437263611 -1.8218177 -0.5536994
#> D -0.005571287 -0.2473253  0.6289820
frmtci(temp2)
#>                      A                      B                      C 
#>  "-1.40 (0.62, -0.24)"   "0.26 (1.15, -0.28)" "-2.44 (-1.82, -0.55)" 
#>                      D 
#>  "-0.01 (-0.25, 0.63)" 

# similar for a data.frame
df2 <- as.data.frame(temp2)
frmtci(df2)
#>                      A                      B                      C 
#>  "-1.40 (0.62, -0.24)"   "0.26 (1.15, -0.28)" "-2.44 (-1.82, -0.55)" 
#>                      D 
#>  "-0.01 (-0.25, 0.63)" 
```
