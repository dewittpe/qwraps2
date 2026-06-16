# File and Working Directory Check

This check is three-fold: 1) verify the current working directory is as
expected, 2) verify the user can access the file, and 3) verify the file
contents are as expected (via md5sum).

## Usage

``` r
file_check(
  paths,
  md5sums = NULL,
  absolute_paths = c("warn", "stop", "silent"),
  stop = FALSE
)
```

## Arguments

- paths:

  a character path to the target file

- md5sums:

  a character string for the expected md5sum of the target file. If
  `NULL` then only a `file.exists` check will be done.

- absolute_paths:

  a character string to set the behavior of warning (default), stopping,
  or silent if/when absolute file paths are used.

- stop:

  if `TRUE` then an error is thrown if any of the checks fail, else
  returns a logical. If `FALSE` (default) a logical is returned.

## Value

The function will return a single TRUE/FALSE value with attributes
`attr(, "checks")`.

## Details

The test for the file access is done to verify the file can be read by
the current user.

The return of the function is `TRUE` if all the files in `paths` are
accessible, are case matched (optional), and all of requested md5sum
checks pass. Windows and macOS are generally case-insensitive systems,
but many Linux systems are case-sensitive. As such
[`file.exists`](https://rdrr.io/r/base/files.html) and
[`file.access`](https://rdrr.io/r/base/file.access.html) may return
different values depending the OS that is active. `file_check` looks for
a case match as part of its checks to hopefully prevent issues across
operating systems.

By default, if the return is `TRUE` then only `TRUE` will be printed to
the console. If the return is `FALSE` then the `attr(, "checks")` is
printed by default as well.

Good practice would be to use relative paths, a warning will be given if
any of the `paths` are determined to be absolute paths. That said, there
are cases when an absolute path is needed, e.g., a common data file on a
server with multiple users accessing the file(s). Set
`absolute_paths = c("silent")` to silence the warnings.

## Examples

``` r
# create example files

relative_example_file1 <-
  basename(
    tempfile(
      pattern = "QWRAPS2_EXAMPLE_1"
      , fileext = ".txt"
      , tmpdir = getwd()
    )
  )

relative_example_file2 <-
  basename(
    tempfile(
      pattern = "QWRAPS2_EXAMPLE_2"
      , fileext = ".txt"
      , tmpdir = getwd()
    )
  )

absolute_example_file <- tempfile()

cat("example file.", file = relative_example_file1)
cat("Another example file.", file = relative_example_file2)
cat("Another example file.", file = absolute_example_file)

# Check that you have access to the files in the working directory.
test1 <- file_check(c(relative_example_file1, relative_example_file2))
test1
#> [1] TRUE

# By default, when the checks return TRUE the details of the checks are not
# printed.  You can view the details of the checks as follows:
attr(test1, "checks")
#>                                                                path
#> QWRAPS2_EXAMPLE_11a207760e830.txt QWRAPS2_EXAMPLE_11a207760e830.txt
#> QWRAPS2_EXAMPLE_21a2072f6b3b2.txt QWRAPS2_EXAMPLE_21a2072f6b3b2.txt
#>                                   absolute_path accessible case_match
#> QWRAPS2_EXAMPLE_11a207760e830.txt         FALSE       TRUE       TRUE
#> QWRAPS2_EXAMPLE_21a2072f6b3b2.txt         FALSE       TRUE       TRUE
#>                                                     current_md5sum
#> QWRAPS2_EXAMPLE_11a207760e830.txt 7a3409e17f9de067740e64448a86e708
#> QWRAPS2_EXAMPLE_21a2072f6b3b2.txt 798e52b92e0ae0e60f3f3db1273235d0
#>                                   expected_md5sum md5check
#> QWRAPS2_EXAMPLE_11a207760e830.txt            <NA>       NA
#> QWRAPS2_EXAMPLE_21a2072f6b3b2.txt            <NA>       NA

# access to absolute_example_file will generate a warning about
# absolute_paths by default
test2 <- file_check(absolute_example_file)
#> Warning: Absolute path used. It would be preferable if you used a relative path.
test2 <- file_check(absolute_example_file, absolute_paths = "silent")
test2
#> [1] TRUE

# Case Match
test_case_match <-
  file_check(
    c(relative_example_file1, tolower(relative_example_file1))
  )
test_case_match
#> [1] FALSE
#> attr(,"checks")
#>                                                                path
#> QWRAPS2_EXAMPLE_11a207760e830.txt QWRAPS2_EXAMPLE_11a207760e830.txt
#> qwraps2_example_11a207760e830.txt qwraps2_example_11a207760e830.txt
#>                                   absolute_path accessible case_match
#> QWRAPS2_EXAMPLE_11a207760e830.txt         FALSE       TRUE       TRUE
#> qwraps2_example_11a207760e830.txt         FALSE      FALSE      FALSE
#>                                                     current_md5sum
#> QWRAPS2_EXAMPLE_11a207760e830.txt 7a3409e17f9de067740e64448a86e708
#> qwraps2_example_11a207760e830.txt                             <NA>
#>                                   expected_md5sum md5check
#> QWRAPS2_EXAMPLE_11a207760e830.txt            <NA>       NA
#> qwraps2_example_11a207760e830.txt            <NA>       NA

# If one or more files is not accessible then return is FALSE and the metadata
# is printed by default.
test_non_existent_file <-
  file_check(
    c("UNLIKELYFILENAME", relative_example_file1, relative_example_file2)
  )
test_non_existent_file
#> [1] FALSE
#> attr(,"checks")
#>                                                                path
#> UNLIKELYFILENAME                                   UNLIKELYFILENAME
#> QWRAPS2_EXAMPLE_11a207760e830.txt QWRAPS2_EXAMPLE_11a207760e830.txt
#> QWRAPS2_EXAMPLE_21a2072f6b3b2.txt QWRAPS2_EXAMPLE_21a2072f6b3b2.txt
#>                                   absolute_path accessible case_match
#> UNLIKELYFILENAME                          FALSE      FALSE      FALSE
#> QWRAPS2_EXAMPLE_11a207760e830.txt         FALSE       TRUE       TRUE
#> QWRAPS2_EXAMPLE_21a2072f6b3b2.txt         FALSE       TRUE       TRUE
#>                                                     current_md5sum
#> UNLIKELYFILENAME                                              <NA>
#> QWRAPS2_EXAMPLE_11a207760e830.txt 7a3409e17f9de067740e64448a86e708
#> QWRAPS2_EXAMPLE_21a2072f6b3b2.txt 798e52b92e0ae0e60f3f3db1273235d0
#>                                   expected_md5sum md5check
#> UNLIKELYFILENAME                             <NA>       NA
#> QWRAPS2_EXAMPLE_11a207760e830.txt            <NA>       NA
#> QWRAPS2_EXAMPLE_21a2072f6b3b2.txt            <NA>       NA

# Or have an error thrown:
if (FALSE) { # \dontrun{
file_check(
  c("UNLIKELYFILENAME", relative_example_file1, relative_example_file2)
, stop = TRUE
)
} # }

# Verify the md5sums as well as file access:
md5_check1 <- file_check(relative_example_file1, "7a3409e17f9de067740e64448a86e708")
md5_check1
#> [1] TRUE

# If you only need to verify a subset of md5sums then use an NA in the md5sums
# argument:
md5_check2 <-
  file_check(c(relative_example_file1, relative_example_file2),
             c("7a3409e17f9de067740e64448a86e708", NA))
md5_check2
#> [1] TRUE

# Verify all the md5sums
md5_check3 <-
  file_check(c(relative_example_file1, relative_example_file2),
             c("7a3409e17f9de067740e64448a86e708", "798e52b92e0ae0e60f3f3db1273235d0"))
md5_check3
#> [1] TRUE

# clean up working directory
unlink(relative_example_file1)
unlink(relative_example_file2)
unlink(absolute_example_file)
```
