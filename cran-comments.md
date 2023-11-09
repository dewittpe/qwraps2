# Version 0.6.0
- Initial submission 8 Nov 2023

## Testing Environments

* Github Actions:
  * macOS R-4.3.2
  * windows R-4.3.2
  * ubuntu R-4.3.2
  * ubuntu R-devel
  * ubuntu R-4.2.3

* win-builder.r-project.org

* Local (macOS Monterey 12.6)
  * R 4.3.2

## R CMD Check results

* Github actions:

    Status OK

* win-builder.r-project.org

  * R Under development (unstable) (2023-11-08 r85496 ucrt):  Status OK
  * R version 4.2.3 (2023-03-15 ucrt): Status OK

* Local

    Status OK

## Reverse dependencies

    None

### Reverse suggests:

* ensr

    No issues

* REDCapExporter

    No issues

* pedalfast.data

    - there is an error in the building of the fss vignette as a result of the
      chagnes to qwraps2.  I am the author of pedalfast.data and I have already
      patched this error and will submit the updated pedalfast.data package to
      CRAN as soon as qwraps2 has been accepted and released on CRAN.
