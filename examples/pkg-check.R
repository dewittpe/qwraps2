# verify that the packages qwraps2, ggplot2, and BH are available
pkg_check(c("qwraps2", "ggplot2", "BH"))

# show that the return is FALSE if a package is not available
pkg_check(c("qwraps2", "ggplot2", "BH", "NOT a PCKG"))

# verify the version for just ggplot2
pkg_check(c("qwraps2", "ggplot2", "BH"),
          c(NA, "2.2.0", NA))

# verify the version for qwraps2 (this is expected to fail as we are looking for
# version 42.3.14 which is far too advanced for the actual package development.
pkg_check(c("qwraps2", "ggplot2", "BH"),
          c("42.3.14", "2.2.0", NA))

\dontrun{
  # If you have missing packages that can be installed from CRAN you may find
  # the following helpful.  If this code, with the needed edits, were placed at
  # the top of a script, then if a package is missing then the current version
  # from a target repository will be installed.  Use this set up with
  # discretion, others may not want the automatic install of packages.
  library(magrittr)
  pkgs <- pkg_check("<packages to install>")

  if (!pkgs) { 
    pkgs %>%
      attr(., "checks") %>%
      dplyr::filter(!.data$available) %>%
      `[[`(., "package") %>%
      install.packages(., repos = c("<your-favorite-cran-mirror>",
                                    "<another-repository>"))
  }

}

