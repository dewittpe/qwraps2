library(qwraps2)

# verify the return is TRUE for a set of installed packages
stopifnot(pkg_check(c("qwraps2", "ggplot2")))

# verify a return of FALSE if at least one of the packages is not installed
stopifnot(pkg_check(c("qwraps2", "NOT A PKG")) == FALSE)

# verify FALSE is installed package is < provided version
stopifnot(pkg_check(c("qwraps2"), "999.999.999") == FALSE)

# Expect an error to be thrown with the `stop` arguement is used.
test <- tryCatch(pkg_check(c("qwraps2", "NOT A PKG"), stop = TRUE), error = function(e) e)
stopifnot(inherits(test, 'error'))

