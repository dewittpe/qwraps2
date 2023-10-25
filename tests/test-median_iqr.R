library(qwraps2)

# basic formatting
stopifnot(identical(median_iqr(mtcars$mpg), "19.20 (15.43, 22.80)"))

# warning given if show_n is not as expected
stopifnot(inherits(tryCatch(median_iqr(mtcars$mpg, show_n = TRUE), error = function(e) e), "error"))

# show n with no missing data
mpg <- mtcars$mpg
stopifnot(identical(median_iqr(mpg, show_n = "always"), paste0(nrow(mtcars), "; 19.20 (15.43, 22.80)")))
stopifnot(identical(median_iqr(mpg, show_n = "ifNA"), "19.20 (15.43, 22.80)"))
stopifnot(identical(median_iqr(mpg, show_n = "never"), "19.20 (15.43, 22.80)"))

# show n with missing data
mpg[c(3, 12)] <- NA_real_

stopifnot(identical(median_iqr(mpg, show_n = "always", na_rm = TRUE), paste0(nrow(mtcars) - 2, "; 19.20 (15.27, 22.48)")))
stopifnot(identical(median_iqr(mpg, show_n = "ifNA", na_rm = TRUE), "30; 19.20 (15.27, 22.48)"))
stopifnot(identical(median_iqr(mpg, show_n = "never", na_rm = TRUE), "19.20 (15.27, 22.48)"))

# non logical na_rm errors
x <- c(1, NA, 2:10)
stopifnot(inherits(tryCatch(median_iqr(x, na_rm = "Yes"), error = function(e) e, warning = function(w) w), "error"))
stopifnot(inherits(tryCatch(median_iqr(x, na.rm = "Yes"), error = function(e) e, warning = function(w) w), "warning"))
stopifnot(inherits(tryCatch(suppressWarnings(median_iqr(x, na.rm = "Yes")), error = function(e) e), "error"))

# missing value will result in an error
x <- c(1, NA, 2:10)
stopifnot(inherits(tryCatch(median_iqr(x), error = function(e) e, warning = function(w) w), "error"))

