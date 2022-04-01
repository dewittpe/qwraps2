library(qwraps2)

# basic formatting
stopifnot(identical(n_perc(mtcars2$cyl == 6),  "7 (21.88\\%)"))
stopifnot(identical(n_perc0(mtcars2$cyl == 6), "7 (22)"))
stopifnot(identical(perc_n(mtcars2$cyl == 6), paste0("21.88\\% (n = ", nrow(mtcars2), ")")))

# errors with bad arguments
stopifnot(inherits(tryCatch(n_perc(mtcars2$cyl), error = function(e) e), "error"))
stopifnot(inherits(tryCatch(n_perc(mtcars2$cyl == 6, na_rm = "please"), error = function(e) e), "error"))
stopifnot(inherits(tryCatch(n_perc(mtcars2$cyl == 6, show_denom = "please"), error = function(e) e), "error"))
stopifnot(inherits(tryCatch(n_perc(mtcars2$cyl == 6, show_symbol = "please"), error = function(e) e), "error"))
stopifnot(inherits(tryCatch(n_perc(mtcars2$cyl == 6, markup = "please"), error = function(e) e), "error"))


# warning if na.rm used instead of na_rm
stopifnot(inherits(tryCatch(n_perc(mtcars2$cyl == 6, na.rm = TRUE), warning = function(w) w), "warning"))
stopifnot(inherits(tryCatch(perc_n(mtcars2$cyl == 6, na.rm = TRUE), warning = function(w) w), "warning"))
stopifnot(inherits(tryCatch(n_perc0(mtcars2$cyl == 6, na.rm = TRUE), warning = function(w) w), "warning"))

# with missing data
x <- mtcars2$cyl == 6
x[c(3, 13)] <- NA

stopifnot(identical(n_perc(x), "NA/30 ( NA\\%)"))
stopifnot(identical(n_perc0(x), "NA (NA)"))
stopifnot(identical(perc_n(x), " NA\\% (n = 30 non-missing)"))

stopifnot(identical(n_perc(x, na_rm = TRUE), "7/30 (23.33\\%)"))
stopifnot(identical(n_perc0(x, na_rm = TRUE), "7 (23)"))
stopifnot(identical(perc_n(x, na_rm = TRUE), "23.33\\% (n = 30 non-missing)"))

stopifnot(identical(n_perc(x, na_rm = TRUE, show_denom = "never"), "7 (23.33\\%)"))
stopifnot(identical(n_perc0(x, na_rm = TRUE, show_denom = "never"), "7 (23)"))
stopifnot(identical(perc_n(x, na_rm = TRUE, show_denom = "never"), "23.33\\% (n = 30 non-missing)"))

stopifnot(identical(n_perc(x, na_rm = TRUE, show_denom = "always"), "7/30 (23.33\\%)"))
stopifnot(identical(n_perc0(x, na_rm = TRUE, show_denom = "always"), "7/30 (23)"))
stopifnot(identical(perc_n(x, na_rm = TRUE, show_denom = "always"), "23.33\\% (n = 30 non-missing)"))

stopifnot(identical(n_perc(x, na_rm = TRUE, show_denom = "ifNA"), "7/30 (23.33\\%)"))
stopifnot(identical(n_perc0(x, na_rm = TRUE, show_denom = "ifNA"), "7/30 (23)"))
stopifnot(identical(perc_n(x, na_rm = TRUE, show_denom = "ifNA"), "23.33\\% (n = 30 non-missing)"))

# show denom with no missing data
x <- mtcars2$cyl == 6

stopifnot(identical(n_perc(mtcars2$cyl == 6, show_denom = "always"),  "7/32 (21.88\\%)"))
stopifnot(identical(n_perc0(mtcars2$cyl == 6, show_denom = "always"), "7/32 (22)"))
stopifnot(identical(perc_n(mtcars2$cyl == 6, show_denom = "always"), paste0("21.88\\% (n = ", nrow(mtcars2), " non-missing)")))

