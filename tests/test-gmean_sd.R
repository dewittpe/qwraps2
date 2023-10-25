library(qwraps2)

# basic formatting
g_mean <- exp(mean(log(mtcars$mpg)))
g_var  <- exp( var(log(mtcars$mpg)) * (nrow(mtcars) - 1) / nrow(mtcars))
g_sd   <- exp(sqrt( var(log(mtcars$mpg)) * (nrow(mtcars) - 1) / nrow(mtcars)))

stopifnot(identical(gmean(mtcars$mpg), g_mean))
stopifnot(identical(gvar(mtcars$mpg), g_var))
stopifnot(identical(gsd(mtcars$mpg), g_sd))

g_mean <- frmt(g_mean)
g_sd   <- frmt(g_sd)
stopifnot(identical(gmean_sd(mtcars$mpg), paste(g_mean, "$\\pm$", g_sd)))

# error given if show_n is not as expected
stopifnot(inherits(tryCatch(gmean_sd(mtcars$mpg, show_n = TRUE), error = function(e) e), "error"))


# warning if na.rm is used
stopifnot(inherits(tryCatch(gmean_sd(mtcars$mpg, na.rm = TRUE), warning = function(w) w), "warning"))

# warning if na_rm is provided along with na.rm
stopifnot(inherits(tryCatch(gmean_sd(mtcars$mpg, na_rm = TRUE, na.rm = TRUE), warning = function(w) w), "warning"))

