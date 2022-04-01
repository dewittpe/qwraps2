library(qwraps2)

# basic formatting
stopifnot(identical(mean_sd(mtcars$mpg), "20.09 $\\pm$ 6.03"))

# error if show_n is not as expected
stopifnot(inherits(tryCatch(mean_sd(mtcars$mpg, show_n = TRUE), error = function(e) e), "error"))

