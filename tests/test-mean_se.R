library(qwraps2)

m  <- mean(mtcars$mpg)
se <- sd(mtcars$mpg) / sqrt(nrow(mtcars))

# basic formatting
stopifnot(identical(mean_se(mtcars$mpg), qwraps2::frmt(m, digits = 2) %s% " $\\pm$ " %s% qwraps2::frmt(se, digits = 2)))

# error if show_n is not as expected
stopifnot(inherits(tryCatch(mean_se(mtcars$mpg, show_n = TRUE), error = function(e) e), "error"))

