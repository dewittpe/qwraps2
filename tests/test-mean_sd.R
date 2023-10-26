library(qwraps2)

m <- mean(mtcars$mpg)
s <- sd(mtcars$mpg)

# basic formatting
stopifnot(
  identical(
    mean_sd(mtcars$mpg) # should default to LaTeX markup
    ,
    qwraps2::frmt(m, digits = 2) %s% " $\\pm$ " %s% qwraps2::frmt(s, digits = 2)
  )
)

stopifnot(
  identical(
    mean_sd(mtcars$mpg, denote_sd = "paren")
    ,
    qwraps2::frmt(m, digits = 2) %s% " (" %s% qwraps2::frmt(s, digits = 2) %s% ")"
  )
)

stopifnot(
  identical(
    mean_sd(mtcars$mpg, markup = "markdown")
    ,
    qwraps2::frmt(m, digits = 2) %s% " &plusmn; " %s% qwraps2::frmt(s, digits = 2)
  )
)

stopifnot(
  identical(
    mean_sd(mtcars$mpg, show_n = "always", markup = "markdown")
    ,
    qwraps2::frmt(nrow(mtcars)) %s% "; " %s% qwraps2::frmt(m, digits = 2) %s% " &plusmn; " %s% qwraps2::frmt(s, digits = 2)
  )
)

stopifnot(
  identical(
    mean_sd(c(mtcars$mpg,NA), na_rm = TRUE, show_n = "always", markup = "markdown")
    ,
    qwraps2::frmt(nrow(mtcars)) %s% "; " %s% qwraps2::frmt(m, digits = 2) %s% " &plusmn; " %s% qwraps2::frmt(s, digits = 2)
  )
)

stopifnot(
  identical(
    mean_sd(c(mtcars$mpg, NA), na_rm = TRUE, show_n = "never", markup = "markdown")
    ,
    qwraps2::frmt(m, digits = 2) %s% " &plusmn; " %s% qwraps2::frmt(s, digits = 2)
  )
)


# error if show_n is not as expected
stopifnot(inherits(tryCatch(mean_sd(mtcars$mpg, show_n = TRUE), error = function(e) e), "error"))

# warning if na.rm is used instead of na_rm
stopifnot(inherits(tryCatch(mean_sd(mtcars$mpg, na.rm = TRUE), warning = function(w) w), "warning"))
