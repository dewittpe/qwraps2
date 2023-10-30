library(qwraps2)

stopifnot(identical(traprule(x = c(0, 0, 1, 1), y = c(0, 1, 1, 0)), 1.0))
stopifnot(identical(traprule(x = c(0, 0, 1, 1), y = c(0, 2, 1, 0)), 1.5))

two_to_the_x <- function(x) {
  2 ** x
}

x <- seq(-1, 3, by = 1)
stopifnot(identical(traprule(x, two_to_the_x(x)), 11.25))

x <- seq(-1, 3, length = 100)
stopifnot(identical(sprintf("%.16f", traprule(x, two_to_the_x(x))), "10.8209200182984446"))

# error if inputs are of unequal length
x <- tryCatch(traprule(c(1, 2, 3, 4), c(2, 3)), error = function(e) {e})
stopifnot(!is.null(x))
stopifnot(inherits(x, "error"))

# error if inputs are of length < 2
x <- tryCatch(traprule(c(1), c(2)), error = function(e) {e})
stopifnot(!is.null(x))
stopifnot(inherits(x, "error"))
