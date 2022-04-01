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
