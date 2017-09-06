xvec <- seq(-2 * pi, 3 * pi, length = 560)
foo  <- function(x) { sin(x) + x * cos(x) + 12 }
yvec <- foo(xvec)
plot(xvec, yvec, type = "l")

integrate(f = foo, lower = -2 * pi, upper = 3 * pi)
traprule(xvec, yvec)
