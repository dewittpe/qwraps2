library(qwraps2)

set.seed(42)
p <- runif(100)

stopifnot(all.equal(logit(p), log(p/(1-p))))
stopifnot(all.equal(invlogit(log(p/(1-p))), p))

x <- seq(-3, 3, length = 1001)
stopifnot(all.equal(invlogit(x), plogis(x)))

x <- seq(sqrt(.Machine$double.eps),  1 - sqrt(.Machine$double.eps), length = 1001)
stopifnot(all.equal(logit(x), qlogis(x)))
