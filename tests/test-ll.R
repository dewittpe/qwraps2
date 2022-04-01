library(qwraps2)

e <- new.env()
stopifnot(identical(ll(e), character(0)))

e$fit <- lm(mpg ~ wt, mtcars)
e$fit2 <- lm(mpg ~ wt + am + vs, data = mtcars)
e$x <- rnorm(1e5)
e$y <- runif(1e4)
e$z <- with(e, x * y)
e$w <- sum(e$z)
e$vroom <- mtcars
e$foo <- function(x) {2 + x}

stopifnot(identical(rownames(ll(e)), as.character(1:nrow(ll(e)))))
stopifnot(identical(ll(e)$object, c("x", "z", "y", "fit2", "fit", "vroom", "foo", "w")))
stopifnot(identical(ll(e, order_by = "object")$object, c("fit", "fit2", "foo", "vroom", "w", "x", "y", "z")))
stopifnot(identical(ll(e, order_by = "object", decreasing = TRUE)$object, rev(c("fit", "fit2", "foo", "vroom", "w", "x", "y", "z"))))

