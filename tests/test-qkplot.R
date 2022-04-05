library(qwraps2)

# error is non-survfit object is passed to qkmplot_bulid_data_frame
fit <- lm(mpg ~ wt, data = mtcars)
test <- tryCatch(qkmplot_bulid_data_frame(fit), error = function(e) e)
stopifnot(inherits(test, "error"))
