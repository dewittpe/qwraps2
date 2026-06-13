library(qwraps2)

# error is non-survfit object is passed to qkmplot_bulid_data_frame
fit <- lm(mpg ~ wt, data = mtcars)
test <- tryCatch(qkmplot_bulid_data_frame(fit), error = function(e) e)
stopifnot(inherits(test, "error"))

# intercept-only survfit objects should work with qrmst
fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = survival::aml)
out <- qrmst(fit)
stopifnot(inherits(out, "data.frame"))
stopifnot(identical(nrow(out), 1L))
stopifnot(identical(out$strata, "All"))
stopifnot(is.finite(out$rmst))
