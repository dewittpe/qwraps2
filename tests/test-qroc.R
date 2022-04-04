library(qwraps2)

data(diamonds, package = "ggplot2")
fit1 <- glm(I(price > 2800) ~ cut * color, data = diamonds, family = binomial())
data1 <- qroc_build_data_frame(fit1)
stopifnot(identical(round(auc(data1), 6), 0.626816))


# errors if non-binomial fit is passed to qroc_build_data_frame
fit <- glm(mpg > 20 ~ wt, data = mtcars)
test <- tryCatch(qroc_build_data_frame(fit), error = function(e) e)
stopifnot(inherits(test, "error"))

# this should work
fit <- glm(mpg > 20 ~ wt, data = mtcars, family = binomial())
test <- tryCatch(qroc_build_data_frame(fit))
