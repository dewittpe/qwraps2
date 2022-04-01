library(qwraps2)

data(diamonds, package = "ggplot2")
fit1 <- glm(I(price > 2800) ~ cut * color, data = diamonds, family = binomial())
data1 <- qroc_build_data_frame(fit1)
stopifnot(identical(round(auc(data1), 6), 0.626816))

