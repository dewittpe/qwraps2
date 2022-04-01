test_that("AUC is as expected", 
          {
            data(diamonds, package = "ggplot2") 
            fit1 <- glm(I(price > 2800) ~ cut * color, data = diamonds, family = binomial())
            data1 <- qroc_build_data_frame(fit1)
            expect_equal(round(auc(data1), 6), 0.626816)
          })
