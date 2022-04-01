test_that("simple ci",
          {
            expect_equivalent(unclass(mean_ci(mtcars$mpg)),
                              c(mean(mtcars$mpg),
                                mean(mtcars$mpg) + qnorm(0.025)*sd(mtcars$mpg)/sqrt(nrow(mtcars)),
                                mean(mtcars$mpg) + qnorm(0.975)*sd(mtcars$mpg)/sqrt(nrow(mtcars)))
                              )
          })

test_that("Printing nicely",
          {
            expect_output(print(mean_ci(mtcars$mpg)), "20\\.09 \\(18\\.00, 22\\.18\\)")
            expect_output(print(mean_ci(mtcars$mpg), show_level = TRUE), "20.09 \\(95% CI: 18\\.00, 22.18\\)")
            expect_output(print(mean_ci(mtcars$mpg, alpha = 0.01), show_level = TRUE), "20\\.09 \\(99% CI: 17\\.35, 22\\.83\\)")
          })

test_that("Compare to the ci that comes form t.test",
          {
            expect_equivalent(t.test(mtcars$mpg)$conf.int,
                              mean_ci(mtcars$mpg, qdist = stats::qt, qdist.args = list(df = 31))[2:3])
          })

test_that("simple ci with missing data",
          {
            x <- mtcars$mpg
            x[c(12, 14)] <- NA

            expect_equivalent(unclass(mean_ci(x, na_rm = TRUE)),
                              c(mean(x, na.rm = TRUE),
                                mean(x, na.rm = TRUE) + qnorm(0.025)*sd(x, na.rm = TRUE)/sqrt(sum(!is.na(x))),
                                mean(x, na.rm = TRUE) + qnorm(0.975)*sd(x, na.rm = TRUE)/sqrt(sum(!is.na(x))))
                              )
          })

test_that("warning is thrown",
          {
            expect_warning(mean_ci(mtcars$mpg, na.rm = TRUE))
          })

test_that("error is thrown",
          {
            expect_error(mean_ci(mtcars$mpg, transform = "exp"))
          })
