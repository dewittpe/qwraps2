test_that("basic formatting",
          {
            g_mean <- frmt(exp(mean(log(mtcars$mpg))))
            g_sd   <- frmt(exp(sqrt( var(log(mtcars$mpg)) * (nrow(mtcars) - 1) / nrow(mtcars))))
            expect_equal(gmean_sd(mtcars$mpg), paste(g_mean, "$\\pm$", g_sd))
          })

test_that("warning given if show_n is not as expected",
          {
            expect_warning(gmean_sd(mtcars$mpg, show_n = TRUE))
          })

