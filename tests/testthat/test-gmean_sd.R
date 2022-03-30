test_that("basic formatting",
          {
            g_mean <- exp(mean(log(mtcars$mpg)))
            g_var  <- exp( var(log(mtcars$mpg)) * (nrow(mtcars) - 1) / nrow(mtcars))
            g_sd   <- exp(sqrt( var(log(mtcars$mpg)) * (nrow(mtcars) - 1) / nrow(mtcars)))
            expect_equal(gmean(mtcars$mpg), g_mean)
            expect_equal(gvar(mtcars$mpg), g_var)
            expect_equal(gsd(mtcars$mpg), g_sd)

            g_mean <- frmt(g_mean)
            g_sd   <- frmt(g_sd)
            expect_equal(gmean_sd(mtcars$mpg), paste(g_mean, "$\\pm$", g_sd))
          })

test_that("warning given if show_n is not as expected",
          {
            expect_warning(gmean_sd(mtcars$mpg, show_n = TRUE))
          })

