test_that("basic formatting",
          {
            expect_equal(mean_sd(mtcars$mpg), "20.09 $\\pm$ 6.03")
          })

test_that("warning given if show_n is not as expected",
          {
            expect_warning(mean_sd(mtcars$mpg, show_n = TRUE))
          })
