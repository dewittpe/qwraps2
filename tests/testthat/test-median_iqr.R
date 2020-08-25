test_that("basic formatting",
          {
            expect_equal(median_iqr(mtcars$mpg), "19.20 (15.43, 22.80)")
          })

test_that("warning given if show_n is not as expected",
          {
            expect_warning(median_iqr(mtcars$mpg, show_n = TRUE))
          })

test_that("missing value will result in an error",
          {
            x <- c(1, NA, 2:10)
            expect_error(median_iqr(x))
          })
