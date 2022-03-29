test_that("basic formatting",
          {
            expect_equal(median_iqr(mtcars$mpg), "19.20 (15.43, 22.80)")
          })

test_that("warning given if show_n is not as expected",
          {
            expect_warning(median_iqr(mtcars$mpg, show_n = TRUE))
          })

test_that("show n with no missing data",
          {
            mpg <- mtcars$mpg
            expect_equal(median_iqr(mpg, show_n = "always"), paste0(nrow(mtcars), "; 19.20 (15.43, 22.80)"))
            expect_equal(median_iqr(mpg, show_n = "ifNA"), "19.20 (15.43, 22.80)")
            expect_equal(median_iqr(mpg, show_n = "never"), "19.20 (15.43, 22.80)")
          })

test_that("show n with missing data",
          {
            mpg <- mtcars$mpg
            mpg[c(3, 12)] <- NA_real_

            expect_equal(median_iqr(mpg, show_n = "always", na_rm = TRUE), paste0(nrow(mtcars) - 2, "; 19.20 (15.27, 22.48)"))
            expect_equal(median_iqr(mpg, show_n = "ifNA", na_rm = TRUE), "30; 19.20 (15.27, 22.48)")
            expect_equal(median_iqr(mpg, show_n = "never", na_rm = TRUE), "19.20 (15.27, 22.48)")
          })

test_that("non logical na_rm errors",
          {
            x <- c(1, NA, 2:10)
            expect_error(median_iqr(x, na_rm = "Yes"))
            expect_error(median_iqr(x, na.rm = "Yes"))
            expect_warning(try(median_iqr(x, na.rm = "Yes"), silent = TRUE))
          })

test_that("missing value will result in an error",
          {
            x <- c(1, NA, 2:10)
            expect_error(median_iqr(x))
          })
