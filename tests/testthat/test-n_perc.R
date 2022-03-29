test_that("basic formatting",
          {
            expect_equal(n_perc(mtcars$cyl == 6),  "7 (21.88\\%)")
            expect_equal(n_perc0(mtcars$cyl == 6), "7 (22)")
            expect_equal(perc_n(mtcars$cyl == 6), paste0("21.88\\% (n = ", nrow(mtcars), ")"))
          })

test_that("errors with bad arguments",
          {
            expect_error(n_perc(mtcars$cyl))
            expect_error(n_perc(mtcars$cyl == 6, na_rm = "please"))
            expect_error(n_perc(mtcars$cyl == 6, show_denom = "please"))
            expect_error(n_perc(mtcars$cyl == 6, show_symbol = "please"))
            expect_error(n_perc(mtcars$cyl == 6, markup = "please"))

            expect_error(n_perc0(mtcars$cyl))
            expect_error(n_perc0(mtcars$cyl == 6, na_rm = "please"))
            expect_error(n_perc0(mtcars$cyl == 6, show_denom = "please"))
            expect_error(n_perc0(mtcars$cyl == 6, show_symbol = "please"))
            expect_error(n_perc0(mtcars$cyl == 6, markup = "please"))

            expect_error(perc_n(mtcars$cyl))
            expect_error(perc_n(mtcars$cyl == 6, na_rm = "please"))
            expect_error(perc_n(mtcars$cyl == 6, show_denom = "please"))
            expect_error(perc_n(mtcars$cyl == 6, show_symbol = "please"))
            expect_error(perc_n(mtcars$cyl == 6, markup = "please"))
          })

test_that("warning if na.rm used instead of na_rm",
          {
            expect_warning(n_perc(mtcars$cyl == 6, na.rm = TRUE))
            expect_warning(perc_n(mtcars$cyl == 6, na.rm = TRUE))
            expect_warning(n_perc0(mtcars$cyl == 6, na.rm = TRUE))
          })

test_that("with missing data",
          {
            x <- mtcars$cyl == 6
            x[c(3, 13)] <- NA

            expect_equal(n_perc(x), "NA/30 ( NA\\%)")
            expect_equal(n_perc0(x), "NA (NA)")
            expect_equal(perc_n(x), " NA\\% (n = 30 non-missing)")

            expect_equal(n_perc(x, na_rm = TRUE), "7/30 (23.33\\%)")
            expect_equal(n_perc0(x, na_rm = TRUE), "7 (23)")
            expect_equal(perc_n(x, na_rm = TRUE), "23.33\\% (n = 30 non-missing)")

          })

test_that("show denom with no missing data",
          {
            x <- mtcars$cyl == 6

            expect_equal(n_perc(mtcars$cyl == 6, show_denom = "always"),  "7/32 (21.88\\%)")
            expect_equal(n_perc0(mtcars$cyl == 6, show_denom = "always"), "7/32 (22)")
            expect_equal(perc_n(mtcars$cyl == 6, show_denom = "always"), paste0("21.88\\% (n = ", nrow(mtcars), " non-missing)"))

          })
