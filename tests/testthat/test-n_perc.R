test_that("basic formatting", 
          {
            expect_equal(n_perc(mtcars$cyl == 6),  "7 (21.88\\%)")
            expect_equal(n_perc0(mtcars$cyl == 6), "7 (22)")
            expect_equal(perc_n(mtcars$cyl == 6), paste0("21.88\\% (n = ", nrow(mtcars), ")"))
          })

