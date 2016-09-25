test_that("basic formatting", 
          {
            expect_equal(n_perc(mtcars$cyl == 6),  "7 (21.88\\%)")
            expect_equal(n_perc0(mtcars$cyl == 6), "7 (22)")
          })

