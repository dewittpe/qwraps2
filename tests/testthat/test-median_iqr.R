test_that("basic formatting", 
          {
            expect_equal(median_iqr(mtcars$mpg), "19.20 (15.43, 22.80)")
          })
