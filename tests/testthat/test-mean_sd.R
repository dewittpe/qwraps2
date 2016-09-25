test_that("basic formatting", 
          {
            expect_equal(mean_sd(mtcars$mpg), "20.09 $\\pm$ 6.03")
          })
