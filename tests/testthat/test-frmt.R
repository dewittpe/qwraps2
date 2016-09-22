test_that("frmt", 
          {
            expect_identical(frmt(pi), "3.14")
            expect_identical(frmt(pi, 1), "3.1")
            expect_identical(frmt(pi, 3), "3.142")
            expect_identical(frmt(pi, 4), "3.1416")
            expect_identical(frmt(pi, 5), "3.14159")

            expect_identical(frmt(4), "4.00")
            expect_identical(frmt(4L), "4")
          })

