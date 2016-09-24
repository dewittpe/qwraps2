test_that("simple print works",
          {
            expect_output(print(qable(mtcars)), "142E\\ \\&\\ 21\\.4")
            expect_output(print(qable(mtcars, markup = "markdown")), "142E\\ +\\|21\\.4")
          })

test_that("markup language check", 
          {
            expect_error(qable(mtcars, markup = "rts"))
          })

test_that('rgroups', 
          {
            make <- sub("^(\\w+)\\s?(.*)$", "\\1", rownames(mtcars))
            make <- c(table(make))

            expect_output(print(qable(mtcars[sort(rownames(mtcars)), ], rgroup = make)),
                          "\\\\bf\\{Volvo\\}\\ \\&\\ ~")
            expect_output(print(qable(mtcars[sort(rownames(mtcars)), ], rgroup = make)),
                          "~~\\ Volvo\\ 142E\\ \\&\\ 21\\.4") 

            expect_output(print(qable(mtcars[sort(rownames(mtcars)), ], rgroup = make, markup = "markdown")),
                          "\\*\\*Volvo\\*\\*")
            expect_output(print(qable(mtcars[sort(rownames(mtcars)), ], rgroup = make, markup = "markdown")),
                          "\\&nbsp;\\&nbsp;\\ Volvo")
          })

