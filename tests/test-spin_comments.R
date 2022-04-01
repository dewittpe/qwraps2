test_that("you can identify miss matched comments in spin documents",
          {
            expect_warning(spin_comments(hair = system.file("examples/spinner1.R", package = "qwraps2")))

            x <- suppressWarnings(spin_comments(hair = system.file("examples/spinner1.R", package = "qwraps2")))

            expect_false(x)

            x <- attr(x, "notes")

            expect_type(x, "character")
            expect_equal(x[1], "  * started on line 5; ended on line 7")
            expect_equal(x[2], "  * no starting delimiter; ended on line 15")
            expect_equal(x[3], "  * started on line 22; ended on line 24")
            expect_equal(x[4], "  * started on line 20; no end delimiter")

            expect_true(spin_comments(hair = system.file("examples/spinner2.R", package = "qwraps2")))

          })
