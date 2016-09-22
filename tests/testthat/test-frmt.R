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

library(qwraps2)

test_that("LaTeX frmtp", 
          {
            ps <- c(0.2, 0.001, 0.00092, 0.047, 0.034781, 0.0000872, 0.787, 0.05, 0.043)
            # LaTeX is the default markup language
            out <- cbind("raw"      = ps, 
                         "default"  = frmtp(ps), 
                         "3lower"   = frmtp(ps, digits = 3, case = "lower"),
                         "PediDent" = frmtp(ps, style = "pediatric_dentistry"))

            expected <- structure(c("0.2", "0.001", "0.00092", "0.047", "0.034781", "8.72e-05", "0.787", "0.05", "0.043", "$P = 0.2000$", "$P = 0.0010$", "$P = 0.0009$", "$P = 0.0470$", "$P = 0.0348$", "$P < 0.0001$", "$P = 0.7870$", "$P = 0.0500$", "$P = 0.0430$", "$p = 0.200$", "$p = 0.001$", "$p < 0.001$", "$p = 0.047$", "$p = 0.035$", "$p < 0.001$", "$p = 0.787$", "$p = 0.050$", "$p = 0.043$", "$P = .20$", "$P = .001$", "$P < .001$", "$P = .047$", "$P = .03$", "$P < .001$", "$P = .79$", "$P = .05$", "$P = .04$"), .Dim = c(9L, 4L), .Dimnames = list(NULL, c("raw", "default", "3lower", "PediDent")))
            
            expect_equal(out, expected)
          })

test_that("markdown frmtp", 
          {
            ps <- c(0.2, 0.001, 0.00092, 0.047, 0.034781, 0.0000872, 0.787, 0.05, 0.043)
            # LaTeX is the default markup language
            out <- cbind("raw"      = ps, 
                         "default"  = frmtp(ps, markup = "markdown"), 
                         "3lower"   = frmtp(ps, digits = 3, case = "lower", markup = "markdown"),
                         "PediDent" = frmtp(ps, style = "pediatric_dentistry", markup = "markdown"))

            expected <- structure(c("0.2", "0.001", "0.00092", "0.047", "0.034781", "8.72e-05", "0.787", "0.05", "0.043", "*P* = 0.2000", "*P* = 0.0010", "*P* = 0.0009", "*P* = 0.0470", "*P* = 0.0348", "*P* < 0.0001", "*P* = 0.7870", "*P* = 0.0500", "*P* = 0.0430", "*p* = 0.200", "*p* = 0.001", "*p* < 0.001", "*p* = 0.047", "*p* = 0.035", "*p* < 0.001", "*p* = 0.787", "*p* = 0.050", "*p* = 0.043", "*P* = .20", "*P* = .001", "*P* < .001", "*P* = .047", "*P* = .03", "*P* < .001", "*P* = .79", "*P* = .05", "*P* = .04"), .Dim = c(9L, 4L), .Dimnames = list(NULL, c("raw", "default", "3lower", "PediDent")))
            
            expect_equal(out, expected)
          })

