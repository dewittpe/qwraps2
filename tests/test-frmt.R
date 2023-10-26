library(qwraps2)

################################################################################
##                               Tests for frmt                               ##
stopifnot(frmt(pi) == "3.14")
stopifnot(frmt(pi, 1) == "3.1")
stopifnot(frmt(pi, 3) == "3.142")
stopifnot(frmt(pi, 4) == "3.1416")
stopifnot(frmt(pi, 5) == "3.14159")
stopifnot(frmt(4) == "4.00")
stopifnot(frmt(4L) == "4")

# append one character
stopifnot(
  identical(
    frmt(c(1, 2, 3)/19 * 100, digits = 2, append = "%")
    ,
    c("5.26%", "10.53%", "15.79%")
  )
)

# append different characters
stopifnot(
  identical(
    frmt(c(1, 2, 3)/19 * 100, digits = 2, append = c("a", "b", "c"))
    ,
    c("5.26a", "10.53b", "15.79c")
  )
)

# error if append has length > 1 and different from length(x)
stopifnot(inherits(tryCatch(frmt(c(1, 2, 3)/19 * 100, digits = 2, append = c("a", "b")), error = function(e) e) , "error"))

################################################################################
##                              Tests for frmtci                              ##
temp <- c(a = 1.23, b = .32, CC = 1.78)
stopifnot(identical(frmtci(temp), "1.23 (0.32, 1.78)"))

# show level uses getOption("qwraps2_alpha", 0.05)
stopifnot(identical(frmtci(temp, show_level = TRUE), "1.23 (95% CI: 0.32, 1.78)"))

# note that the show_level will be ignored in the following
stopifnot(identical(frmtci(temp, format = "est ***lcl, ucl***", show_level = FALSE), "1.23 ***0.32, 1.78***"))
stopifnot(identical(frmtci(temp, format = "est ***lcl, ucl***", show_level = TRUE), "1.23 ***0.32, 1.78***"))

# show_level as a character
stopifnot(identical(frmtci(temp, show_level = "confidence between: "), "1.23 (confidence between: 0.32, 1.78)"))

# For a matrix: the numbers in this example don't mean anything, but the
# formatting should.
set.seed(42)
temp2 <- matrix(rnorm(12), nrow = 4, dimnames = list(c("A", "B", "C", "D"), c("EST", "LOW", "HIGH")))
stopifnot(identical(frmtci(temp2), c(A = "1.37 (0.40, 2.02)", B = "-0.56 (-0.11, -0.06)", C = "0.36 (1.51, 1.30)", D = "0.63 (-0.09, 2.29)")))

df2 <- as.data.frame(temp2)
stopifnot(identical(frmtci(df2), c(A = "1.37 (0.40, 2.02)", B = "-0.56 (-0.11, -0.06)", C = "0.36 (1.51, 1.30)", D = "0.63 (-0.09, 2.29)")))

################################################################################
##                    Rpkg formating and urls in markdown                     ##

# Markdown formating of packages and links to them
stopifnot(identical(Rpkg(qwraps2), "*qwraps2*"))
stopifnot(identical(Rpkg("qwraps2"), "*qwraps2*"))

stopifnot(identical(CRANpkg(qwraps2), "*[qwraps2](https://cran.r-project.org/package=qwraps2)*"))
stopifnot(identical(CRANpkg("qwraps2"), "*[qwraps2](https://cran.r-project.org/package=qwraps2)*"))

stopifnot(identical(Githubpkg(qwraps2, "dewittpe"), "*[qwraps2](https://github.com/dewittpe/package=qwraps2)*"))
stopifnot(identical(Githubpkg("qwraps2", dewittpe), "*[qwraps2](https://github.com/dewittpe/package=qwraps2)*"))

stopifnot(identical(Gitlabpkg(qwraps2, "dewittpe"), "*[qwraps2](https://gitlab.com/dewittpe/package=qwraps2)*"))
stopifnot(identical(Gitlabpkg("qwraps2", dewittpe), "*[qwraps2](https://gitlab.com/dewittpe/package=qwraps2)*"))

################################################################################
##                                  backtick                                  ##
stopifnot(identical(backtick("a string"), "`\"a string\"`"))
stopifnot(identical(backtick("a string", dequote = TRUE), "`a string`"))
stopifnot(identical(backtick("\"a string\"", dequote = TRUE), "`\\\"a string\\\"`"))
stopifnot(identical(backtick(noquotestring), "`noquotestring`"))
