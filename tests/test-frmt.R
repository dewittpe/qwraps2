library(qwraps2)

# Tests for frmt
stopifnot(frmt(pi) == "3.14")
stopifnot(frmt(pi, 1) == "3.1")
stopifnot(frmt(pi, 3) == "3.142")
stopifnot(frmt(pi, 4) == "3.1416")
stopifnot(frmt(pi, 5) == "3.14159")
stopifnot(frmt(4) == "4.00")
stopifnot(frmt(4L) == "4")

