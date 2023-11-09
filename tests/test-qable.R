library(qwraps2)
set.seed(42)

################################################################################
# testing attributes
x <- qable(mtcars2[, c("mpg", "disp")], markup = "latex")
stopifnot(
  identical(
    attributes(x)
    ,
    list(dim = c(32L, 3L), dimnames = list(NULL, c("", "mpg", "disp")), class = "qwraps2_qable", qable_args = list(rtitle = "", rgroup = numeric(0), rnames = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32"), cnames = c("", "mpg", "disp"), markup = "latex", kable_args = list()))
  )
)

################################################################################
# simple output tests
output_latex <- capture.output(print(qable(mtcars2, markup = "latex")))
output_markdown <- capture.output(print(qable(mtcars2, markup = "markdown")))

# simple print test
stopifnot(any(grepl("142E\\ \\&\\ 21\\.4", output_latex)))
stopifnot(any(grepl("142E\\ +\\|21\\.4", output_markdown)))

# error for unexpected markup language
test <- tryCatch(qable(mtcars2, markup = "rts"), error = function(e) e)
stopifnot(inherits(test, "error"))


# rtitle
out <- capture.output(print(qable(mtcars[1, ], rtitle = "user defined rtitle")))
stopifnot(grepl("^user\\ defined\\ rtitle", out[4]))
out <- capture.output(print(qable(mtcars[1, ], markup = "markdown", rtitle = "user defined rtitle")))
stopifnot(grepl("^\\|user\\ defined\\ rtitle", out[3]))

# rgroups
make <- sub("^(\\w+)\\s?(.*)$", "\\1", rownames(mtcars))
make <- c(table(make))
output <- capture.output(print(qable(mtcars[sort(rownames(mtcars)), ], rgroup = make)))

any(grepl("\\\\bf\\{Volvo\\}\\ \\&\\ ~", output))
any(grepl("~~\\ Volvo\\ 142E\\ \\&\\ 21\\.4", output))

output <- capture.output(print(qable(mtcars[sort(rownames(mtcars)), ], rgroup = make, markup = "markdown")))

stopifnot(any(grepl("\\*\\*\\Volvo\\*\\*", output)))
stopifnot(any(grepl("\\&nbsp;\\&nbsp;\\ Volvo", output)))

################################################################################
# Test cbind
qtab_1 <- qable(mtcars[sort(rownames(mtcars)), c("mpg", "cyl")], rgroup = make, kable_args = list(caption = "1"))
qtab_2 <- qable(mtcars[sort(rownames(mtcars)), c("disp", "hp", "drat")], rgroup = make, kable_args = list(caption = "2"))
qtab_0 <- qable(mtcars[sort(rownames(mtcars)), c("mpg", "cyl", "disp", "hp", "drat")], rgroup = make, kable_args = list(caption = "1"))

# the number of column of each of these tables should be one more than the
# number of columns of the data.frame passed.  the "extra" column is the row
# grouping and names.
stopifnot(identical(ncol(qtab_1), 3L))
stopifnot(identical(ncol(qtab_2), 4L))
stopifnot(identical(ncol(qtab_0), 6L))

# qtab_1 and qtab_2 should join together to be identical to qtab_0
stopifnot(identical(cbind(qtab_1, qtab_2), qtab_0))

# should be able to cbind on a vector of values
out <- cbind(qtab_1, a_number = rnorm(nrow(qtab_1)))
stopifnot(identical(colnames(out), c("", "mpg", "cyl", "a_number")))

# should be able to cbind on a vector of values as a matrix
out <- cbind(qtab_1, a_number = matrix(rnorm(nrow(qtab_1)), ncol = 1))
stopifnot(identical(colnames(out), c("", "mpg", "cyl", "a_number")))

# or even another matrix
out <- cbind(qtab_1, a = matrix(rnorm(3 * nrow(qtab_1)), ncol = 3))
stopifnot(identical(colnames(out), c("", "mpg", "cyl", "a1", "a2", "a3")))

# error if nrows are not the same, or if a vector length is different from the
# nrow of the first element
test <-
  tryCatch(
           cbind(qtab_1, qtab_1_2, a_number = rnorm(nrow(qtab_1) - 1)),
           error = function(e) e)
stopifnot(identical(inherits(test, "simpleError"), TRUE))

test <-
  tryCatch(
           cbind(qtab_1, qtab_1_2[1:3, ]),
           error = function(e) e)
stopifnot(identical(inherits(test, "simpleError"), TRUE))

# there should be an error if the row groups are not identical
m <- make; names(m) <- toupper(names(m))
qtab_1_2 <- qable(mtcars[, c("mpg", "cyl")], rgroup = m)
test <- tryCatch(cbind(qtab_1, qtab_1_2), error = function(e) e)
stopifnot(identical(inherits(test, "simpleError"), TRUE))

qtab_1_3 <- qable(mtcars[, c("mpg", "cyl")], rgroup = sample(make))
test <- tryCatch(cbind(qtab_1, qtab_1_3), error = function(e) e)
stopifnot(identical(inherits(test, "simpleError"), TRUE))

# the attributes of the object from cbind should  be those of the first object
# passed in
stopifnot(identical(attributes(qtab_2)$qable_args$kable_args, list(caption = "2")))
stopifnot(identical(attributes(qtab_1)$qable_args$kable_args, list(caption = "1")))
stopifnot(identical(attributes(cbind(qtab_1, qtab_2))$qable_args$kable_args, list(caption = "1")))
stopifnot(identical(attributes(cbind(qtab_2, qtab_1))$qable_args$kable_args, list(caption = "2")))

################################################################################
# testing rbind
m1 <- mtcars2[grepl("^M", mtcars2$make), ]
m1 <- m1[order(m1$make, m1$model), ]
m2 <- mtcars2[!grepl("^M", mtcars2$make), ]
m2 <- m2[order(m2$make, m2$model), ]

rg1 <- c(table(m1$make))
rg2 <- c(table(m2$make))
m <- rbind(m1, m2)
rg <- c(table(m$make))
rg <- rg[c(names(rg1), names(rg2))]

stopifnot(isTRUE(
  all.equal(
    rbind(qable(m1, rgroup = rg1), qable(m2, rgroup = rg2))
    ,
    qable(m, rgroup = rg)
    )
  )
)

################################################################################
##                                End of File                                 ##
################################################################################
