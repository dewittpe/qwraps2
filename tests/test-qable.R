library(qwraps2)

output_latex <- capture.output(print(qable(mtcars2)))
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

