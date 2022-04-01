library(qwraps2)

# test that a warning is thrown when there are missmatches
test <-
  tryCatch(
           spin_comments(hair = system.file("examples/spinner1.R", package = "qwraps2"))
           , error   = function(e) e , warning = function(w) w)
stopifnot(inherits(test, "warning"))

# verify return is FALSE
x <- suppressWarnings(spin_comments(hair = system.file("examples/spinner1.R", package = "qwraps2")))
stopifnot(isFALSE(x))
x_notes <- attr(x, "notes")
stopifnot(typeof(x_notes) == "character")
stopifnot(identical(x_notes[1], "  * started on line 5; ended on line 7"))
stopifnot(identical(x_notes[2], "  * no starting delimiter; ended on line 15"))
stopifnot(identical(x_notes[3], "  * started on line 22; ended on line 24"))
stopifnot(identical(x_notes[4], "  * started on line 20; no end delimiter"))

# verify no warning when there are no missmatches
test <-
  tryCatch(
           spin_comments(hair = system.file("examples/spinner2.R", package = "qwraps2"))
           , error   = function(e) e , warning = function(w) w)
stopifnot(!inherits(test, "warning"))

# verify true
x <- spin_comments(hair = system.file("examples/spinner2.R", package = "qwraps2"))
stopifnot(x)

x_notes <- attr(x, "notes")
stopifnot(typeof(x_notes) == "character")
stopifnot(identical(x_notes[1], "  * started on line 2; ended on line 6"))
stopifnot(identical(x_notes[2], "  * started on line 9; ended on line 11"))
stopifnot(identical(x_notes[3], "  * started on line 14; ended on line 14"))
stopifnot(identical(x_notes[4], "  * started on line 18; ended on line 18"))


