library(qwraps2)

set_a <- c("A", "a", "E",      "I", "i", "O", "o", "U", "u", "E", "I")
set_b <- c("A", "a", "E", "e",      "i",      "o", "U", "u", "u", "a", "e")
set_c <- c("A", "e")
sd_ab <- set_diff(set_a, set_b)
sd_ba <- set_diff(set_b, set_a)

stopifnot(!is.null(attr(sd_ab, "class")))
stopifnot(!is.null(attr(sd_ba, "class")))
stopifnot(!is.null(attr(sd_ab, "yname")))
stopifnot(!is.null(attr(sd_ba, "yname")))
stopifnot(!is.null(attr(sd_ab, "xname")))
stopifnot(!is.null(attr(sd_ba, "xname")))

stopifnot(attr(sd_ab, "class") == "qwraps2_set_diff")
stopifnot(attr(sd_ba, "class") == "qwraps2_set_diff")

stopifnot(attr(sd_ab, "xname") == "set_a")
stopifnot(attr(sd_ba, "xname") == "set_b")

stopifnot(attr(sd_ab, "yname") == "set_b")
stopifnot(attr(sd_ba, "yname") == "set_a")

stopifnot(names(sd_ab) == c("all_values", "x_only", "y_only", "both", "equal"))
stopifnot(names(sd_ba) == c("all_values", "x_only", "y_only", "both", "equal"))

stopifnot(sort(sd_ab$all_values) == sort(sd_ba$all_values))
stopifnot(sd_ab$x_only == sd_ba$y_only)
stopifnot(sd_ab$y_only == sd_ba$x_only)
stopifnot(sd_ab$both == sd_ba$both)
stopifnot(sd_ab$equal == sd_ba$equal)
stopifnot(set_diff(set_b, set_b)$equal)

stopifnot(sd_ab$all_values == base::union(set_a, set_b))
stopifnot(sd_ab$x_only     == setdiff(set_a, set_b))
stopifnot(sd_ab$y_only     == setdiff(set_b, set_a))
stopifnot(sd_ab$both       == intersect(set_a, set_b))
stopifnot(sd_ab$both       == intersect(set_b, set_a))
stopifnot(sd_ab$equal      == setequal(set_a, set_b))
stopifnot(sd_ab$equal      == setequal(set_b, set_a))


# verify the print method returns the original object
sd_ab_printed <- print(sd_ab)
stopifnot(!is.null(sd_ab_printed))
stopifnot(identical(sd_ab_printed, sd_ab))
