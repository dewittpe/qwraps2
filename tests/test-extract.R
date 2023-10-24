library(qwraps2)

fit <- lm(mpg ~ wt + hp + drat, data = mtcars)

stopifnot(extract_fstat(fit) == "$F_{3, 28} = 47.88$")
stopifnot(extract_fpvalue(fit) == "$P < 0.0001$")
