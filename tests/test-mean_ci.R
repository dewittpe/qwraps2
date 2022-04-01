library(qwraps2)

# simple ci
stopifnot(
  all.equal(
              unclass(mean_ci(mtcars$mpg))
            ,
              c(mean(mtcars$mpg),
                mean(mtcars$mpg) + qnorm(0.025)*sd(mtcars$mpg)/sqrt(nrow(mtcars)),
                mean(mtcars$mpg) + qnorm(0.975)*sd(mtcars$mpg)/sqrt(nrow(mtcars)))
            , check.attributes = FALSE
  )
)

# Printing nicely
out <- capture.output(print(mean_ci(mtcars$mpg)))
stopifnot(grepl("20\\.09 \\(18\\.00, 22\\.18\\)", out))

out <- capture.output(print(mean_ci(mtcars$mpg), show_level = TRUE))
stopifnot(grepl("20.09 \\(95% CI: 18\\.00, 22.18\\)", out))

out <- capture.output(print(mean_ci(mtcars$mpg, alpha = 0.01), show_level = TRUE))
stopifnot(grepl("20.09 \\(99% CI: 17\\.35, 22\\.83\\)", out))

# Compare to the ci that comes form t.test
stopifnot(
          all.equal(
            t.test(mtcars$mpg)$conf.int,
            mean_ci(mtcars$mpg, qdist = stats::qt, qdist.args = list(df = 31))[2:3],
            check.attributes = FALSE
          ))

# simple ci with missing data
x <- mtcars$mpg
x[c(12, 14)] <- NA

stopifnot(
          all.equal(unclass(mean_ci(x, na_rm = TRUE)),
                    c(mean(x, na.rm = TRUE),
                      mean(x, na.rm = TRUE) + qnorm(0.025)*sd(x, na.rm = TRUE)/sqrt(sum(!is.na(x))),
                      mean(x, na.rm = TRUE) + qnorm(0.975)*sd(x, na.rm = TRUE)/sqrt(sum(!is.na(x))))
                    , check.attributes = FALSE)
)

# warnings
stopifnot(inherits(tryCatch(mean_ci(mtcars$mpg, na.rm = TRUE), warning = function(w) w), "warning"))

# errors
stopifnot(inherits(tryCatch(mean_ci(mtcars$mpg, transform = "exp"), error = function(e) e), "error"))
