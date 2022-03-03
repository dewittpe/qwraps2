test_that("logit",
          {
            set.seed(42)
            p <- runif(100)

            expect_equivalent(logit(p), log(p/(1-p)))
          })

test_that("invlogit",
          {
            set.seed(42)
            p <- runif(100)
            expect_equivalent(invlogit(log(p/(1-p))), p)
          })

test_that("logit and invlogit are equivalent to plogis",
          {
            x <- seq(-3, 3, length = 1001)
            expect_equivalent(invlogit(x), plogis(x))

            x <- seq(sqrt(.Machine$double.eps),  1 - sqrt(.Machine$double.eps), length = 1001)
            expect_equivalent(logit(x), qlogis(x))
          }

)

