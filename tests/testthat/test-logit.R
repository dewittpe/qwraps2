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
