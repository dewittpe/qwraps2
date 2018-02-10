test  <- c(rep(1, 53), rep(0, 47))
truth <- c(rep(1, 20), rep(0, 33), rep(1, 10), rep(0, 37))
con_mat <- confusion_matrix(test, truth, positive = "1")

test_that("Table is as expected",
          { 
            expect_equivalent(unclass(con_mat$tab), matrix(c(20, 10, 33, 37), ncol = 1))
          })

test_that("Boot Strap", 
          {
            set.seed(42)
            con_mat_boot <- confusion_matrix(test, truth, positive = "1", boot = TRUE)

            expect_equivalent(round(con_mat_boot$stats[, "Boot Est"], 3),
                              c(0.573, 0.669, 0.532, 0.382, 0.788))
          })


test_that("Sensitivity",
          { 
            expect_equal(con_mat$stats["Sensitivity", 1], 20 / 30) 
          })

test_that("Specificity",
          { 
            expect_equal(con_mat$stats["Specificity", 1], 37 / 70)
          })

test_that("PPV",
          { 
            expect_equal(con_mat$stats["PPV", 1], 20 / 53)
          })

test_that("NPV",
          { 
            expect_equal(con_mat$stats["NPV", 1], 37 / 47)
          })

test_that("print confusion_matrix",
          {
            expect_output(print(con_mat), "Truth:\\ *truth")
            expect_output(print(con_mat), "Prediction:\\ *test")
            expect_output(print(con_mat), "Accuracy\\ *0.57")
            expect_output(print(con_mat), "NPV\\ *0.78")
          })

test_that("is confusion_matrix",
          { 
            expect_true(is.confusion_matrix(con_mat))
          })

test_that("errors for levels", 
          { 
            x <- sample(c("A", "B", "C"), 100, replace = TRUE)
            y <- sample(c("A", "B"), 100, replace = TRUE)
            expect_error(confusion_matrix(x, y), "factors with two levels")

            x <- sample(c("A", "B"), 100, replace = TRUE)
            x <- factor(x, levels = c("B", "A"))
            y <- sample(c("A", "B"), 100, replace = TRUE)
            y <- factor(y, levels = c("B", "A"))

            expect_equal(attr(confusion_matrix(x, y), "positive") , "B")
 
            x <- factor(sample(c("C", "D"), 100, replace = TRUE))
            expect_error(confusion_matrix(x, y), "levels of x and y need to be identical.")
          })

