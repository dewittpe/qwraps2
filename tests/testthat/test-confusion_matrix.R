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

test_that("confusion_matrix object", 
          {
            expected_con_mat <- structure(list(tab = structure(c(20L, 10L, 33L, 37L), .Dim = c(2L, 2L), .Dimnames = structure(list(Prediction = c("1", "0"), Truth = c("1", "0")), .Names = c("Prediction", "Truth")), class = "table"), stats = structure(c(0.57, 0.666666666666667, 0.528571428571429, 0.377358490566038, 0.787234042553192, 0.472153895492123, 0.569623188665981, 0.431497322303242, 0.288554433843916, 0.697177823059726, 0.662667014758899, 0.751378978598357, 0.623531620656359, 0.475236424206854, 0.856038677970304), .Dim = c(5L, 3L), .Dimnames = list(c("Accuracy", "Sensitivity", "Specificity", "PPV", "NPV"), c("Est", "LCL", "UCL")))), .Names = c("tab", "stats"), class = c("confusion_matrix", "list"), boot = FALSE, alpha = 0.05, var_names = structure(list(Truth = "truth", Prediction = "test"), .Names = c("Truth", "Prediction")))

            expect_equal(con_mat, expected_con_mat)
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
            x <- factor(x, levels = c("A", "B"))
            y <- factor(y, levels = c("B", "A"))
            expect_error(confusion_matrix(x, y), "same levels for the factors")
          })

