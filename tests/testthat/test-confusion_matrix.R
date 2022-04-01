set.seed(42)
test  <- c(rep(1, 53), rep(0, 47))
truth <- c(rep(1, 20), rep(0, 33), rep(1, 10), rep(0, 37))
con_mat <- confusion_matrix(test, truth, positive = "1")
con_mat_boot <- confusion_matrix(test, truth, positive = "1", boot = TRUE)

test_that("S3 versions give same results",
          {
            expect_equal( confusion_matrix(test, truth, positive = "1")
                         , 
                           confusion_matrix(truth ~ test, positive = "1")
                         ,
                         ignore_attr = TRUE
                         )
          })

test_that("Table is as expected",
          {
            expect_equal(unclass(con_mat$tab), matrix(c(20, 10, 33, 37), ncol = 2), ignore_attr = TRUE)
          })

test_that("Boot Strap",
          {
            expect_equal(con_mat_boot$stats[, "Boot Est"],
                              c(Accuracy = 0.57016, Sensitivity = 0.670261267572451, Specificity = 0.527289529267115,
                                PPV = 0.377233632132097, NPV = 0.789307633664318, FNR = 0.329738732427549,
                                FPR = 0.472710470732885, FDR = 0.622766367867903, FOR = 0.210692366335682,
                                F1 = 0.479808238983136, MCC = 0.18118222982454)
                              )
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
            expect_error(confusion_matrix(x, y))

            x <- sample(c("A", "B"), 100, replace = TRUE)
            x <- factor(x, levels = c("B", "A"))
            y <- sample(c("A", "B"), 100, replace = TRUE)
            y <- factor(y, levels = c("B", "A"))

            expect_equal(attr(confusion_matrix(x, y), "positive") , "B")

            x <- factor(sample(c("C", "D"), 100, replace = TRUE))
            expect_error(confusion_matrix(x, y))
          })

