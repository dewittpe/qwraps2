library(qwraps2)

set.seed(42)
test  <- c(rep(1, 53), rep(0, 47))
truth <- c(rep(1, 20), rep(0, 33), rep(1, 10), rep(0, 37))
con_mat <- confusion_matrix(test, truth, positive = "1")
con_mat_boot <- confusion_matrix(test, truth, positive = "1", boot = TRUE)

# S3 versions give same results
stopifnot(
  all.equal(confusion_matrix(test, truth, positive = "1") ,
            confusion_matrix(truth ~ test, positive = "1") ,
            check.attributes = FALSE)
)

# table is as expected
stopifnot(all.equal(unclass(con_mat$tab), matrix(c(20, 10, 33, 37), ncol = 2), check.attributes = FALSE))


# boot strap

stopifnot(
  all.equal(con_mat_boot$stats[, "Boot Est"],
            c(Accuracy = 0.57016, Sensitivity = 0.670261267572451, Specificity = 0.527289529267115,
              PPV = 0.377233632132097, NPV = 0.789307633664318, FNR = 0.329738732427549,
              FPR = 0.472710470732885, FDR = 0.622766367867903, FOR = 0.210692366335682,
              F1 = 0.479808238983136, MCC = 0.18118222982454)
  )
)

# Check Statistics
stopifnot(con_mat$stats["Sensitivity", 1] == 20 / 30)
stopifnot(con_mat$stats["Specificity", 1] == 37 / 70)
stopifnot(con_mat$stats["PPV", 1] == 20 / 53)
stopifnot(con_mat$stats["NPV", 1] == 37 / 47)

# check printing
output <- capture.output(print(con_mat))
stopifnot(any(grepl("Truth:\\ *truth", output)))
stopifnot(any(grepl("Prediction:\\ *test", output)))
stopifnot(any(grepl("Accuracy\\ *0\\.57", output)))
stopifnot(any(grepl("NPV\\ *0\\.78", output)))

# is confusion_matrix?
stopifnot(is.confusion_matrix(con_mat))
stopifnot(is.confusion_matrix(con_mat_boot))
stopifnot(!is.confusion_matrix(test))

# check for thrown errors
x <- sample(c("A", "B", "C"), 100, replace = TRUE)
y <- sample(c("A", "B"), 100, replace = TRUE)
test <- tryCatch(confusion_matrix(x, y), error = function(e) e)
stopifnot(inherits(test, "error"))

x <- sample(c("A", "B"), 100, replace = TRUE)
x <- factor(x, levels = c("B", "A"))
y <- sample(c("A", "B"), 100, replace = TRUE)
y <- factor(y, levels = c("B", "A"))

stopifnot(identical(attr(confusion_matrix(x, y), "positive") , "B"))

x <- factor(sample(c("C", "D"), 100, replace = TRUE))
test <- tryCatch(confusion_matrix(x, y), error = function(e) e)
stopifnot(inherits(test, "error"))

