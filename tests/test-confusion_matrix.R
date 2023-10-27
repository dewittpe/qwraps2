library(qwraps2)

set.seed(42)
test  <- c(rep(1, 53), rep(0, 47))
truth <- c(rep(1, 20), rep(0, 33), rep(1, 10), rep(0, 37))

# S3 versions give same results
stopifnot(
  all.equal(confusion_matrix(truth, test),
            confusion_matrix(truth ~ test),
            check.attributes = FALSE)
)

con_mat <- confusion_matrix(truth = truth, predicted = test, thresholds = NULL)

# check names
stopifnot(
  names(con_mat) == c("threshold", "TP", "TN", "FP", "FN", "sensitivity", "specificity", "ppv", "npv", "mcc", "f1")
)

# Counts are as expected
stopifnot(con_mat[["TP"]] == c(30, 30, 20, 0),
          con_mat[["TN"]] == c(0, 0, 37, 70),
          con_mat[["FP"]] == c(70, 70, 33, 0),
          con_mat[["FN"]] == c(0, 0, 10, 30))

con_mat <- confusion_matrix(truth = truth, predicted = test, thresholds = 1)
stopifnot(con_mat[["TP"]] == 20,
          con_mat[["TN"]] == 37,
          con_mat[["FP"]] == 33,
          con_mat[["FN"]] == 10)

# Check Statistics
stopifnot(con_mat[["sensitivity"]] == 20 / 30)
stopifnot(con_mat[["specificity"]] == 37 / 70)
stopifnot(con_mat[["ppv"]] == 20 / 53)
stopifnot(con_mat[["npv"]] == 37 / 47)
