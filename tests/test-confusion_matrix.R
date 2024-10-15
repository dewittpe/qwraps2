library(qwraps2)
################################################################################
df <-
  data.frame(
      truth = c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
    , pred  = c(1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0)
  )

TP <- with(df, sum(truth == 1 & pred == 1))
TN <- with(df, sum(truth == 0 & pred == 0))
FP <- with(df, sum(truth == 0 & pred == 1))
FN <- with(df, sum(truth == 1 & pred == 0))

cmat <- confusion_matrix(df$truth, df$pred, thresholds = 1)$cm_stats
stopifnot(nrow(cmat) == 3L)
stopifnot(cmat[is.finite(cmat$threshold), "TP"] == TP)
stopifnot(cmat[is.finite(cmat$threshold), "TN"] == TN)
stopifnot(cmat[is.finite(cmat$threshold), "FP"] == FP)
stopifnot(cmat[is.finite(cmat$threshold), "FN"] == FN)

################################################################################
set.seed(42)
test  <- c(rep(1, 53), rep(0, 47))
truth <- c(rep(1, 20), rep(0, 33), rep(1, 10), rep(0, 37))

# S3 versions give same results, save for the call element
cm1 <- confusion_matrix(truth, test)
cm2 <- confusion_matrix(truth ~ test)
stopifnot(all.equal(cm1, cm2, check.attributes = FALSE))

con_mat <- confusion_matrix(truth = truth, predicted = test, thresholds = NULL)

# check names, if this fails and is due to a change in the code, make sure the
# documentation for the return of confusion_matrix has been updated.
stopifnot(
  names(con_mat) == c("cm_stats", "auroc", "auroc_ci", "auprc", "auprc_ci", "confint_method", "alpha", "prevalence")
)

# check names of cm_stats
stopifnot(
  names(con_mat$cm_stats) == c("threshold", "TP", "TN", "FP", "FN"
                               , "sensitivity", "sensitivity_lcl", "sensitivity_ucl"
                               , "specificity", "specificity_lcl", "specificity_ucl"
                               , "ppv", "ppv_lcl", "ppv_ucl"
                               , "npv", "npv_lcl", "npv_ucl"
                               , "accuracy", "accuracy_lcl", "accuracy_ucl"
                               , "youden"
                               , "mcc"
                               , "f1")
)

# Counts are as expected
stopifnot(con_mat[["cm_stats"]][["TP"]] == c(30, 30, 20, 0),
          con_mat[["cm_stats"]][["TN"]] == c(0, 0, 37, 70),
          con_mat[["cm_stats"]][["FP"]] == c(70, 70, 33, 0),
          con_mat[["cm_stats"]][["FN"]] == c(0, 0, 10, 30))

con_mat <- confusion_matrix(truth = truth, predicted = test, thresholds = 1)
stopifnot(con_mat[["cm_stats"]][is.finite(con_mat$cm_stats$threshold), ][["TP"]] == 20,
          con_mat[["cm_stats"]][is.finite(con_mat$cm_stats$threshold), ][["TN"]] == 37,
          con_mat[["cm_stats"]][is.finite(con_mat$cm_stats$threshold), ][["FP"]] == 33,
          con_mat[["cm_stats"]][is.finite(con_mat$cm_stats$threshold), ][["FN"]] == 10)

# Check Statistics
stopifnot(con_mat[["cm_stats"]][is.finite(con_mat$cm_stats$threshold), ][["sensitivity"]] == 20 / 30)
stopifnot(con_mat[["cm_stats"]][is.finite(con_mat$cm_stats$threshold), ][["specificity"]] == 37 / 70)
stopifnot(con_mat[["cm_stats"]][is.finite(con_mat$cm_stats$threshold), ][["ppv"]] == 20 / 53)
stopifnot(con_mat[["cm_stats"]][is.finite(con_mat$cm_stats$threshold), ][["npv"]] == 37 / 47)

# errors if non-binomial fit is passed
fit <- glm(mpg > 20 ~ wt, data = mtcars)
test <- tryCatch(confusion_matrix(fit), error = function(e) e)
stopifnot(!is.null(test))
stopifnot(inherits(test, "error"))
