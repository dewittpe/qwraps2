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

# GERBIL convention for indeterminate sensitivity, PPV, and F1: if TP, FP, and
# FN are all 0 then all three metrics are 1; if TP is 0 and FP + FN > 0 then all
# three metrics are 0.
cm_one_fp <- confusion_matrix(truth = 0, predicted = 1, thresholds = 1)$cm_stats
cm_one_fp <- cm_one_fp[is.finite(cm_one_fp$threshold), ]
stopifnot(cm_one_fp$sensitivity == 0)
stopifnot(cm_one_fp$ppv == 0)
stopifnot(cm_one_fp$f1 == 0)

cm_one_fn <- confusion_matrix(truth = 1, predicted = 0, thresholds = 1)$cm_stats
cm_one_fn <- cm_one_fn[is.finite(cm_one_fn$threshold), ]
stopifnot(cm_one_fn$sensitivity == 0)
stopifnot(cm_one_fn$ppv == 0)
stopifnot(cm_one_fn$f1 == 0)

cm_empty_positive_prediction <- confusion_matrix(truth = 0, predicted = 0, thresholds = Inf)$cm_stats
cm_empty_positive_prediction <- cm_empty_positive_prediction[cm_empty_positive_prediction$threshold == Inf, ]
stopifnot(cm_empty_positive_prediction$sensitivity == 1)
stopifnot(cm_empty_positive_prediction$ppv == 1)
stopifnot(cm_empty_positive_prediction$f1 == 1)

# Boundary confidence intervals should not produce NaNs when proportions are
# exactly zero or one.
cm_perfect <- confusion_matrix(truth = c(0, 1), predicted = c(0.2, 0.8), thresholds = 0.5)
cm_perfect_stats <- cm_perfect$cm_stats
stopifnot(!any(is.nan(unlist(cm_perfect_stats[, grepl("_(lcl|ucl)$", names(cm_perfect_stats))]))))
stopifnot(!any(is.nan(cm_perfect$auroc_ci)))
stopifnot(!any(is.nan(cm_perfect$auprc_ci)))

# errors if non-binomial fit is passed
fit <- glm(mpg > 20 ~ wt, data = mtcars)
test <- tryCatch(confusion_matrix(fit), error = function(e) e)
stopifnot(!is.null(test))
stopifnot(inherits(test, "error"))
