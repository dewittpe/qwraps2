#' Area Under the Curve
#'
#' Find the area under the Receiver-Operator and the Precision-Recall curve
#'
#' @param
#' @return
#'
#' @examples
#'
#' df <-
#'   data.frame(
#'       truth = c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
#'     , pred  = c(1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0)
#'   )
#'
#' cm1 <- confusion_matrix(df$truth, df$pred, thresholds = 1)
#' auc(cm1)
#'
#' cm2 <- confusion_matrix(df$truth, df$pred)
#' auc(cm2)
#'
#' # for a glm
#' mod <- glm(
#'   formula = spam ~ word_freq_our + word_freq_over + capital_run_length_total
#' , data = spambase
#' , family = binomial()
#' )
#' auc(mod)
#'
#' @export
auc <- function(x, ...) {
  UseMethod("auc")
}

#' @export
#' @rdname confusion_matrix
auc.qwraps2_confusion_matrix <- function(x, alpha = getOption("qwraps2_alpha", 0.05), frmtci_args = list(), ...) {

  this_cm <- rbind(x[["cm_stats"]], x[["cm_stats_Inf"]])
  this_cm <- this_cm[order(this_cm[["threshold"]]), ]
  
  roc_data <- data.frame(threshold = this_cm[["threshold"]], "FNR"    = 1 - this_cm[["specificity"]], "TPR" = this_cm[["sensitivity"]])
  prc_data <- data.frame(threshold = this_cm[["threshold"]], "Recall" = this_cm[["sensitivity"]], "Precision" = this_cm[["ppv"]])

  # auroc <- NA_real_
  # auprc <- NA_real_
  auroc <- traprule(rev(roc_data[["FNR"]]), rev(roc_data[["TPR"]]))
  auprc <- traprule(rev(prc_data[["Recall"]]), rev(prc_data[["Precision"]]))

  N <- unique(sum(x[["cm_stats"]][c("TP", "FP", "TN", "FN")]))
  condition_P <- unique(sum(x[["cm_stats"]][c("TP", "FN")]))
  condition_N <- unique(sum(x[["cm_stats"]][c("TN", "FP")]))

  auroc_m <- qlogis(auroc)
  auprc_m <- qlogis(auprc)
  auroc_s <- 1/sqrt(N * auroc * (1 - auroc))
  auprc_s <- 1/sqrt(N * auprc * (1 - auprc))
  auroc_lcl <- plogis(auroc_m + qnorm(alpha/2) * auroc_s)
  auprc_lcl <- plogis(auprc_m + qnorm(alpha/2) * auprc_s)
  auroc_ucl <- plogis(auroc_m + qnorm(1 - alpha/2) * auroc_s)
  auprc_ucl <- plogis(auprc_m + qnorm(1 - alpha/2) * auprc_s)

  rtn <-
    list(
          roc_data = roc_data
        , auroc = auroc
        , auroc_lcl = auroc_lcl
        , auroc_ucl = auroc_ucl
        , auroc_ci  = do.call(frmtci, c(list(x = c(auroc, auroc_lcl, auroc_ucl), est = 1, lcl = 2, ucl = 3), frmtci_args))
        , prc_data = prc_data
        , auprc = auprc
        , auprc_lcl = auprc_lcl
        , auprc_ucl = auprc_ucl
        , auprc_ci  = do.call(frmtci, c(list(x = c(auprc, auprc_lcl, auprc_ucl), est = 1, lcl = 2, ucl = 3), frmtci_args))
        , prevalence = condition_P / N
    )
  class(rtn) <- "qwraps2_auc"
  rtn
}

#' @export
auc.glm <- function(x, ...) {
  truth <- x[["y"]]
  pred  <- predict(x, type = "response")

  this_cm <- confusion_matrix(truth = truth, predicted = pred, thresholds = c(-Inf, unique(pred), Inf), ...)
  auc(x = this_cm, ...)
}
