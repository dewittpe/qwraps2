#' @title Confusion Matrices (Contingency Tables)
#'
#' @description Construction of confusion matrices, accuracy, sensitivity,
#' specificity, confidence intervals (Wilson's method and (optional
#' bootstrapping)).
#'
#' @param truth a integer vector with the values \code{0} and \code{1}, or a logical vector.
#' A value of \code{0} or \code{FALSE} is an indication of condition negative;
#' \code{1} or \code{TRUE} is an indication of condition positive.
#' @param predicted a numeric vector.  See Details.
#' @param formula of the form \code{truth ~ predicted}.
#' @param data a data.frame containing the variables reference in
#' \code{formula}.
#' @param thresholds a numeric vector of thresholds to be used to define the
#' confusion matrix (one threshold) or matrices (two or more thresholds).  If
#' \code{NULL} the unique values of \code{predicted} will be used.
#' @param ... pass through
#' @param confint logical, if \code{TRUE} generate and report confidence
#' intervals for sensitivity, specificity, ppv, and npv.
#' @param confint_method character string denoting if the logit or binomial
#' method for deriving confidence intervals should be used
#' @param alpha alpha level for 100 * (1 - alpha) percent confidence intervals
#' @param frmtci_args a list of arguments passed to frmtci
#'
#' @details
#' The confusion matrix:
#'
#' \tabular{lccc}{
#'                     \tab      \tab True \tab Condition \cr
#'                     \tab      \tab +    \tab -         \cr
#' Predicted Condition \tab +    \tab TP   \tab FP        \cr
#' Predicted Condition \tab -    \tab FN   \tab TN        \cr
#' }
#' where
#' \itemize{
#'   \item FN: False Negative = truth = 1 & prediction < threshold,
#'   \item FP: False Positive = truth = 0 & prediction >= threshold,
#'   \item TN: True Negative  = truth = 0 & prediction < threshold, and
#'   \item TP: True Positive  = truth = 1 & prediction >= threshold.
#' }
#'
#' The statistics returned in the \code{stats} element are:
#' \itemize{
#'   \item accuracy    = (TP + TN) / (TP + TN + FP + FN)
#'   \item sensitivity, aka true positive rate = TP / (TP + FN)
#'   \item specificity, aka true negative rate = TN / (TN + FP)
#'   \item positive predictive value (PPV), aka precision = TP / (TP + FP)
#'   \item negative predictive value (NPV) = TN / (TN + FN)
#'   \item false negative rate (FNR) = 1 - Sensitivity
#'   \item false positive rate (FPR) = 1 - Specificity
#'   \item false discovery rate (FDR) = 1 - PPV
#'   \item false omission rate (FOR) = 1 - NPV
#'   \item F1 score
#'   \item Matthews Correlation Coefficient (MCC) =
#'     ((TP * TN) - (FP * FN)) / sqrt((TP + FP) (TP+FN) (TN+FP) (TN+FN))
#' }
#'
#' Synonyms for the statistics:
#' \itemize{
#' \item Sensitivity: true positive rate (TPR), recall, hit rate
#' \item Specificity: true negative rate (TNR), selectivity
#' \item PPV: precision
#' \item FNR: miss rate
#' }
#'
#' Sensitivity and PPV could, in some cases, be indeterminate due to division by
#' zero.  To address this we will use the following rule based on the DICE group
#' \url{https://github.com/dice-group/gerbil/wiki/Precision,-Recall-and-F1-measure}:
#' If TP, FP, and FN are all 0, then PPV, sensitivity, and F1 will be defined to
#' be 1.  If TP are 0 and FP + FN > 0, then PPV, sensitivity, and F1 are all
#' defined to be 0.
#'
#' @return
#' \code{confusion_matrix} returns a data.frame with columns
#' \itemize{
#'   \item
#'   \item
#'   \item
#' }
#'
#' @examples
#'
#' # Example 1: known truth and prediction status
#' df <-
#'   data.frame(
#'       truth = c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
#'     , pred  = c(1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0)
#'   )
#'
#' confusion_matrix(df$truth, df$pred, thresholds = 1)
#' confusion_matrix(df$truth, df$pred, thresholds = 1, confint = TRUE)
#' confusion_matrix(df$truth, df$pred, thresholds = 1, confint = TRUE, frmtci_args = list(show_level = TRUE, digits = 3))
#' confusion_matrix(df$truth, df$pred, thresholds = 1, confint = TRUE, alpha = 0.1)
#' confusion_matrix(df$truth, df$pred, thresholds = 1, confint = TRUE, alpha = 0.10, frmtci_args = list(show_level = TRUE, digits = 3))
#'
#' # Example 2: Use with a logistic regression model
#' mod <- glm(
#'   formula = spam ~ word_freq_our + word_freq_over + capital_run_length_total
#' , data = spambase
#' , family = binomial()
#' )
#'
#' pred <- predict(mod, type = "response")
#'
#' confusion_matrix(truth = spambase$spam, predicted = pred)
#' confusion_matrix(truth = spambase$spam, predicted = pred, thresholds = 0.5)
#'
#'
#' @export
#' @rdname confusion_matrix
confusion_matrix <- function(..., thresholds = NULL, confint = FALSE, confint_method = "logit", alpha = getOption("qwraps2_alpha", 0.05), frmtci_args = list(...)) {
  UseMethod("confusion_matrix")
}

#' @export
#' @rdname confusion_matrix
confusion_matrix.default <- function(truth, predicted, ..., thresholds = NULL, confint = FALSE, confint_method = "logit", alpha = getOption("qwraps2_alpha", 0.05), frmtci_args = list(...)) {

  truth <- as.integer(truth)

  # assumption checks
  stopifnot(!any(is.na(truth)), !any(is.na(predicted)))
  stopifnot(length(truth) == length(predicted))
  stopifnot(all(truth %in% c(0L, 1L)))
  stopifnot(is.numeric(predicted))

  if (is.null(thresholds)) {
    thresholds <- c(-Inf, sort(unique(predicted)), Inf)
  } else {
    stopifnot(is.numeric(thresholds))
    thresholds <- sort(unique(thresholds))
  }

  cm_stats <-
    lapply(thresholds,
           function(threshold) {
             cells <-
               list(  TP        = sum(truth == 1 & predicted >= threshold)
                    , TN        = sum(truth == 0 & predicted <  threshold)
                    , FP        = sum(truth == 0 & predicted >= threshold)
                    , FN        = sum(truth == 1 & predicted <  threshold))
             list(threshold = threshold
                  , cells
                  , sensitivity = do.call(sensitivity, cells)
                  , specificity = do.call(specificity, cells)
                  , ppv         = do.call(PPV, cells)
                  , npv         = do.call(NPV, cells)
                  , mcc         = do.call(MCC, cells)
                  , f1          = do.call(F1, cells)
                  )
           })
  cm_stats <- lapply(cm_stats, as.data.frame)
  cm_stats <- do.call(rbind, cm_stats)
  class(cm_stats) <- c("qwraps2_confusion_matrix", class(cm_stats))

  if (confint) {
    if (confint_method == "binomial") {

      sen_m <- cm_stats[["sensitivity"]]
      spc_m <- cm_stats[["specificity"]]
      ppv_m <- cm_stats[["ppv"]]
      npv_m <- cm_stats[["npv"]]
      sen_s <- sqrt(cm_stats[["sensitivity"]] * (1 - cm_stats[["sensitivity"]]) / (cm_stats[["TP"]] + cm_stats[["FN"]]))
      spc_s <- sqrt(cm_stats[["specificity"]] * (1 - cm_stats[["specificity"]]) / (cm_stats[["TN"]] + cm_stats[["FP"]]))
      ppv_s <- sqrt(cm_stats[["ppv"]] * (1 - cm_stats[["ppv"]]) / (cm_stats[["TP"]] + cm_stats[["FP"]]))
      npv_s <- sqrt(cm_stats[["npv"]] * (1 - cm_stats[["npv"]]) / (cm_stats[["TN"]] + cm_stats[["FN"]]))

      sensitivity_lcl <- sen_m + qnorm(alpha / 2) * sen_s
      sensitivity_ucl <- sen_m + qnorm(1 - alpha / 2) * sen_s
      specificity_lcl <- spc_m + qnorm(alpha / 2) * spc_s
      specificity_ucl <- spc_m + qnorm(1 - alpha / 2) * spc_s
      ppv_lcl <- ppv_m + qnorm(alpha / 2) * ppv_s
      ppv_ucl <- ppv_m + qnorm(1 - alpha / 2) * ppv_s
      npv_lcl <- npv_m + qnorm(alpha / 2) * npv_s
      npv_ucl <- npv_m + qnorm(1 - alpha / 2) * npv_s

    } else if (confint_method == "logit") {

      sen_m <- qlogis(cm_stats[["sensitivity"]])
      spc_m <- qlogis(cm_stats[["specificity"]])
      ppv_m <- qlogis(cm_stats[["ppv"]])
      npv_m <- qlogis(cm_stats[["npv"]])
      sen_s <- 1 / sqrt(cm_stats[["sensitivity"]] * (1 - cm_stats[["sensitivity"]]) * (cm_stats[["TP"]] + cm_stats[["FN"]]))
      spc_s <- 1 / sqrt(cm_stats[["specificity"]] * (1 - cm_stats[["specificity"]]) * (cm_stats[["TN"]] + cm_stats[["FP"]]))
      ppv_s <- 1 / sqrt(cm_stats[["ppv"]] * (1 - cm_stats[["ppv"]]) * (cm_stats[["TP"]] + cm_stats[["FP"]]))
      npv_s <- 1 / sqrt(cm_stats[["npv"]] * (1 - cm_stats[["npv"]]) * (cm_stats[["TN"]] + cm_stats[["FN"]]))

      sensitivity_lcl <- plogis(sen_m + qnorm(alpha / 2) * sen_s)
      sensitivity_ucl <- plogis(sen_m + qnorm(1 - alpha / 2) * sen_s)
      specificity_lcl <- plogis(spc_m + qnorm(alpha / 2) * spc_s)
      specificity_ucl <- plogis(spc_m + qnorm(1 - alpha / 2) * spc_s)
      ppv_lcl <- plogis(ppv_m + qnorm(alpha / 2) * ppv_s)
      ppv_ucl <- plogis(ppv_m + qnorm(1 - alpha / 2) * ppv_s)
      npv_lcl <- plogis(npv_m + qnorm(alpha / 2) * npv_s)
      npv_ucl <- plogis(npv_m + qnorm(1 - alpha / 2) * npv_s)

    } else {
      stop("confint_method not in c('logit', 'binomial')")
    }

    if ("show_level" %in% names(frmtci_args)) {
      if (is.logical(frmtci_args[["show_level"]])) {
        frmtci_args[["show_level"]] <- paste0(100 * (1 - alpha), "% CI: ")
      }
    }

    cm_stats <- cbind(cm_stats[c("threshold", "TP", "TN", "FP", "FN")]
                      , sensitivity = cm_stats[["sensitivity"]], sensitivity_lcl, sensitivity_ucl
                      , sensitivity_ci = do.call(frmtci, c(list(x = cbind(cm_stats[["sensitivity"]], sensitivity_lcl, sensitivity_ucl), est = 1, lcl = 2, ucl = 3), frmtci_args))
                      , specificity = cm_stats[["specificity"]], specificity_lcl, specificity_ucl
                      , specificity_ci = do.call(frmtci, c(list(x = cbind(cm_stats[["specificity"]], specificity_lcl, specificity_ucl), est = 1, lcl = 2, ucl = 3), frmtci_args))
                      , ppv = cm_stats[["ppv"]], ppv_lcl, ppv_ucl
                      , ppv_ci = do.call(frmtci, c(list(x = cbind(cm_stats[["ppv"]], ppv_lcl, ppv_ucl), est = 1, lcl = 2, ucl = 3), frmtci_args))
                      , npv = cm_stats[["npv"]], npv_lcl, npv_ucl
                      , npv_ci = do.call(frmtci, c(list(x = cbind(cm_stats[["npv"]], npv_lcl, npv_ucl), est = 1, lcl = 2, ucl = 3), frmtci_args))
                      , cm_stats[-which(names(cm_stats) %in% c("threshold", "TP", "TN", "FP", "FN"))]
                      )

  }

  cm_stats
}

#' @param formula column (known) ~ row (test) for building the confusion matrix
#' @param data environment containing the variables listed in the formula
#' @export
#' @rdname confusion_matrix
confusion_matrix.formula <- function(formula, data = parent.frame(), ..., thresholds = NULL, confint = FALSE, confint_method = "logit", alpha = getOption("qwraps2_alpha", 0.05), frmtci_args = list()) {
  cl <- as.list(match.call())[-1]

  mf <- stats::model.frame(formula, data)
  cl[["truth"]] <- mf[[1]]
  cl[["predicted"]] <- mf[[2]]
  cl[["formula"]] <- NULL
  cl[["data"]] <- NULL

  do.call(confusion_matrix, cl)
}

#' @export
#' @rdname confusion_matrix
auc <- function(x, ...) {
  UseMethod("auc")
}

#' @export
#' @rdname confusion_matrix
auc.qwraps2_confusion_matrix <- function(x, alpha = getOption("qwraps2_alpha", 0.05), frmtci_args = list(), ...) {
  roc_data <- data.frame(threshold = x$threshold, "FNR" = 1 - x$specificity, "TPR" = x$sensitivity)
  prc_data <- data.frame(threshold = x$threshold, "Recall" = x$sensitivity, "Precision" = x$ppv)

  auroc <- traprule(rev(roc_data$FNR), rev(roc_data$TPR))
  auprc <- traprule(rev(prc_data$Recall), rev(prc_data$Precision))

  N <- unique(sum(x[c("TP", "FP", "TN", "FN")]))
  condition_P <- unique(sum(x[c("TP", "FN")]))
  condition_N <- unique(sum(x[c("TN", "FP")]))

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
#' @rdname confusion_matrix
qroc <- function(x, ...) {
  UseMethod("qroc")
}

#' @export
#' @rdname confusion_matrix
qroc.qwraps2_confusion_matrix <- function(x, ...) {
  qroc(auc(x))
}

#' @export
#' @rdname confusion_matrix
qroc.data.frame <- function(x, ...) {
  stopifnot("FNR" %in% names(x))
  stopifnot("TPR" %in% names(x))
  ggplot2::ggplot(data = x) +
    eval(substitute(ggplot2::aes(x = X, y = Y), list(X = as.name("FNR"), Y = as.name("TPR")))) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 1), color = "black", linetype = 2) 
}

#' @export
#' @rdname confusion_matrix
qroc.qwraps2_auc <- function(x, ...) {
  qroc(x$roc_data)
}

#' @export
#' @rdname confusion_matrix
qprc <- function(x, ...) {
  UseMethod("qprc")
}

#' @export
#' @rdname confusion_matrix
qprc.qwraps2_confusion_matrix <- function(x, prevalence = NULL, ...) {
  qprc(auc(x), prevalence = prevalence, ...)
}

#' @export
#' @rdname confusion_matrix
qprc.data.frame <- function(x, prevalence = NULL, ...) {
  stopifnot("Recall" %in% names(x))
  stopifnot("Precision" %in% names(x))
  g <-
    ggplot2::ggplot(data = x) +
    eval(substitute(ggplot2::aes(x = X, y = Y), list(X = as.name("Recall"), Y = as.name("Precision")))) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1)
  if (!is.null(prevalence)) {
    stopifnot(is.numeric(prevalence))
    g <- g + ggplot2::geom_hline(yintercept = prevalence)
  }
  g
}

#' @export
#' @rdname confusion_matrix
qprc.qwraps2_auc <- function(x, prevalence = NULL, ...) {
  qprc(x = x$prc_data, prevalence = x$prevalence, ...)
}

#' @rdname confusion_matrix
#' @export
print.qwraps2_confusion_matrix <- function(x, ...) {
  NextMethod(print, x)
  invisible(x)
}

# non-exported functions
accuracy <- function(TP, TN, FP, FN, ...) {
  (TP + TN) / (TP + TN + FP + FN)
}

sensitivity <- function(TP, TN, FP, FN, ...) {
  # The following rule to deal with division by zero is based on the DICE group
  # <URL: https://github.com/dice-group/gerbil/wiki/Precision,-Recall-and-F1-measure>:
  if ((TP + FP + FN) == 0) {
    rtn <- 1
  } else if ((TP == 0) & (FP + FN > 1)) {
    rtn <- 0
  } else {
    rtn <- TP / (TP + FN)
  }

  rtn
}

specificity <- function(TP, TN, FP, FN, ...) {
  TN  / (TN + FP)
}

precision <- PPV <- function(TP, TN, FP, FN, ...) {
  # The following rule to deal with division by zero is based on the DICE group
  # <URL: https://github.com/dice-group/gerbil/wiki/Precision,-Recall-and-F1-measure>:
  if ((TP + FP + FN) == 0) {
    rtn <- 1
  } else if ((TP == 0) & (FP + FN > 1)) {
    rtn <- 0
  } else {
    rtn <- TP / (TP + FP)
  }
  rtn
}

NPV <- function(TP, TN, FP, FN, ...) {
  rtn <- TN  / (TN + FN)
  rtn
}

FNR <- function(TP, TN, FP, FN, ...) {
  1 - sensitivity(TP, TN, FP, FN)
}

FPR <- function(TP, TN, FP, FN, ...) {
  1 - specificity(TP, TN, FP, FN)
}

FDR <- function(TP, TN, FP, FN, ...) {
  1 - PPV(TP, TN, FP, FN)
}

# False Omission Rate
FOR <- function(TP, TN, FP, FN, ...) {
  1 - NPV(TP, TN, FP, FN)
}

F1 <- function(TP, TN, FP, FN, ...) {
  if ((TP + FP + FN) == 0) {
    rtn <- 1
  } else if ((TP == 0) & (FP + FN > 1)) {
    rtn <- 0
  } else {
    rtn <- (2 * TP) / (2 * TP + FP + FN)
  }
  rtn
}

youden <- function(TP, TN, FP, FN, ...) {
  sensitivity(TP, TN, FP, FN) + specificity(TP, TN, FP, FN) - 1
}

# Matthews Correlation Coefficient
MCC <- function(TP, TN, FP, FN, ...) {
  #((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP+FN) * (TN+FP) * (TN+FN))

  # because there can be very large numbers work with logs
  numerator <- exp(log(TP) + log(TN)) - exp(log(FP) + log(FN))
  exp(
    log(numerator) - 0.5 * ( log(TP+FP) + log(TP+FN) + log(TN+FP) + log(TN+FN) )
  )
}

wilson_score_interval <- function(p, n, alpha = 0.05) {
  z <- stats::qnorm(1 - alpha/2)
  1 / (1 + 1/n * z^2) * (p + 1 / (2 * n) * z^2 + c(-z, z) * sqrt( 1 / n * p * (1 - p) + 1 / (4 * n^2) * z^2))
}
