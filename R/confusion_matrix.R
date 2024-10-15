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
#' @param confint_method character string denoting if the logit (default),
#' binomial, or Wilson Score method for deriving confidence intervals
#' @param alpha alpha level for 100 * (1 - alpha)\% confidence intervals
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
#' The statistics returned in the \code{cm_stats} element are:
#' \itemize{
#'   \item accuracy    = (TP + TN) / (TP + TN + FP + FN)
#'   \item sensitivity, aka true positive rate or recall = TP / (TP + FN)
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
#' \code{confusion_matrix} returns a list with elements
#' \itemize{
#'   \item \code{cm_stats} a data.frame with columns:
#'   \item \code{auroc} numeric value for the area under the receiver operating
#'   curve
#'   \item \code{auroc_ci} a numeric vector of length two with the lower and
#'   upper bounds for a 100(1-alpha)\% confidence interval about the auroc
#'   \item \code{auprc} numeric value for the area under the precision recall
#'   curve
#'   \item \code{auprc_ci} a numeric vector of length two with the lower and
#'   upper limits for a 100(1-alpha)\% confidence interval about the auprc
#'   \item \code{confint_method} a character string reporting the method used to
#'   build the \code{auroc_ci} and \code{auprc_ci}
#'   \item \code{alpha} the alpha level of the confidence intervals
#'   \item \code{prevalence} the proportion of the input of positive cases, that
#'   is (TP + FN) / (TP + FN + FP + TN) = P / (P + N)
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
#'
#' # Example 2: Use with a logistic regression model
#' mod <- glm(
#'   formula = spam ~ word_freq_our + word_freq_over + capital_run_length_total
#' , data = spambase
#' , family = binomial()
#' )
#'
#' confusion_matrix(mod)
#' confusion_matrix(mod, thresholds = 0.5)
#'
#' @export
#' @rdname confusion_matrix
confusion_matrix <- function(..., thresholds = NULL, confint_method = "logit", alpha = getOption("qwraps2_alpha", 0.05)) {
  UseMethod("confusion_matrix")
}

#' @export
#' @rdname confusion_matrix
confusion_matrix.default <- function(truth, predicted, ..., thresholds = NULL, confint_method = "logit", alpha = getOption("qwraps2_alpha", 0.05)) {

  truth <- as.integer(truth)

  # assumption checks
  stopifnot(!any(is.na(truth)), !any(is.na(predicted)))
  stopifnot(length(truth) == length(predicted))
  stopifnot(all(truth %in% c(0L, 1L)))
  stopifnot(is.numeric(predicted))

  if (is.null(thresholds)) {
    thresholds <- unique(predicted)
  } else {
    stopifnot(is.numeric(thresholds))
    thresholds <- unique(thresholds)
  }
  thresholds <- sort(unique(c(-Inf, thresholds, Inf)))

  cm_cells <-
    lapply(thresholds,
           function(threshold) {
             cells <-
               list(  threshold = threshold
                    , TP        = sum(truth == 1 & predicted >= threshold)
                    , TN        = sum(truth == 0 & predicted <  threshold)
                    , FP        = sum(truth == 0 & predicted >= threshold)
                    , FN        = sum(truth == 1 & predicted <  threshold))
           })
  sen <- sapply(cm_cells, do.call, what = sensitivity)
  spc <- sapply(cm_cells, do.call, what = specificity)
  ppv <- sapply(cm_cells, do.call, what = PPV)
  npv <- sapply(cm_cells, do.call, what = NPV)
  acc <- sapply(cm_cells, do.call, what = accuracy)

  auroc <- -traprule(x = 1 - spc, y = sen)
  auprc <- -traprule(x = sen, y = ppv)

  # marginal sums
  N <- unique(sapply(cm_cells, function(x) x[["TP"]] + x[["FP"]] + x[["TN"]] + x[["FN"]]))
  stopifnot(length(N) == 1L)
  condition_P <- unique(sapply(cm_cells, function(x) x[["TP"]] + x[["FN"]]))
  stopifnot(length(condition_P) == 1L)
  condition_N <- unique(sapply(cm_cells, function(x) x[["TN"]] + x[["FP"]]))
  stopifnot(length(condition_N) == 1L)
  predicted_P <- sapply(cm_cells, function(x) x[["TP"]] + x[["FP"]])
  predicted_N <- sapply(cm_cells, function(x) x[["FN"]] + x[["TN"]])

  # confidence intervals
  sen_ci <- do.call(rbind, Map(proportion_confint, p = sen, n = condition_P, method = confint_method, alpha = alpha))
  spc_ci <- do.call(rbind, Map(proportion_confint, p = spc, n = condition_N, method = confint_method, alpha = alpha))
  ppv_ci <- do.call(rbind, Map(proportion_confint, p = ppv, n = predicted_P, method = confint_method, alpha = alpha))
  npv_ci <- do.call(rbind, Map(proportion_confint, p = npv, n = predicted_N, method = confint_method, alpha = alpha))
  acc_ci <- do.call(rbind, Map(proportion_confint, p = acc, n = N,           method = confint_method, alpha = alpha))

  cm_stats <- lapply(cm_cells, as.data.frame)
  cm_stats <- do.call(rbind, cm_stats)
  cm_stats <- cbind(cm_stats
         , sensitivity     = sen
         , sensitivity_lcl = sen_ci[, 1]
         , sensitivity_ucl = sen_ci[, 2]
         , specificity     = spc
         , specificity_lcl = spc_ci[, 1]
         , specificity_ucl = spc_ci[, 2]
         , ppv             = ppv
         , ppv_lcl         = ppv_ci[, 1]
         , ppv_ucl         = ppv_ci[, 2]
         , npv             = npv
         , npv_lcl         = npv_ci[, 1]
         , npv_ucl         = npv_ci[, 2]
         , accuracy        = acc
         , accuracy_lcl    = acc_ci[, 1]
         , accuracy_ucl    = acc_ci[, 2]
         , youden          = sapply(cm_cells, do.call, what = youden)
         , mcc            = sapply(cm_cells, do.call, what = MCC)
         , f1             = sapply(cm_cells, do.call, what = F1)
         )

  rtn <-
    list(
           cm_stats       = cm_stats
         , auroc          = auroc
         , auroc_ci       = proportion_confint(p = auroc, n = N, method = confint_method, alpha = alpha)
         , auprc          = auprc
         , auprc_ci       = proportion_confint(p = auprc, n = N, method = confint_method, alpha = alpha)
         , confint_method = confint_method
         , alpha          = alpha
         , prevalence     = condition_P / (condition_P + condition_N)
         )

  class(rtn) <- c("qwraps2_confusion_matrix")
  rtn
}

#' @param formula column (known) ~ row (test) for building the confusion matrix
#' @param data environment containing the variables listed in the formula
#' @export
#' @rdname confusion_matrix
confusion_matrix.formula <- function(formula, data = parent.frame(), ..., thresholds = NULL, confint_method = "logit", alpha = getOption("qwraps2_alpha", 0.05)) {
  cl <- as.list(match.call())[-1]

  mf <- stats::model.frame(formula, data)
  cl[["truth"]] <- mf[[1]]
  cl[["predicted"]] <- mf[[2]]
  cl[["formula"]] <- NULL
  cl[["data"]] <- NULL

  do.call(confusion_matrix, cl)
}

#' @param x a \code{glm} object
#' @export
#' @rdname confusion_matrix
confusion_matrix.glm <- function(x, ..., thresholds = NULL, confint_method = "logit", alpha = getOption("qwraps2_alpha", 0.05)) {
  stopifnot(x[["family"]][["family"]] == "binomial")
  truth <- x[["y"]]
  pred  <- stats::predict(x, type = "response")
  confusion_matrix(truth = truth, predicted = pred, ..., thresholds = thresholds, confint_method = confint_method, alpha = alpha)
}

#' @rdname confusion_matrix
#' @export
print.qwraps2_confusion_matrix <- function(x, ...) {
  NextMethod(print, x)
  invisible(x)
}

################################################################################
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
  # ((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP+FN) * (TN+FP) * (TN+FN))

  # because there can be very large numbers work with logs when needed
  # denominator <- 0.5 * ( log(TP+FP) + log(TP+FN) + log(TN+FP) + log(TN+FN) )

  if ( ((TP + FP) == 0) | ((TP + FN) == 0) | ((TN + FP) == 0) | ((TN + FN) == 0)) {
    rtn <- NA_real_
  } else {
    denominator <- exp(0.5 * ( log(TP+FP) + log(TP+FN) + log(TN+FP) + log(TN+FN) ))

    if (TP == 0 | TN == 0 | FP == 0 | FN == 0 ) {
      numerator <- (TP * TN) - (FP * FN)
    } else {
      numerator <- exp(log(TP) + log(TN)) - exp(log(FP) + log(FN))
    }

    rtn <- numerator / denominator
  }

  rtn
}

proportion_confint <- function(p, n, method = "logit", alpha = getOption("qwraps2_alpha", 0.05)) {
  z <- stats::qnorm(1 - alpha/2)

  if (method == "logit") {
    m <- stats::qlogis(p)
    tau <- 1 / sqrt( n * p * (1 - p) )
    rtn <- stats::plogis(m + c(-z, z) * tau)
  } else if (method == "binomial") {
    rtn <- p + c(-z, z) * sqrt( p * (1 - p) / n)
  } else if (method == "wilson_score") {
    rtn <- 1 / (1 + 1/n * z^2) * (p + 1 / (2 * n) * z^2 + c(-z, z) * sqrt( 1 / n * p * (1 - p) + 1 / (4 * n^2) * z^2))
  } else {
    stop("method not in c('logit', 'binomial', 'wilson_score')")
  }
  rtn
}
