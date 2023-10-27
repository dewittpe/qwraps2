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
confusion_matrix <- function(..., thresholds = NULL) {
  UseMethod("confusion_matrix")
}

#' @export
#' @rdname confusion_matrix
confusion_matrix.default <- function(truth, predicted, ..., thresholds = NULL) {

  truth <- as.integer(truth)

  # assumption checks
  stopifnot(!any(is.na(truth)), !any(is.na(predicted)))
  stopifnot(length(truth) == length(predicted))
  stopifnot(all(truth %in% c(0L, 1L)))
  stopifnot(is.numeric(predicted))

  if (is.null(thresholds)) {
    thresholds <- sort(unique(predicted))
  } else {
    stopifnot(is.numeric(thresholds))
    thresholds <- sort(thresholds)
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
  cm_stats
}

#' @param formula column (known) ~ row (test) for building the confusion matrix
#' @param data environment containing the variables listed in the formula
#' @export
#' @rdname confusion_matrix
confusion_matrix.formula <- function(formula, data = parent.frame(), ..., threshold = NULL) {
  cl <- as.list(match.call())[-1]

  mf <- stats::model.frame(formula, data)

  confusion_matrix(truth = mf[[1]], predicted = mf[[2]], ...)
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

  # attr(rtn, "variance") <- rtn * (1 - rtn) / (TP + FN)

  rtn
}

specificity <- function(TP, TN, FP, FN, ...) {
  rtn <- TN  / (TN + FP)
  # attr(rtn, "variance") <- rtn * (1 - rtn) / (TN + FP)
  rtn
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
  # attr(rtn, "variance") <- rtn * (1 - rtn) / (TP + FP)
  rtn
}

NPV <- function(TP, TN, FP, FN, ...) {
  rtn <- TN  / (TN + FN)
  # attr(rtn, "variance") <- rtn * (1 - rtn) / (TN + FN)
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
