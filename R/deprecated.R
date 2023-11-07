#' Deprecated Functions
#'
#' Archive of deprecated functions. Some of these might be removed from the
#' package in later releases.
#'
#' @name deprecated
NULL

#' qroc and qprc building of data frames:
#'
#' Deprecated methods for building the data sets needed for plotting roc and prc
#' plots.  use \code{\link{confusion_matrix}} instead.
#'
#' @param fit a \code{glm} fit with \code{family = binomial()}, or predicted
#' values
#' @param truth ignored if \code{fit} is a \code{glm} object.  A vector of
#' observations, 0/1 or FALSE/TRUE values, of equal length to \code{fit}
#' @param n_threshold number of thresholds to use to estimate auroc or auprc
#' @param ... passed to \code{\link[stats]{predict}}
#'
#' @export
#' @rdname deprecated
qroc_build_data_frame <- function(fit, truth = NULL, n_threshold = 200, ...) {
  .Deprecated(new = "confusion_matrix", msg = "qroc_build_data_frame as been replaced by confusion_matrix(), this will become an error message in a later release of qwraps2")
  UseMethod("qroc_build_data_frame")
}


#' @export
#' @rdname deprecated
qroc_build_data_frame.default <- function(fit, truth = NULL, n_threshold = 200, ...) {
  .Deprecated(new = "confusion_matrix", msg = "qroc_build_data_frame as been replaced by confusion_matrix(), this will become an error message in a later release of qwraps2")
  stopifnot(!is.null(truth))
  stopifnot(length(fit) == length(truth))

  true_pos <- function(threshold){ sum((fit >= threshold) &  (truth)) }
  true_neg <- function(threshold){ sum((fit <  threshold) & !(truth)) }

  false_pos <- function(threshold){ sum((fit >= threshold) & !(truth)) }
  false_neg <- function(threshold){ sum((fit <  threshold) &  (truth)) }

  x <- matrix(seq(1, 0, length = n_threshold))

  true_positives  <- apply(x, 1, true_pos)
  true_negatives  <- apply(x, 1, true_neg)
  false_positives <- apply(x, 1, false_pos)
  false_negatives <- apply(x, 1, false_neg)

  sensitivity <- true_positives / (true_positives + false_negatives)
  specificity <- true_negatives / (true_negatives + false_positives)

  roc_data <- data.frame(false_positives = 1 - specificity,
                         true_positives  = sensitivity)

  # trapezoid rule approximation for the area under the curve
  auc <- sum((roc_data[2:n_threshold, 1] - roc_data[1:(n_threshold-1), 1]) * 1/2 *
             (roc_data[2:n_threshold, 2] + roc_data[1:(n_threshold-1), 2]))

  attr(roc_data, "auc") <- auc
  class(roc_data)       <- c("qwraps2_generated", class(roc_data))

  return(roc_data)
}

#' @export
#' @rdname deprecated
qroc_build_data_frame.glm <- function(fit, truth = NULL, n_threshold = 200, ...) {
  .Deprecated(new = "auc", msg = "qroc_build_data_frame as been replaced by auc(), this will become an error message in a later release of qwraps2")

  stopifnot(fit$family$family %in% c("binomial", "quasibinomial"))

  # find the predicted values
  pred_vals <- stats::predict(fit, ..., type = "response")

  qroc_build_data_frame(pred_vals, fit$y)
}

#' @export
#' @rdname deprecated
qprc_build_data_frame <- function(fit, n_threshold = 200, ...) {
  .Deprecated(new = "auc", msg = "qprc_build_data_frame as been replaced by auc(), this will become an error message in a later release of qwraps2")

  # find the predicted values
  pred_vals <- stats::predict(fit, ..., type = "response")

  true_pos <- function(threshold){ sum((pred_vals >= threshold) &  (fit$y)) }
  true_neg <- function(threshold){ sum((pred_vals <  threshold) & !(fit$y)) }

  false_pos <- function(threshold){ sum((pred_vals >= threshold) & !(fit$y)) }
  false_neg <- function(threshold){ sum((pred_vals <  threshold) &  (fit$y)) }

  x <- matrix(seq(1, 0, length = n_threshold))

  true_positives  <- apply(x, 1, true_pos)
  true_negatives  <- apply(x, 1, true_neg)
  false_positives <- apply(x, 1, false_pos)
  false_negatives <- apply(x, 1, false_neg)

  Recall    <- true_positives / (true_positives + false_negatives) # aka sensitivity
  Precision <- true_positives / (true_positives + false_positives) # aka positive predictive power

  Precision[is.na(Precision)] <- stats::na.omit(Precision)[1]

  prc_data <- data.frame(Recall, Precision)

  # trapezoid rule approximation for the area under the curve
  auc <- sum((prc_data[2:n_threshold, 1] - prc_data[1:(n_threshold-1), 1]) * 1/2 *
             (prc_data[2:n_threshold, 2] + prc_data[1:(n_threshold-1), 2]))

  attr(prc_data, "baseline") <- mean(fit$y)
  attr(prc_data, "auc") <- auc
  class(prc_data)       <- c("qwraps2_generated", class(prc_data))

  return(prc_data)
}
