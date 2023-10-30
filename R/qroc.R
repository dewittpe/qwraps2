#' @title Receiver-Operator and Precision-Recall Curves
#'
#' @description Construction of ROC and PRC data and plots.
#'
#' @details
#'
#' The area under the curve (AUC) is determined by a trapezoid approximation for
#' both the AUROC and AUPRC.
#'
#' @param x a \code{glm} fit, \code{qwraps2_confusion_matrix},
#' \code{qwraps2_auc}, or \code{data.frame}.  In the case of a generic data
#' frame, there is an expectation of two columns \dQuote{FNR} and \dQuote{TPR} for
#' \code{qroc}; \dQuote{Recall} and \dQuote{Precision} for \code{qprc}.
#' @param ... passed to \code{stats::predict}
#'
#' @seealso \code{vignette("qwraps2-graphics", package = "qwraps2")} for more
#' examples.
#'
#' @return a ggplot.  Minimal aesthetics have been used so that the user may
#' modify the graphic as desired with ease.
#'
#' @examples
#' #########################################################
#' # Example 1
#'
#' df <-
#'   data.frame(
#'       truth = c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
#'     , pred  = c(1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0)
#'   )
#'
#' cm <- confusion_matrix(df$truth, df$pred)
#' auc <- auc(cm)
#' auc$auroc
#' auc$auprc
#'
#' # You can get the same ROC/PRC plot from either the confusion_matrix or the
#' # auc object
#' qroc(cm)
#' qroc(auc)
#' qprc(cm)
#' qprc(auc)
#'
#' #########################################################
#' # Getting a ROC or PRC plot from a glm object:
#' # you could call confusion_matrix, auc, or just go directly to qroc/qprc
#'
#' mod <- glm(
#'   formula = spam ~ word_freq_our + word_freq_over + capital_run_length_total
#' , data = spambase
#' , family = binomial()
#' )
#'
#' qroc(mod)
#'
#' qprc(mod)
#' qprc(mod) + ggplot2::ylim(0, 1)
#'
#' #########################################################
#' # plot more than one ROC
#' mod2 <- update(mod, formula = . ~ word_freq_our)
#'
#' auc1 <- auc(mod)
#' auc2 <- auc(mod2)
#'
#' auroc_data <- rbind(cbind(auc1$roc_data, model = "Model 1"),
#'                     cbind(auc2$roc_data, model = "Model 2"))
#'
#' qroc(auroc_data) + ggplot2::aes(color = model)
#'
#' auc1$auroc
#' auc2$auroc
#'
#' auprc_data <- rbind(cbind(auc1$prc_data, model = "Model 1"),
#'                     cbind(auc2$prc_data, model = "Model 2"))
#'
#' qprc(auprc_data, prevalence = auc1$prevalence) + ggplot2::aes(color = model)
#'
#' auc1$auprc
#' auc2$auprc
#'
#'
#' @name
#' qroc-qprc
NULL

#' @export
#' @rdname qroc-qprc
qroc <- function(x, ...) {
  UseMethod("qroc")
}

#' @export
#' @rdname qroc-qprc
qroc.default <- function(x, ...) {
  qroc_ggplot(x, ...)
}

#' @export
#' @rdname qroc-qprc
qroc.qwraps2_auc <- function(x, ...) {
  qroc_ggplot(x[["roc_data"]], ...)
}

#' @export
#' @rdname qroc-qprc
qroc.qwraps2_confusion_matrix <- function(x, ...) {
  auc_data <- auc(x, ...)
  qroc_ggplot(auc_data[["roc_data"]], ...)
}

#' @export
#' @rdname qroc-qprc
qroc.glm <- function(x, ...) {
  auc_data <- auc(x, ...)
  qroc_ggplot(auc_data[["roc_data"]])
}

qroc_ggplot <- function(data) {
  stopifnot("FNR" %in% names(data))
  stopifnot("TPR" %in% names(data))
  ggplot2::ggplot(data) +
  eval(substitute(ggplot2::aes(x = X, y = Y), list(X = as.name("FNR"), Y = as.name("TPR")))) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::xlim(0, 1) +
  ggplot2::ylim(0, 1) +
  ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 1), color = "black", linetype = 2)
}

#' @export
#' @rdname qroc-qprc
qprc <- function(x, ...) {
  UseMethod("qprc")
}

#' @export
#' @rdname qroc-qprc
qprc.default <- function(x, ...) {
  qprc_ggplot(x, ...)
}

#' @export
#' @rdname qroc-qprc
qprc.qwraps2_auc <- function(x, ...) {
  qprc_ggplot(x[["prc_data"]], ...)
}

#' @export
#' @rdname qroc-qprc
qprc.qwraps2_confusion_matrix <- function(x, ...) {
  auc_data <- auc(x, ...)
  qprc_ggplot(auc_data[["prc_data"]], ...)
}

#' @export
#' @rdname qroc-qprc
qprc.glm <- function(x, ...) {
  auc_data <- auc(x, ...)
  qprc_ggplot(auc_data[["prc_data"]])
}

qprc_ggplot <- function(data, prevalence = NULL) {
  stopifnot("Recall" %in% names(data))
  stopifnot("Precision" %in% names(data))
  g <-
    ggplot2::ggplot(data = data) +
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

################################################################################
# Deprecated Functions

#' @export
#' @param fit a \code{glm} fit with \code{family = binomial()}, or predicted
#' values
#' @param truth ignored if \code{fit} is a \code{glm} object.  A vector of
#' observations, 0/1 or FALSE/TRUE values, of equal length to \code{fit}
#' @param n_threshold number of thresholds to use to estimate aucroc or auprc
#'
#' @export
#' @rdname qroc-qprc
qroc_build_data_frame <- function(fit, truth = NULL, n_threshold = 200, ...) {
  .Deprecated(new = "auc", msg = "qroc_build_data_frame as been replaced by auc(), this will become an error message in a later release of qwraps2")
  UseMethod("qroc_build_data_frame")
}


#' @export
#' @rdname qroc-qprc
qroc_build_data_frame.default <- function(fit, truth = NULL, n_threshold = 200, ...) {
  .Deprecated(new = "auc", msg = "qroc_build_data_frame as been replaced by auc(), this will become an error message in a later release of qwraps2")
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
#' @rdname qroc-qprc
qroc_build_data_frame.glm <- function(fit, truth = NULL, n_threshold = 200, ...) {
  .Deprecated(new = "auc", msg = "qroc_build_data_frame as been replaced by auc(), this will become an error message in a later release of qwraps2")

  stopifnot(fit$family$family %in% c("binomial", "quasibinomial"))

  # find the predicted values
  pred_vals <- stats::predict(fit, ..., type = "response")

  qroc_build_data_frame(pred_vals, fit$y)
}

#' @export
#' @rdname qroc-qprc
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
