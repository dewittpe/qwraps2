#' @title Receiver-Operator and Precision-Recall Curves
#'
#' @description Construction of ROC and PRC data and plots.
#'
#' @details
#'
#' The area under the curve (AUC) is determined by a trapezoid approximation for
#' both the AUROC and AUPRC.
#'
#' @param x an object
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
#' qroc(cm)
#' qprc(cm)
#'
#' #########################################################
#' # Getting a ROC or PRC plot from a glm object:
#'
#' mod <- glm(
#'   formula = spam ~ word_freq_our + word_freq_over + capital_run_length_total
#' , data = spambase
#' , family = binomial()
#' )
#'
#' qroc(mod)
#' qprc(mod)
#'
#' #########################################################
#' # plot more than one ROC
#' mod2 <- update(mod, formula = . ~ word_freq_our)
#'
#' cm1 <- confusion_matrix(mod)
#' cm2 <- confusion_matrix(mod2)
#'
#' auc_data <- rbind(cbind(cm1$cm_stats, model = "Model 1"),
#'                   cbind(cm2$cm_stats, model = "Model 2"))
#'
#' qroc(auc_data) + ggplot2::aes(color = model)
#'
#' qprc(auc_data, prevalence = cm1$prevalence) + ggplot2::aes(color = model)
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
qroc.qwraps2_confusion_matrix <- function(x, ...) {
  qroc_ggplot(x[["cm_stats"]], ...)
}

#' @export
#' @rdname qroc-qprc
qroc.glm <- function(x, ...) {
  qroc(confusion_matrix(x, ...))
}

qroc_ggplot <- function(data) {
  stopifnot("specificity" %in% names(data))
  stopifnot("sensitivity" %in% names(data))
  data[["FPR"]] <- 1 - data[["specificity"]]
  data[["TPR"]] <- data[["sensitivity"]]
  ggplot2::ggplot(data) +
  eval(substitute(ggplot2::aes(x = X, y = Y), list(X = as.name("FPR"), Y = as.name("TPR")))) +
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
qprc.qwraps2_confusion_matrix <- function(x, ...) {
  qprc_ggplot(x[["cm_stats"]], prevalence = x$prevalence, ...)
}

#' @export
#' @rdname qroc-qprc
qprc.glm <- function(x, ...) {
  qprc(confusion_matrix(x, ...))
}

qprc_ggplot <- function(data, prevalence = NULL) {
  stopifnot("sensitivity" %in% names(data)) # recall
  stopifnot("ppv" %in% names(data)) # Precision
  g <-
    ggplot2::ggplot(data = data) +
    eval(substitute(ggplot2::aes(x = X, y = Y), list(X = as.name("sensitivity"), Y = as.name("ppv")))) +
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
