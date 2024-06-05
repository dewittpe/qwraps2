#' @title Receiver-Operator and Precision-Recall Curves
#'
#' @description Construction of ROC and PRC data and plots.
#'
#' @details
#'
#' The area under the curve (AUC) is determined by a trapezoid approximation for
#' both the AUROC and AUPRC.
#'
#' More details and examples for graphics within qwraps2 are in the
#' vignette(\dQuote{qwraps2-graphics}, package = \dQuote{qwraps2})
#'
#' @param x an object
#' @param ... pass through
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
#' # View the vignette for more examples
#' \dontrun{
#' vignette("qwraps2-graphics")
#' }
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
  ggplot2::geom_path() +
  ggplot2::xlim(0, 1) +
  ggplot2::ylim(0, 1) +
  ggplot2::annotate(geom = "segment", x = 0, y = 0, xend = 1, yend = 1, color = "black", linetype = 2)
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
    ggplot2::geom_path() +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1)
  if (!is.null(prevalence)) {
    stopifnot(is.numeric(prevalence))
    g <- g + ggplot2::geom_hline(yintercept = prevalence)
  }
  g
}
