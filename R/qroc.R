#' @title Receiver Operating Curves
#'
#' @description Construction of ROC curves.
#'
#' @details
#' Given a \code{glm} fit with \code{family = "binomial"} (either a log-link or
#' logit-link should be fine, a data set will be constructed and ROC plots
#' generated.
#'
#' The area under the curve (AUC) is deterined by a trapazoid approximation.
#'
#' @param fit a \code{glm} fit with \code{family = "binomial"}.
#' @param n_threshold number of thresholds to test against. 
#' @param generate_data logical, defaults to TRUE.  If TRUE, then the call to
#' \code{qblandaltman_build_data_frame} is done automatically for you.  If
#' FALSE, then you should explicitly call \code{qblandaltman_build_data_frame}
#' before calling \code{qblandaltman}.
#'
#' @return a ggplot.  Minimula aesthetics have been used so that the user may
#' modify the graphic as desired with ease.
#'
#' @examples
#'
#' # load the diamonds data set
#' data(diamonds, package = "ggplot2")
#'
#' 
#' # Two plots in one ggplot object
#' 
#' # individual plots
#' 
#' # combined plots
#' 
#' @export   
#' @rdname qroc
qroc <- function(fit, .data, n_threshold, generate_data) { 

  # if ("glm" %in% class(fit))

  if (is.null(attr(.data, "qwraps2_generated"))) { 
    if (generate_data) {
      .data <- qroc_build_data_frame(.data, alpha)
    }
  }

}

#' @export
#' @rdname qroc
qroc_build_data_frame <- function(fit, n_threshold) { 

  # find the predicted values
  pred_vals <- predict(fit, type = "response")

  true_pos <- function(threshold){sum((pred_vals >= threshold) & (fit$y))}
  true_neg <- function(threshold){sum((pred_vals <  threshold) & !(fit$y))}

  false_pos <- function(threshold){sum((pred_vals >= threshold) & !(fit$y))}
  false_neg <- function(threshold){sum((pred_vals <  threshold) & (fit$y))}

  # n <- 200 # number of thresholds to check
  x <- matrix(seq(1, 0, length = n))

  true_positives  <- apply(x, 1, true_pos)
  true_negatives  <- apply(x, 1, true_neg)
  false_positives <- apply(x, 1, false_pos)
  false_negatives <- apply(x, 1, false_neg)

  sensitivity <- true_positives / (true_positives + false_negatives)
  specificity <- true_negatives / (true_negatives + false_positives)

  roc_data <- data.frame(false_positives = 1 - specificity, 
                         true_positives  = sensitivity)

  # trapazoid rule approximation for the area under the curve
  auc <- sum((roc_data[2:n, 1] - roc_data[1:(n-1), 1]) * 1/2 * (roc_data[2:n, 2] + roc_data[1:(n-1), 2]))

  attr(rtn, "auc")               = auc
  attr(rtn, "qwraps2_generated") = TRUE

  return(rtn)
}

