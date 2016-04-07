#' @title Confusion Matrices (Contingency Tables)
#'
#' @description Construction of confusion matrices, accuracy, sensitivity,
#' specificity, confidence intervals (Wilson's method and (optional
#' bootstrapping)).
#'
#' @param x prediction vector
#' @param y True Condition vector
#' @param formula column (known) ~ row (test) for building the confusion matrix
#' @param data environment containing the variables listed in the formula
#' @param boot boolean, should bootstrapped confidence intervals for the
#' sensitivity and specificity be computed?  Defaults to FALSE.
#' @param boot_samples number of bootstrapping sample to generate, defaults to
#' 1000L.  Ignored if \code{boot == FALSE}.
#' @param alpha 100(1-alpha)% confidence intervals for specificity and
#' sensitivity.  Ignored if \code{boot == FALSE}.
#' @param ... not currently used
#'
#' @details
#' Sensitivity and Specificity:
#' For the sensitivity and specificity function we expect the 2-by-2 confusion
#' matrix (contingency table) to be of the form:
#'
#' \tabular{lccc}{
#'                     \tab      \tab True \tab Condition \cr
#'                     \tab      \tab -    \tab +         \cr
#' Predicted Condition \tab -    \tab TN   \tab FN        \cr
#' Predicted Condition \tab +    \tab FP   \tab TP        \cr
#' }
#' where
#' \itemize{
#'   \item TN: True Negative,
#'   \item FP: False Positive, 
#'   \item FN: False Negative, and
#'   \item TP: True Positive.
#' }
#'
#' sensitivity = TP / (TP + FN)
#' specificity = TN / (TN + FP) 
#'
#' This table set up is the result of using 0/1 or boolean variables. See
#' examples.
#'
#' @return The sensitivity and specificity functions return numeric values.
#' \code{confusion_matrix} returns a list with elements:
#' \itemize{
#'   \item tab the confusion matrix,
#'   \item specificity point estimate for specificity,
#'   \item sensitivity point estimate for sensitivity, 
#'   \item specificity_ci bootstrapped confidence interval for specificity, and
#'   \item sensitivity_ci bootstrapped confidence interval for sensitivity.
#' }
#'
#' @examples
#' ## Example taken from caret::confusionMatrix
#' \donttest{
#' lvs <- c("normal", "abnormal")
#' truth <- factor(rep(lvs, times = c(86, 258)),
#'                 levels = rev(lvs))
#' pred <- factor(c(rep(lvs, times = c(54, 32)),
#'                  rep(lvs, times = c(27, 231))),               
#'                levels = rev(lvs))
#' 
#' confusion_matrix(pred, truth)
#' confusion_matrix(pred, truth)$stats
#' 
#' temp <- confusion_matrix(pred, truth, boot = TRUE)
#' temp$stats
#' temp$boot_stats
#' }
#' 
#'
#' @export
#' @rdname confusion_matrix
confusion_matrix <- function(x, ...) { 
  UseMethod("confusion_matrix")
}

#' @export
confusion_matrix.default <- function(x, y, boot = FALSE, boot_samples = 1000L, alpha = 0.05) { 
  confusion_matrix.formula(Known ~ Predicted, 
                           data = data.frame(Predicted = x, Known = y), 
                           boot, boot_samples, alpha)
}

#' @export
confusion_matrix.formula <- function(formula, data, boot = FALSE, boot_samples = 1000L, alpha = 0.05) { 

  ftab <- stats::ftable(formula, data) 

  acc <- accuracy(ftab)
  sen <- sensitivity(ftab)
  spe <- specificity(ftab)

  cis <- lapply(list(acc, sen, spe), 
                wilson_score_interval,
                n = nrow(data), alpha = alpha)

  stats <- cbind(c(Accuracy = acc, Sensitivity = sen, Specificity = spe), 
                 do.call(rbind, cis))
  colnames(stats) <- c("Est", "LCL", "UCL")

  if (boot) { 
    rows <- replicate(boot_samples, 
                      sample(seq(1, nrow(data), by = 1), nrow(data), replace = TRUE), 
                      simplify = FALSE)
    tabs <- lapply(rows, function(x) { stats::ftable(formula, data[x, ]) })
    acs  <- do.call(c, lapply(tabs, accuracy))
    sps  <- do.call(c, lapply(tabs, specificity))
    sns  <- do.call(c, lapply(tabs, sensitivity))

    boot_stats <- lapply(list(acs, sns, sps), 
                         function(x) {
                           c(mean(x), stats::quantile(x, probs = c(alpha / 2, 1 - alpha / 2)))
                         }) 
    boot_stats <- do.call(rbind, boot_stats)
    colnames(boot_stats) <- c("Boot Est", "Boot LCL", "Boot UCL")
    rownames(boot_stats) <- c("Accuracy", "Sensitivity", "Specificity")

    stats <- cbind(stats, boot_stats)
  }

  rtn <- list(tab   = ftab, stats = stats)

  class(rtn) <- c("confusion_matrix", class(rtn))
  attr(rtn, "boot") <- boot
  attr(rtn, "alpha") <- alpha

  rtn 
}

#' @export
print.confusion_matrix <- function(x, ...) { 
  stats:::print.ftable(x$tab) 
  print(x$stats) 
  invisible(x) 
}

accuracy <- function(tab) { 
  if (any(dim(tab) != 2)) { stop("Incorrect dim(tab)") } 
  as.numeric(sum(diag(tab)) / sum(tab))
}

sensitivity <- function(tab) { 
  if (any(dim(tab) != 2)) { stop("Incorrect dim(tab)") } 
  as.numeric(tab[1, 1] / sum(tab[, 1]))
}

specificity <- function(tab, ...) { 
  if (length(dim(tab)) != 2 | any(dim(tab) != 2)) { stop("Incorrect dim(tab)") } 
  as.numeric(tab[2, 2] / sum(tab[, 2]))
}

wilson_score_interval <- function(p, n, alpha = 0.05) { 
  z <- qnorm(1 - alpha/2) 
  1 / (1 + 1/n * z^2) * (p + 1 / (2 * n) * z^2 + c(-z, z) * sqrt( 1 / n * p * (1 - p) + 1 / (4 * n^2) * z^2)) 
}
