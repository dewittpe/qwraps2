#' @title Confusion Matrices (Contingency Tables)
#'
#' @description Construction of confusion matrices, accuracy, sensitivity,
#' specificity, confidence intervals (Wilson's method and (optional
#' bootstrapping)).
#'
#' @param x prediction condition vector, a two level factor variable or a
#' variable that can be converted to one.
#' @param ... not currently used
#'
#' @details
#' Sensitivity and Specificity:
#' For the sensitivity and specificity function we expect the 2-by-2 confusion
#' matrix (contingency table) to be of the form:
#'
#' \tabular{lccc}{
#'                     \tab      \tab True \tab Condition \cr
#'                     \tab      \tab +    \tab -         \cr
#' Predicted Condition \tab +    \tab TP   \tab FP        \cr
#' Predicted Condition \tab -    \tab FN   \tab TN        \cr
#' }
#' where
#' \itemize{
#'   \item FN: False Negative, and
#'   \item FP: False Positive, 
#'   \item TN: True Negative,
#'   \item TP: True Positive.
#' }
#'
#' Recall: 
#' \itemize{
#'   \item sensitivity = TP / (TP + FN)
#'   \item specificity = TN / (TN + FP) 
#'   \item positive predictive value (PPV) = TP / (TP + FP)
#'   \item negative predictive value (NPV) = TN / (TN + FN)
#' }
#'
#' @return The sensitivity and specificity functions return numeric values.
#' \code{confusion_matrix} returns a list with elements:
#' \itemize{
#'   \item tab the confusion matrix,
#'   \item stats a matrix of summary statistics and confidence intervals.
#' }
#'
#' @example examples/confusion_matrix.R
#' 
#' @export
#' @rdname confusion_matrix
confusion_matrix <- function(x, ...) { 
  UseMethod("confusion_matrix")
}

#' @param y True Condition vector with the same possible values as x.
#' @param positive the level of x and y which is the positive outcome.  If
#' missing the first level of factor(y) will be used as the positive level.
#' @param boot boolean, should bootstrapped confidence intervals for the
#' sensitivity and specificity be computed?  Defaults to FALSE.
#' @param boot_samples number of bootstrapping sample to generate, defaults to
#' 1000L.  Ignored if \code{boot == FALSE}.
#' @param alpha 100(1-alpha)% confidence intervals for specificity and
#' sensitivity.  Ignored if \code{boot == FALSE}.
#' @export
#' @rdname confusion_matrix
confusion_matrix.default <- function(x, y, positive, boot = FALSE, boot_samples = 1000L, alpha = 0.05, ...) { 
  confusion_matrix.formula(stats::as.formula(paste(deparse(substitute(y)), deparse(substitute(x)), sep = "~")),
                           data = stats::setNames(data.frame(x,  y), c(deparse(substitute(x)), deparse(substitute(y)))),
                           positive,
                           boot, boot_samples, alpha)
}

#' @param formula column (known) ~ row (test) for building the confusion matrix
#' @param data environment containing the variables listed in the formula
#' @export
#' @rdname confusion_matrix
confusion_matrix.formula <- function(formula, data = parent.frame(), positive, boot = FALSE, boot_samples = 1000L, alpha = 0.05, ...) { 

  mf <- stats::model.frame(formula, data)
  mf[[1]] <- factor(mf[[1]])  # Truth
  mf[[2]] <- factor(mf[[2]])  # Predicited

  if (nlevels(mf[[1]]) != 2L || nlevels(mf[[2]]) != 2L) {
    stop("qwraps2::confusion_matrix only supports factors with two levels.",
         call. = FALSE)
  }
  if (!identical(levels(mf[[1]]), levels(mf[[2]]))) { 
    stop("levels of x and y need to be identical.",
         call. = FALSE)
  }

  if (missing(positive)) {
    # Add error handing here
    positive <- levels(mf[[1]])[1]
  }
  mf[[1]] <- stats::relevel(mf[[1]], positive)
  mf[[2]] <- stats::relevel(mf[[2]], positive) 


  if (any(levels(mf[[1]]) != levels(mf[[2]]))) { 
    stop("qwraps2::confusion_matrix expectes the same levels for the factors.")
  } 

  tab <- table(mf[[2]], mf[[1]], dnn = c("Prediction", "Truth"))#rev(names(mf)))

  cells <- list(true_positives  = tab[1, 1],
                true_negatives  = tab[2, 2],
                false_positives = tab[1, 2],
                false_negatives = tab[2, 1],
                positives       = sum(tab[1, ]),
                negatives       = sum(tab[2, ]))

  # cat(paste(c("TP = ", "FN = ", "FP = ", "TN = "), conmat, collapse = "; ")) 

  stats <- rbind(Accuracy = accuracy(tab), 
                 Sensitivity = sensitivity(tab),
                 Specificity = specificity(tab), 
                 PPV = ppv(tab), 
                 NPV = npv(tab))

  stats <- cbind(stats, t(apply(stats, 1, wilson_score_interval, n = nrow(mf), alpha = alpha))) 
  colnames(stats) <- c("Est", "LCL", "UCL")

  if (boot) { 
    rows <- replicate(boot_samples, 
                      sample(seq(1, nrow(mf), by = 1), nrow(mf), replace = TRUE), 
                      simplify = FALSE)
    boot_stats <- 
      lapply(rows, 
             function(x) { 
               tab <- table(mf[[2]][x], mf[[1]][x], dnn = c("Prediction", "Truth"))

               rbind(Accuracy = accuracy(tab), 
                     Sensitivity = sensitivity(tab),
                     Specificity = specificity(tab),
                     PPV = ppv(tab), 
                     NPV = npv(tab))
             }) 
    boot_stats <- do.call(cbind, boot_stats)

    boot_stats <- apply(boot_stats, 1, 
                         function(x) {
                           c(mean(x), stats::quantile(x, probs = c(alpha / 2, 1 - alpha / 2)))
                         })
    boot_stats <- t(boot_stats)
    colnames(boot_stats) <- c("Boot Est", "Boot LCL", "Boot UCL")

    stats <- cbind(stats, boot_stats)
  }

  rtn <- list(tab = tab, cells = cells, stats = stats)

  class(rtn) <- c("confusion_matrix", class(rtn))
  attr(rtn, "boot") <- boot
  attr(rtn, "alpha") <- alpha
  attr(rtn, "var_names") <- stats::setNames(as.list(names(mf)), c("Truth", "Prediction"))
  attr(rtn, "positive_value") <- positive

  rtn 
}

#' @rdname confusion_matrix
#' @export
is.confusion_matrix <- function(x) inherits(x, "confusion_matrix")

#' @rdname confusion_matrix
#' @export
print.confusion_matrix <- function(x, ...) { 
  cat("\nTruth:          ", attr(x, "var_names")[[1]], 
      "\nPrediction:     ", attr(x, "var_names")[[2]],
      "\nPositive Value: ", attr(x, "positive"),
      "\n\n")

  print.table(apply(x$tab, 1:2, frmt)) 
  cat("\n")
  print(x$stats) 
  invisible(x) 
}

accuracy <- function(tab) { 
  as.numeric(sum(diag(tab)) / sum(tab))
}

ppv <- function(tab) { 
  as.numeric(tab[1, 1] / sum(tab[1, ]))
}

npv <- function(tab) { 
  as.numeric(tab[2, 2] / sum(tab[2, ]))
}

sensitivity <- function(tab) { 
  as.numeric(tab[1, 1] / sum(tab[, 1]))
}

specificity <- function(tab, ...) { 
  as.numeric(tab[2, 2] / sum(tab[, 2]))
}

wilson_score_interval <- function(p, n, alpha = 0.05) { 
  z <- stats::qnorm(1 - alpha/2) 
  1 / (1 + 1/n * z^2) * (p + 1 / (2 * n) * z^2 + c(-z, z) * sqrt( 1 / n * p * (1 - p) + 1 / (4 * n^2) * z^2)) 
}
