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
#' The statistics returned in the \code{stats} element are:
#' \itemize{
#'   \item accuracy    = (TP + TN) / (TP + TN + FP + FN)
#'   \item sensitivity = TP / (TP + FN)
#'   \item specificity = TN / (TN + FP)
#'   \item positive predictive value (PPV) = TP / (TP + FP)
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
#' @return The sensitivity and specificity functions return numeric values.
#' \code{confusion_matrix} returns a list with elements:
#' \itemize{
#'   \item \code{tab} the confusion matrix,
#'   \item \code{cells}
#'   \item \code{stats} a matrix of summary statistics and confidence intervals.
#' }
#'
#' @examples
#' ################################################################################
#' ## Example 1
#' test  <- c(rep(1, 53), rep(0, 47))
#' truth <- c(rep(1, 20), rep(0, 33), rep(1, 10), rep(0, 37))
#' con_mat <- confusion_matrix(x = test, y = truth, positive = "1")
#' str(con_mat)
#'
#' con_mat
#'
#' con_mat$cells$true_positives  # 20
#' con_mat$cells$true_negatives  # 37
#' con_mat$cells$false_positives # 33
#' con_mat$cells$false_negatives # 10
#'
#' con_mat_with_boot <- confusion_matrix(test, truth, positive = "1", boot = TRUE)
#' con_mat_with_boot
#'
#' # only one value in one of the vectors
#' a <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0)  # all zeros
#' b <- c(1,0,1,0,1,0,0,0,0,0,0,0,0,1)  # some zeros and ones
#'
#' confusion_matrix(a, b)
#' confusion_matrix(b, a)
#' confusion_matrix(a, b, positive = 1)
#' confusion_matrix(b, a, positive = 1)
#'
#'
#' ################################################################################
#' ## Example 2: based on an example from the wikipedia page:
#' # https://en.wikipedia.org/wiki/Confusion_matrix
#'
#' animals <-
#'   data.frame(Predicted = c(rep("Cat",    5 + 2 +  0),
#'                            rep("Dog",    3 + 3 +  2),
#'                            rep("Rabbit", 0 + 1 + 11)),
#'              Actual    = c(rep(c("Cat", "Dog", "Rabbit"), times = c(5, 2,  0)),
#'                            rep(c("Cat", "Dog", "Rabbit"), times = c(3, 3,  2)),
#'                            rep(c("Cat", "Dog", "Rabbit"), times = c(0, 1, 11))),
#'              stringsAsFactors = FALSE)
#'
#' table(animals)
#'
#' cats <- apply(animals, 1:2, function(x) ifelse(x == "Cat", "Cat", "Non-Cat"))
#'
#' # Default calls, note the difference based on what is set as the 'positive'
#' # value.
#' confusion_matrix(cats[, "Predicted"], cats[, "Actual"], positive = "Cat")
#' confusion_matrix(cats[, "Predicted"], cats[, "Actual"], positive = "Non-Cat")
#'
#' # Using a Formula
#' confusion_matrix(formula = I(Actual == "Cat") ~ I(Predicted == "Cat"),
#'                  data = animals,
#'                  positive = "TRUE")
#'
#' confusion_matrix(formula = I(Actual == "Cat") ~ I(Predicted == "Cat"),
#'                  data = animals,
#'                  positive = "TRUE",
#'                  boot = TRUE)
#'
#' ################################################################################
#' ## Example 3
#' russell <-
#'   data.frame(Pred  = c(rep(0, 2295), rep(0, 118), rep(1, 1529), rep(1, 229)),
#'              Truth = c(rep(0, 2295), rep(1, 118), rep(0, 1529), rep(1, 229)))
#'
#' # The values for Sensitivity, Specificity, PPV, and NPV are dependent on the
#' # "positive" level.  By default, the first level of y is used.
#' confusion_matrix(x = russell$Pred, y = russell$Truth, positive = "0")
#' confusion_matrix(x = russell$Pred, y = russell$Truth, positive = "1")
#'
#' confusion_matrix(Truth ~ Pred, data = russell, positive = "0")
#' confusion_matrix(Truth ~ Pred, data = russell, positive = "1")
#'
#' @export
#' @rdname confusion_matrix
confusion_matrix <- function(x, ...) {
  UseMethod("confusion_matrix")
}

#' @param y True Condition vector with the same possible values as x.
#' @param positive the level of x and y which is the positive outcome.  If
#' \code{NULL} the first level of \code{factor(y)} will be used as the positive
#' level.
#' @param boot boolean, should bootstrapped confidence intervals for the
#' sensitivity and specificity be computed?  Defaults to FALSE.
#' @param boot_samples number of bootstrapping sample to generate, defaults to
#' 1000L.  Ignored if \code{boot == FALSE}.
#' @param alpha 100(1-alpha)% confidence intervals for specificity and
#' sensitivity.  Ignored if \code{boot == FALSE}.
#'
#' @export
#' @rdname confusion_matrix
confusion_matrix.default <- function(x, y, positive = NULL, boot = FALSE, boot_samples = 1000L, alpha = 0.05, ...) {
  cl <- as.list(match.call())
  xname <- deparse(cl$x)
  yname <- deparse(cl$y)

  if (is.factor(x)) {
    ux <- levels(x)
  } else {
    ux <- unique(x)
  }

  if (is.factor(y)) {
    uy <- levels(y)
  } else {
    uy <- unique(y)
  }

  if (length(ux) == 1 & length(uy) == 2 & all(ux %in% uy)) {
    x <- factor(x, levels = uy)
  } else {
    x <- factor(x, levels = ux)
  }

  if (length(uy) == 1 & length(ux) == 2 & all(uy %in% ux)) {
    y <- factor(y, levels = ux)
  } else {
    y <- factor(y, levels = uy)
  }

  if (nlevels(x) != 2L | nlevels(y) != 2L) {
    stop(paste0("qwraps2::confusion_matrix only supports inputs with two unique values.",
                "\n  `", xname, "` has ", nlevels(x), " unique values and \n  `",
                yname, "` has ", nlevels(y), " unique values."),
         call. = FALSE)
  }
  if (!identical(levels(x), levels(y))) {
    print(do.call(set_diff, args = list(x = as.name(xname), y = as.name(yname))))
    stop(paste0("levels of `", xname, "` and `", yname, "` need to be identical."),
         call. = FALSE)
  }

  if(length(x) != length(y)) {
    stop("length of x and y need to be the same")
  }

  if (is.null(positive)) {
    # Add error handing here
    positive <- levels(x)[1]
  }
  x <- stats::relevel(x, positive)
  y <- stats::relevel(y, positive)

  tab <- table(x, y, dnn = c("Predicted Condition", "True Condition"))
  rownames(tab)[1] <- paste0("pos. (", rownames(tab)[1], ")")
  rownames(tab)[2] <- paste0("neg. (", rownames(tab)[2], ")")
  colnames(tab)[1] <- paste0("pos. (", colnames(tab)[1], ")")
  colnames(tab)[2] <- paste0("neg. (", colnames(tab)[2], ")")

  cells <- list(true_positives  = tab[1, 1],
                true_negatives  = tab[2, 2],
                false_positives = tab[1, 2],
                false_negatives = tab[2, 1],
                positives       = sum(tab[1, ]),
                negatives       = sum(tab[2, ]))

  stats <- rbind(Accuracy    = accuracy(tab),
                 Sensitivity = sensitivity(tab),
                 Specificity = specificity(tab),
                 PPV         = ppv(tab),
                 NPV         = npv(tab),
                 FNR         = 1 - sensitivity(tab),
                 FPR         = 1 - specificity(tab),
                 FDR         = 1 - ppv(tab),
                 FOR         = 1 - npv(tab)
                 )

  stats <- cbind(stats, t(apply(stats, 1, wilson_score_interval, n = length(x), alpha = alpha)))
  colnames(stats) <- c("Est", "LCL", "UCL")
  stats <- rbind(stats, cbind(f1(tab), NA, NA), cbind(mcc(tab), NA, NA))
  rownames(stats)[nrow(stats) - c(1, 0)] <- c("F1", "MCC")


  if (boot) {
    rows <- replicate(boot_samples,
                      sample(seq(1, length(x), by = 1), length(x), replace = TRUE),
                      simplify = FALSE)
    boot_stats <-
      lapply(rows,
             function(xx) {
               tab <- table(x[xx], y[xx], dnn = c("Predicted Condition", "True Condition"))

               rbind(Accuracy    = accuracy(tab),
                     Sensitivity = sensitivity(tab),
                     Specificity = specificity(tab),
                     PPV         = ppv(tab),
                     NPV         = npv(tab),
                     FNR         = 1 - sensitivity(tab),
                     FPR         = 1 - specificity(tab),
                     FDR         = 1 - ppv(tab),
                     FOR         = 1 - npv(tab),
                     F1          = f1(tab),
                     MCC         = mcc(tab))

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
  attr(rtn, "var_names") <- list("Truth" = yname, "Prediction" = xname)
  attr(rtn, "positive_value") <- positive
  attr(rtn, "call") <- match.call()

  rtn
}

#' @param formula column (known) ~ row (test) for building the confusion matrix
#' @param data environment containing the variables listed in the formula
#' @export
#' @rdname confusion_matrix
confusion_matrix.formula <- function(formula, data = parent.frame(), positive = NULL, boot = FALSE, boot_samples = 1000L, alpha = 0.05, ...) {
  cl <- as.list(match.call())[-1]

  mf <- stats::model.frame(formula, data)

  e <- environment(formula)
  e[[names(mf)[1]]] <- mf[[1]]
  e[[names(mf)[2]]] <- mf[[2]]

  cl$y <- substitute(y, list(y = as.name(names(mf)[1])))
  cl$x <- substitute(x, list(x = as.name(names(mf)[2])))

  do.call(confusion_matrix.default, args = cl, envir = e)
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

  print.table(apply(x$tab, 1:2, frmt), right = TRUE)
  cat("\n")
  print(x$stats)
  invisible(x)
}

accuracy <- function(tab) {
  as.numeric(sum(diag(tab)) / sum(tab))
}

precision <- ppv <- function(tab) {
  if (tab[1, 1] == 0) {
    if ((tab[2, 1] + tab[1, 2]) == 0) {
      return(1)
    } else {
      return(0)
    }
  } else {
    as.numeric(tab[1, 1] / sum(tab[1, ]))
  }
}

npv <- function(tab) {
  as.numeric(tab[2, 2] / sum(tab[2, ]))
}

tpr <- recall <- sensitivity <- function(tab) {
  if (tab[1, 1] == 0) {
    if ((tab[2, 1] + tab[1, 2]) == 0) {
      return(1)
    } else {
      return(0)
    }
  } else {
    as.numeric(tab[1, 1] / sum(tab[, 1]))
  }
}

tnr <- specificity <- function(tab, ...) {
  as.numeric(tab[2, 2] / sum(tab[, 2]))
}

f1 <- function(tab, beta = 1, ...) {
  fbeta(tab, beta = beta, ...)
}

fbeta <- function(tab, beta, ...) {
  if (beta < 0) {
    stop("beta needs to be non-negative.")
  }

  if (tab[1, 1] == 0) {
    if ((tab[2, 1] + tab[1, 2]) == 0) {
      return(1)
    } else {
      return(0)
    }
  } else {
    # as.numeric(2 * tab[1, 1] / (2 * tab[1, 1] + tab[1, 2] + tab[2, 2]))
    as.numeric((1 + beta^2) * tab[1, 1] / ( (1 + beta^2) * tab[1, 1] + beta^2 * tab[2, 1] + tab[1, 2] ))
  }
}

mcc <- function(tab, ...) {
  as.numeric((tab[1, 1] * tab[2, 2] - tab[1, 2] * tab[2, 1]) / sqrt(prod(prod(rowSums(tab)), prod(colSums(tab)))))
}

wilson_score_interval <- function(p, n, alpha = 0.05) {
  z <- stats::qnorm(1 - alpha/2)
  1 / (1 + 1/n * z^2) * (p + 1 / (2 * n) * z^2 + c(-z, z) * sqrt( 1 / n * p * (1 - p) + 1 / (4 * n^2) * z^2))
}
