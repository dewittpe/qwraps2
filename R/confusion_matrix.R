#' @title Sensitivity, Specificity, and Confusion Matrices (Contingency Tables)
#'
#' @description Functions for calculating the sensitivity and specificity, along
#' with bootstrapped confidence intervals, of confusion matrices (contingency
#' tables).
#'
#' @param tab A 2-by-2 confusion_matrix matrix or ftable
#' @param formula column ~ row for building the confusion matrix
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
#' ## Does knowing if a diamond is more than 1.5 carats in weight tell us if the
#' ## price is more than $5,000?
#' 
#' data("diamonds", package = "ggplot2")
#' 
#' x <- ftable(I(price > 5000) ~ I(carat > 1.5), data = diamonds)
#' 
#' sensitivity(x)
#' specificity(x)
#' 
#' sensitivity(I(price > 5000) ~ I(carat > 1.5), data = diamonds)
#' specificity(I(price > 5000) ~ I(carat > 1.5), data = diamonds)
#' 
#' confusion_matrix(I(price > 5000) ~ I(carat > 1.5), data = diamonds)
#' print(confusion_matrix(I(price > 5000) ~ I(carat > 1.5), data = diamonds), digits = 4)
#' 
#' \donttest{
#' x <- confusion_matrix(I(price > 5000) ~ I(carat > 1.5), 
#'                       data = diamonds, 
#'                       boot = TRUE, 
#'                       boot_samples = 100L)
#' print(x, digits = 4)
#' }
#' 
#'
#' @export   
#' @rdname confusion_matrix 
sensitivity <- function(tab, ...) { 
  UseMethod("sensitivity")
}
#' @export
#' @rdname confusion_matrix 
specificity <- function(tab, ...) { 
  UseMethod("specificity")
}

#' @export
sensitivity.default <- function(tab, ...) { 
  if (length(dim(tab)) != 2 | any(dim(tab) != 2)) { stop("Incorrect dim(tab)") } 
  as.numeric(tab[2, 2] / sum(tab[, 2]))
}

#' @export
sensitivity.ftable <- function(tab, ...) { 
  if (any(dim(tab) != 2)) { stop("Incorrect dim(tab)") } 
  as.numeric(tab[2, 2] / sum(tab[, 1]))
}

#' @export
sensitivity.formula <- function(formula, data, ...) { 
  ftab <- stats::ftable(formula, data)
  sensitivity.ftable(ftab)
} 

#' @export
specificity.default <- function(tab, ...) { 
  if (length(dim(tab)) != 2 | any(dim(tab) != 2)) { stop("Incorrect dim(tab)") } 
  as.numeric(tab[1, 1] / sum(tab[, 1]))
}

#' @export
specificity.ftable <- function(tab, ...) {
  if (any(dim(tab) != 2)) { stop("Incorrect dim(tab)") } 
  as.numeric(tab[1, 1] / sum(tab[, 1]))
}

#' @export
specificity.formula <- function(formula, data, ...) { 
  ftab <- stats::ftable(formula, data)
  specificity.ftable(ftab)
}


#' @export
#' @rdname confusion_matrix
confusion_matrix <- function(formula, data, boot = FALSE, boot_samples = 1000L, alpha = 0.05) { 

  ftab <- stats::ftable(formula, data)
  sp   <- specificity(ftab)
  sn   <- sensitivity(ftab)

  if (boot) { 
    rows <- replicate(boot_samples, 
                      sample(seq(1, nrow(data), by = 1), nrow(data), replace = TRUE), 
                      simplify = FALSE)
    tabs <- lapply(rows, function(x) { stats::ftable(formula, data[x, ]) })
    sps  <- do.call(c, lapply(tabs, specificity))
    sns  <- do.call(c, lapply(tabs, sensitivity))
  }

  rtn <- 
    list(tab            = ftab,
         specificity    = sp, 
         sensitivity    = sn,
         specificity_ci = if (boot) stats::quantile(sps, probs = c(alpha / 2, 1 - alpha / 2)) else NA,
         sensitivity_ci = if (boot) stats::quantile(sns, probs = c(alpha / 2, 1 - alpha / 2)) else NA)

  class(rtn) <- c("confusion_matrix", class(rtn))
  attr(rtn, "boot") <- boot

  return(rtn)
}

print.confusion_matrix <- function(x, ...) { 
  print(x$tab)

  if (attr(x, "boot")) { 
    cat("\n",
        "Sensitivity: ", qwraps2::frmt(x$sensitivity, ...), " (", paste(qwraps2::frmt(x$sensitivity_ci, ...), collapse = ", "), ")\n",
        "Specificity: ", qwraps2::frmt(x$specificity, ...), " (", paste(qwraps2::frmt(x$specificity_ci, ...), collapse = ", "), ")\n\n",
        sep = "") 
  } else { 
    cat("\n",
        "Sensitivity: ", qwraps2::frmt(x$sensitivity, ...), "\n",
        "Specificity: ", qwraps2::frmt(x$specificity, ...), "\n\n",
        sep = "") 
  } 
}
