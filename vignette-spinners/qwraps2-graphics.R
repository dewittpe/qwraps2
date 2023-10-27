#'---
#'title: "Graphics in qwraps2"
#'author: "Peter DeWitt"
#'output:
#'  rmarkdown::html_vignette:
#'    toc: true
#'    number_sections: true
#'vignette: >
#'  %\VignetteIndexEntry{Graphics in qwraps2}
#'  %\VignetteEngine{knitr::rmarkdown}
#'  %\VignetteEncoding{UTF-8}
#'---
#'
#+ label = "setup", include = FALSE
knitr::opts_chunk$set(collapse = TRUE)
#'
#' There are several graphics generated within qwraps2.  The naming convention
#' for the "quick" plots was inspired by the ggplot2 function qplot.  The
#' development, flexibility, and robustness of these functions vary.  Some "tips
#' and tricks" are provided.
# /*
if (interactive()) {
  devtools::load_all()
} else {
# */
library(qwraps2)
packageVersion("qwraps2")
# /*
}
# */
library(ggplot2)
#'
#'
#' # qacf: Autocorrelation Plots
#'
#' Generate an example data set.
set.seed(42)
n <- 250
x1 <- x2 <- x3 <- x4 <- vector('numeric', length = n)
x1[1] <- runif(1)
x2[1] <- runif(1)
x3[1] <- runif(1)
x4[1] <- runif(1)

# white noise
Z.1 <- rnorm(n, 0, 1)
Z.2 <- rnorm(n, 0, 2)
Z.3 <- rnorm(n, 0, 5)

for(i in 2:n)
{
  x1[i] <- x1[i-1] + Z.1[i] - Z.1[i-1] + x4[i-1] - x2[i-1]
  x2[i] <- x2[i-1] - 2 * Z.2[i] + Z.2[i-1] - x4[i-1]
  x3[i] <- x3[i-1] + x2[i-1] + 0.2 * Z.3[i] + Z.3[i-1]
  x4[i] <- x4[i-1] + runif(1, 0.5, 1.5) * x4[i-1]
}
testdf <- data.frame(x1, x2, x3, x4)

# Base acf plot for one variable
acf(testdf$x1)

# qacf plot for one variable
qacf(testdf$x1)
qacf(testdf$x1, show_sig = TRUE)

#+ fig.width = 5, fig.height = 5
# more than one variable
acf(testdf)
qacf(testdf)
qacf(testdf, show_sig = TRUE)

#'
#' ## Tips and tricks
#'
#' The implementation of qacf is based on the use of
{{ backtick(stats::acf) }}
#' to produce the statistics needed for the plot.  If you want to get at the
#' data itself to build your own acf plot you can extract the data frame from
#' the qacf return:
acf_plot_data <- qacf(testdf)$data
head(acf_plot_data)

#'
#' # qblandaltman: Bland Altman Plot
#'
#' Introduced in [@altman1983measurement] and [@bland1986statistical], the
#' qblandaltman method builds ggplot2 style Bland Altman plots.  For examples we
#' use the provided pefr data set which was transcribed from
#' [@bland1986statistical].  See
{{ backtick(vignette("qwraps2-data-sets", package = "qwraps2")) }}
#' For more details on that data set.
#'
#' The following replicates the figures in [@bland1986statistical].
#'
#' Using the first measurement only:
pefr_m1 <-
  cbind("Large" = pefr[pefr$measurement == 1 & pefr$meter == "Wright peak flow meter", "pefr"],
        "Mini"  = pefr[pefr$measurement == 1 & pefr$meter == "Mini Wright peak flow meter", "pefr"])

#'
#' A standard x-y style plot and a correlation coefficient suggests that the two
#' meters provide reasonably similar results.
cor(pefr_m1)

qplot(x = pefr_m1[, 1],
      y = pefr_m1[, 2],
      geom = "point",
      xlab = "Large Meter",
      ylab = "Mini Meter",
      xlim = c(0, 800),
      ylim = c(0, 800)) +
geom_abline(slope = 1)

#'
#' However, for many reasons, this the above is misleading.  One simple note:
#' correlation is not a metric for agreement, i.e., perfect agreement would be
#' shown if all the data points fell on the line of equality were as perfect
#' correlation occurs when the data points are co-linear.
#'
#' The Bland Altman plot plots the average value on the x-axis and the
#' difference in the measurements on the y-axis:
qblandaltman(pefr_m1) +
xlim(0, 800) +
ylim(-100, 100) +
xlab("Average of two meters") +
ylab("Difference in the measurements")

#'
#' There is no distinct relationship between the differences and the average,
#' but the difference in the measurements between the two meters was observed to
#' range between
{{ paste(range(apply(pefr_m1, 1, diff)), collapse = " and ") }}
#' liters per minute.  Such a discrepancy between the meters is not observable
#' from the simple x-y plot.
#'
#' Reliability, or repeatability, of measurements can also be investigated with
#' a Bland Altman plot.

pefr_mini <-
  cbind(m1 = pefr[pefr$measurement == 1 & pefr$meter == "Mini Wright peak flow meter", "pefr"],
        m2 = pefr[pefr$measurement == 2 & pefr$meter == "Mini Wright peak flow meter", "pefr"])

qblandaltman(pefr_mini)

#'
#'
#' # qkmplot: Kaplan Meier Plots
# create a survfit object
require(survival)
leukemia.surv <- survival::survfit(survival::Surv(time, status) ~ x, data = survival::aml)

# base R km plot
survival:::plot.survfit(leukemia.surv, conf.int = TRUE, lty = 2:3, col = 1:2)

#+ fig.width = 5
# qkmplot
qkmplot(leukemia.surv, conf_int = TRUE)

# build a data.frame for plotting km curves, this could be helpful for
# creating bespoke plots
leukemia_km_data <- qkmplot_bulid_data_frame(leukemia.surv)
head(leukemia_km_data, 3)

#+ fig.width = 5
qkmplot(leukemia_km_data)

#+ fig.width = 5
# intercept only plot
intonly_fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = survival::aml)
survival:::plot.survfit(intonly_fit, conf.int = TRUE)
qkmplot(intonly_fit, conf_int = TRUE)
#'
#' # qroc and qprc: Receiver Operating Curve and Precision Recall Curve
#'
#' Starting in
{{ Rpkg(qwraps2) }}
#' version 0.6.0, the methods for building these graphics have been
#' fundamentally changed as part of a major refactor of the
{{ backtick(confusion_matrix) }}
#' method which replaces a lot of the code that
{{ backtick(qroc) }}
#' and
{{ backtick(qprc) }}
#' were built on.
#'
#' Let's start with an example of having two models for predicing if the price
#' of a diamond exceeds $2,800.
data(diamonds, package = "ggplot2")

# Create two logistic regression models
fit1 <- glm(I(price > 2800) ~ cut * color, data = diamonds, family = binomial())
fit2 <- glm(I(price > 2800) ~ cut + color + clarity, data = diamonds, family = binomial())

#'
#' To build ROC and/or PRC plots start by building the confusion matrix for each
cm1 <- confusion_matrix(truth = diamonds$price > 2800, predicted = predict(fit1, type = "response"))
cm2 <- confusion_matrix(truth = diamonds$price > 2800, predicted = predict(fit2, type = "response"))

#'
#' Calling
{{ backtick(auc) }}
#' on the confusion matrices will generate list object with the need plotting
#' data and other results.
str(auc(cm1))
str(auc(cm2))

#'
#' Plotting ROC can be done by calling
{{ backtick(qroc) }}
#' on either the confusion matrix object or the object returned by
{{ backtick(auc) %s% "."}}
qroc(auc(cm1))
qroc(cm2)

#'
#' The plots generated a ggplot2 plots and can be modified like any other
#' ggplot2 plot, for example
# Add the AUC value to the plot title
qroc(cm2) + ggtitle(label = "Fit 2", subtitle = paste("AUROC =", frmt(auc(cm2)$auroc, digits = 3)))

#'
#' You can also use the results to build up a data set for plotting multiple
#' curves together.
plot_data <- rbind(cbind(Model = "fit1", auc(cm1)$roc_data),
                   cbind(Model = "fit2", auc(cm2)$roc_data))
qroc(plot_data) + aes(color = Model)

# with AUC in the legend
plot_data <- rbind(cbind(Model = paste("Fit1\nAUROC =", frmt(auc(cm1)$auroc, 3)), auc(cm1)$roc_data),
                   cbind(Model = paste("Fit2\nAUROC =", frmt(auc(cm2)$auroc, 3)), auc(cm2)$roc_data))
qroc(plot_data) +
  ggplot2::theme_bw() +
  ggplot2::aes(color = Model, linetype = Model) +
  ggplot2::theme(legend.position   = "bottom",
                 legend.text.align = 0.5)

#'
#' Building PRC plots is similar, just use prc insead of roc in all the above
#' calls.
qprc(auc(cm1))
qprc(cm2) + ggtitle(label = "Fit 2", subtitle = paste("AUPRC =", frmt(auc(cm2)$auprc, digits = 3)))
plot_data <- rbind(cbind(Model = "fit1", auc(cm1)$prc_data),
                   cbind(Model = "fit2", auc(cm2)$prc_data))
qprc(plot_data) + aes(color = Model)

#'
#' One caveat for plotting multiple PRC on one plot is that you'll need to
#' specify the prevalence value to plot.
plot_data <- rbind(cbind(Model = paste("Fit1\nAUPRC =", frmt(auc(cm1)$auprc, 3)), auc(cm1)$prc_data),
                   cbind(Model = paste("Fit2\nAUPRC =", frmt(auc(cm2)$auprc, 3)), auc(cm2)$prc_data))
auc(cm1)$prevalence
qprc(plot_data, prevalence = auc(cm1)$prevalence) +
  ggplot2::theme_bw() +
  ggplot2::aes(color = Model, linetype = Model) +
  ggplot2::theme(legend.position   = "bottom",
                 legend.text.align = 0.5)
#'
#' In general, the use of the
{{ backtick(roc_data) }}
#' and
{{ backtick(prc_data) }}
#' elements generated by
{{ backtick(auc) }}
#' can be used to inform as complex or simple a plot as you like.  The plotting
#' methods provided are just quick and easy ones to use.  Customization of the
#' plots is expected.
#'
#' # Session Info
#+ label = "sessioninfo"
sessionInfo()

# /* ---------------------------- END OF FILE ------------------------------- */

