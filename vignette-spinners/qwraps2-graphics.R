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
# /*
devtools::load_all()
# */
library(qwraps2)
packageVersion("qwraps2")

#'
#' There are several graphics generated within qwraps2.  The naming convention
#' for the "quick" plots was inspired by the (deprecated)
{{ CRANpkg(ggplot2) }}
#' function
{{ backtick(qplot) %s% "." }}
#' The development, flexibility, and robustness of these functions vary.  Some
#' "tips and tricks" are provided.
#'
# /* ------------------------------------------------------------------------ */
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
# /* ------------------------------------------------------------------------ */
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

ggplot2::ggplot(data = as.data.frame(pefr_m1)) +
  ggplot2::aes(x = Large, y = Mini) +
  ggplot2::geom_point() +
  ggplot2::xlab("Large Meter") +
  ggplot2::ylab("Mini Meter") +
  ggplot2::xlim(0, 800) +
  ggplot2::ylim(0, 800) +
  ggplot2::geom_abline(slope = 1)

#'
#' However, for many reasons, the above is misleading.  One simple note:
#' correlation is not a metric for agreement, i.e., perfect agreement would be
#' shown if all the data points fell on the line of equality whereas perfect
#' correlation occurs when the data points are simply co-linear.
#'
#' The Bland Altman plot plots the average value on the x-axis and the
#' difference in the measurements on the y-axis:
# default plot
qblandaltman(pefr_m1)

# modified plot
ggplot2::last_plot() +
  ggplot2::xlim(0, 800) +
  ggplot2::ylim(-100, 100) +
  ggplot2::xlab("Average of two meters") +
  ggplot2::ylab("Difference in the measurements")

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
# /* ------------------------------------------------------------------------ */
#'
#' # qkmplot: Kaplan Meier Plots
#'
# create a survfit object
require(survival)
leukemia.surv <- survival::survfit(survival::Surv(time, status) ~ x, data = survival::aml)

# base R km plot
survival:::plot.survfit(leukemia.surv, conf.int = TRUE, lty = 2:3, col = 1:2)

#+ fig.width = 5
# qkmplot
qkmplot(leukemia.surv, conf_int = TRUE)

#'
#' The function
{{ backtick(qkmplot_bulid_data_frame) }}
#' can be used to generate a data.frame needed for building a KM plot.  This
#' could be helpful for creating bespoke plots.
leukemia_km_data <- qkmplot_bulid_data_frame(leukemia.surv)
head(leukemia_km_data, 3)

#+ fig.width = 5
qkmplot(leukemia_km_data)

#'
#' Intercept only models are easy to plot too.
#+ fig.width = 5
intonly_fit <- survival::survfit(survival::Surv(time, status) ~ 1, data = survival::aml)
survival:::plot.survfit(intonly_fit, conf.int = TRUE)
qkmplot(intonly_fit, conf_int = TRUE)

#'
# /* ------------------------------------------------------------------------ */
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
#' Let's start with an example of having two models for predicting if the price
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
{{ backtick(confusion_matrix) }}
#' will generate list object with the need plotting data and other results.
str(cm1)
str(cm2)

#'
#' Plotting ROC can be done by calling
{{ backtick(qroc) }}
qroc(cm2)

#'
#' The plots generated a ggplot2 plots and can be modified like any other
#' ggplot2 plot, for example
# Add the AUC value to the plot title
qroc(cm2) + ggplot2::ggtitle(label = "Fit 2", subtitle = paste("AUROC =", frmt(cm2$auroc, digits = 3)))

#'
#' You can also use the results to build up a data set for plotting multiple
#' curves together.
plot_data <- rbind(cbind(Model = "fit1", cm1$cm_stats),
                   cbind(Model = "fit2", cm2$cm_stats))
qroc(plot_data) + ggplot2::aes(color = Model)

# with AUC in the legend
plot_data <- rbind(cbind(Model = paste("Fit1\nAUROC =", frmt(cm1$auroc, 3)), cm1$cm_stats),
                   cbind(Model = paste("Fit2\nAUROC =", frmt(cm2$auroc, 3)), cm2$cm_stats))
qroc(plot_data) +
  ggplot2::theme_bw() +
  ggplot2::aes(color = Model, linetype = Model) +
  ggplot2::theme(legend.position   = "bottom",
                 legend.text.align = 0.5)

#'
#' Building PRC plots is similar, just use prc instead of roc in all the above
#' calls.
qprc(cm1)
qprc(cm2) + ggplot2::ggtitle(label = "Fit 2", subtitle = paste("AUPRC =", frmt(cm2$auprc, digits = 3)))
qprc(plot_data) + ggplot2::aes(color = Model)

#'
#' One caveat for plotting multiple PRC on one plot is that you'll need to
#' specify the prevalence value to plot.
plot_data <- rbind(cbind(Model = paste("Fit1\nAUPRC =", frmt(cm1$auprc, 3)), cm1$cm_stats),
                   cbind(Model = paste("Fit2\nAUPRC =", frmt(cm2$auprc, 3)), cm2$cm_stats))

qprc(plot_data, prevalence = cm1$prevalence) +
  ggplot2::theme_bw() +
  ggplot2::aes(color = Model, linetype = Model) +
  ggplot2::theme(legend.position   = "bottom",
                 legend.text.align = 0.5)

#'
# /* ------------------------------------------------------------------------ */
#'
#' # Session Info
#+ label = "sessioninfo"
sessionInfo()

# /* ---------------------------- END OF FILE ------------------------------- */
