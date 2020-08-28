#'---
#'title: "Graphics in qwraps2"
#'author: "Peter DeWitt"
#'output:
#'  rmarkdown::html_vignette:
#'    toc: true
#'    number_sections: true
#'vignette: >
#'  %\VignetteIndexEntry{qwraps2-graphics}
#'  %\VignetteEngine{knitr::rmarkdown}
#'  %\VignetteEncoding{UTF-8}
#'---
#'
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
#' There are several graphics generated within qwraps2.  The naming convension
#' for the "quick" plots was inspired by the ggplot2 function qplot.  The
#' development, flexability, and robustness of these functions vary.  Some "tips
#' and tricks" are provided.
#'
#' # qacf : Autocorrelation Plots
# /* {{{ */
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

# more than one variable
acf(testdf)
qacf(testdf)
qacf(testdf, show_sig = TRUE)

#'
#' ## Tips and tricks
#'
#' The implimentation of qacf is based on the use of
{{ backtick(stats::acf) }}
#' to produce the statistics needed for the plot.  If you want to get at the
#' data itself to build your own acf plot you can extract the data frame from
#' the qacf return:
acf_plot_data <- qacf(testdf)$data
head(acf_plot_data)

# /* end of qacf }}} */
#'
#' # qblandaltman : Bland Altman Plot
# /* {{{ */
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
#' meters provide reasonablly similar results.
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
#' The Bland Altman plot plots the average value on the x-asis and the
#' difference in the measuremnts on the y-axis:
qblandaltman(as.data.frame(pefr_m1)) +
xlim(0, 800) +
ylim(-100, 100)

#'
# /* end of qblandaltman }}} */

#'
#' # Session Info
#+ label = "sessioninfo"
sessionInfo()

# /* ---------------------------- END OF FILE ------------------------------- */

