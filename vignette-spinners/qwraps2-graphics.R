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
#' Introduced in [@altman1983measurement] and [@bland1986statistical],
#'
# /* end of qblandaltman }}} */

#'
#' # Session Info
#+ label = "sessioninfo"
sessionInfo()

# /* ---------------------------- END OF FILE ------------------------------- */

