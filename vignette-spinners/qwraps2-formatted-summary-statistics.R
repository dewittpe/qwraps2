#'---
#'title: "qwraps2: Formatted Summary Statistics"
#'author: "Peter E. DeWitt"
#'output:
#'  rmarkdown::html_vignette:
#'    toc: true
#'    number_sections: true
#'vignette: >
#'  %\VignetteIndexEntry{qwraps2: Formatted Summary Statistics}
#'  %\VignetteEngine{knitr::rmarkdown}
#'  %\VignetteEncoding{UTF-8}
#'---
#'
#+ label = "setup", include = FALSE
knitr::opts_chunk$set(collapse = TRUE)
#'
set.seed(42)
# /*  if interactive load_all, else, a library(qwraps2) is called below for the
# vignette
if (interactive()) {
  devtools::load_all()
} else {
# */
library(qwraps2)
# /*
}
# */
# define the markup language we are working in.
# options(qwraps2_markup = "latex") is also supported.
options(qwraps2_markup = "markdown")

#'
#' # Introduction
#'
#' It is common for a manuscript to require a data summary table.  The table might
#' include simple summary statistics for the whole sample and for subgroups.
#' There are several tools available to build such tables.  In my opinion, though,
#' most of those tools have nuances imposed by the creators/authors such that other
#' users need not only understand the tool, but also think like the authors.
#' I wrote this package to be as flexible and general as possible.  I hope you like
#' these tools and will be able to use them in your work.
#'
#' This vignette presents the use of the
{{ paste0(backtick(summary_table), ",") }}
{{ paste0(backtick(qsummary), ",") }}
#' and
{{ backtick(qable) }}
#' functions for quickly building data summary tables.  We will be using summary
#' statistic functions,
{{ paste0(backtick(mean_sd), ",") }}
{{ paste0(backtick(median_iqr), ",") }}
{{ paste0(backtick(n_perc), ",") }}
#' and others, from
{{ CRANpkg(qwraps2) }}
#' as well.
#'
#' ## Prerequisites Example Data Set
# /*
while (FALSE) {
# */
library(qwraps2)
# /*
}
# */
#'
#' We will use the data set
{{ backtick(mtcars2) }}
#' for the examples throughout this vignette. It is a modified and extended
#' version of the base R data set
{{ paste(backtick(mtcars), ".") }}
#' For details on the construction of the
{{ backtick(mtcars2) }}
#' data set please view the vignette:
{{ backtick(vignette('qwraps2-data-sets', package = "qwraps2")) }}
data(mtcars2)
str(mtcars2)

#'
#' # Review of Summary Statistic Functions and Formatting
#'
#'
#' ## Means and Standard Deviations
#'
{{ backtick(mean_sd) }}
#' returns the (arithmetic) mean and standard deviation for numeric
#' vector as a formatted character string. For example,
{{ backtick(mean_sd(mtcars2$mpg)) }}
#' returns the formatted string
{{ paste0(mean_sd(mtcars2$mpg), ".") }}
#' There are other options for formatting the character string:
mean_sd(mtcars2$mpg)
mean_sd(mtcars2$mpg, denote_sd = "paren")

#'
#' ## Mean and Confidence intervals
#'
#' If you need the mean and a confidence interval there is the function
{{ paste0(backtick(mean_ci), ".") }}
#' which returns a
{{ backtick(qwraps2_mean_ci) }}
#' object which is a
#' named vector with the mean, lower confidence limit, and the upper confidence
#' limit.   The printing method for
{{ backtick(qwraps2_mean_ci) }}
#' objects is a call to the
{{ backtick(frmtci) }}
#' function.  You can modify the formatting of the printed result by adjusting
#' the arguments passed to
{{ paste0(backtick(frmtci), ".") }}
mci <- mean_ci(mtcars2$mpg)
str(mci)
mci
print(mci, show_level = TRUE)

#'
#' ## Median and Inner Quartile Range
#'
#' Similar to the
{{ backtick(mean_sd) }}
#' function, the
{{ backtick(median_iqr) }}
#' returns the median and the inner quartile range (IQR) of a data vector.
median_iqr(mtcars2$mpg)

#'
#' ## Count and Percentages
#'
#' The
{{ backtick(n_perc) }}
#' function is the workhorse.
{{ backtick(n_perc0) }}
#' is also provided for ease of use in the same way that base R has
{{ backtick(paste) }}
#' and
{{ paste(backtick(paste0), ".") }}
{{ backtick(n_perc) }}
#' returns the n (%) with the percentage sign in the string,
{{ backtick(n_perc0) }}
#' omits the percentage sign from the string.  The latter is good for tables,
#' the former for in-line text.
#'
n_perc(mtcars2$cyl == 4)
n_perc0(mtcars2$cyl == 4)

n_perc(mtcars2$cyl_factor == 4)  # this returns 0 (0.00%)
n_perc(mtcars2$cyl_factor == "4 cylinders")
n_perc(mtcars2$cyl_factor == levels(mtcars2$cyl_factor)[2])

# The count and percentage of 4 or 6 cylinder vehicles in the data set is
n_perc(mtcars2$cyl %in% c(4, 6))

#'
#' ## Geometric Means and Standard Deviations
#'
#' Let $\left\{x_1, x_2, x_3, \ldots, x_n \right\}$ be a sample of size $n$ with
#' $x_i > 0$ for all $i.$  Then the geometric mean, $\mu_g,$ and geometric standard
#' deviation are
#'
#' $$
#' \begin{equation}
#'   \mu_g = \left( \prod_{i = 1}^{n} x_i \right)^{\frac{1}{n}} =
#'   b^{ \frac{1}{n} \sum_{i = 1}^{n} \log_{b} x_i },
#' \end{equation}
#' $$
#' and
#' $$
#' \begin{equation}
#'   \sigma_g = b ^ {
#'   \sqrt{
#'     \frac{\sum_{i = 1}^{n} \left( \log_{b} \frac{x_i}{\mu_g} \right)^2}{n}
#'   }
#'   }
#' \end{equation}
#' $$
#' or, for clarity,
#' $$
#' \begin{equation}
#'   \log_{b} \sigma_g =
#'   \sqrt{ \frac{\sum_{i = 1}^{n} \left( \log_{b} \frac{x_i}{\mu_g}
#'   \right)^2}{n}}
#' \end{equation}
#' $$
#'
#' When looking for the geometric standard deviation in R, the simple
{{ backtick(exp(sd(log(x)))) }}
#' does not match the population-denominator definition used here.  The
#' geometric standard deviation above uses $n,$ the full sample size, in the
#' denominator, whereas
#' the
{{ backtick(sd) }}
#' and
{{ backtick(var) }}
#' functions in R use the denominator $n - 1.$  To get
#' the geometric standard deviation one should adjust the result by multiplying the
#' variance by $(n - 1) / n$ or the standard deviation by $\sqrt{(n - 1) / n}.$
#' See the example below.
x <- runif(6, min = 4, max = 70)

# geometric mean
mu_g <- prod(x) ** (1 / length(x))
mu_g
exp(mean(log(x)))
1.2 ** mean(log(x, base = 1.2))

# geometric standard deviation
exp(sd(log(x)))  ## Uses the sample denominator, n - 1

# these equations are correct
sigma_g <- exp(sqrt(sum(log(x / mu_g) ** 2) / length(x)))
sigma_g

exp(sqrt((length(x) - 1) / length(x)) * sd(log(x)))

# geometric variance
geometric_variance <- exp(log(sigma_g) ** 2)
geometric_variance

#'
#' The functions
{{ paste0(backtick(gmean), ",") }}
{{ paste0(backtick(gvar), ",") }}
#' and
{{ backtick(gsd) }}
#' provide the geometric mean, variance, and standard deviation for a numeric
#' vector.  Here, geometric variance is defined as $\exp((\log \sigma_g)^2),$
#' not as $\sigma_g^2.$
gmean(x)
all.equal(gmean(x), mu_g)

gvar(x)
all.equal(gvar(x), sigma_g^2)       # This is supposed to be FALSE
all.equal(gvar(x), geometric_variance)

gsd(x)
all.equal(gsd(x), sigma_g)

#'
{{ backtick(gmean_sd) }}
#' will provide a quick way for reporting the geometric mean and
#' geometric standard deviation in the same way that
{{ backtick(mean_sd) }}
#' does for the arithmetic mean and arithmetic standard deviation:
gmean_sd(x)

#'
#'
#'
#' # Building a Data Summary Table
#'
#' The function
{{ backtick(summary_table) }}
#' appears to be the most widely used tool provided by the qwraps2 package.  As
#' such, that function has earned its own vignette.
#+ eval = FALSE
# /*
while(FALSE) { # do not evaluate this code
# */
vignette("qwraps2-summary-table")
# /*
}
# */
#'
#'
#' # Session Info
#'
print(sessionInfo(), local = FALSE)
