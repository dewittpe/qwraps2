% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n_perc.R
\name{n_perc}
\alias{n_perc}
\alias{perc_n}
\alias{n_perc0}
\title{Count and Percentage}
\usage{
n_perc(
  x,
  digits = getOption("qwraps2_frmt_digits", 2),
  na_rm = FALSE,
  show_denom = "ifNA",
  show_symbol = TRUE,
  markup = getOption("qwraps2_markup", "latex"),
  ...
)

perc_n(
  x,
  digits = getOption("qwraps2_frmt_digits", 2),
  na_rm = FALSE,
  show_denom = "ifNA",
  show_symbol = FALSE,
  markup = getOption("qwraps2_markup", "latex"),
  ...
)

n_perc0(
  x,
  digits = 0,
  na_rm = FALSE,
  show_denom = "never",
  show_symbol = FALSE,
  markup = getOption("qwraps2_markup", "latex"),
  ...
)
}
\arguments{
\item{x}{a 0:1 or boolean vector}

\item{digits}{digits to the right of the decimal point to return in the
percentage estimate.}

\item{na_rm}{if true, omit NA values}

\item{show_denom}{defaults to "ifNA".  Other options are "always" or "never".}

\item{show_symbol}{if TRUE (default) the percent symbol is shown, else it is suppressed.}

\item{markup}{latex or markdown}

\item{...}{pass through}
}
\value{
a character vector of the formatted values
}
\description{
A function for calculating and formatting counts and
percentages.
}
\details{
Default behavior will return the count of successes and the percentage as "N
(pp%).  If there are missing values in the input vector the omission of such
can be controlled by setting \code{na.rm = TRUE}.  In this case, the number
of non-missing values will be reported by default.  Omission of the
non-missing values can be controlled by setting \code{show_denom = "never"}.

The function n_perc0 uses a set of default arguments which may be
advantageous for use in building tables.
}
\examples{

n_perc(c(0, 1,1, 1, 0, 0), show_denom = "always")
n_perc(c(0, 1,1, 1, 0, 0, NA), na_rm = TRUE)

n_perc(mtcars$cyl == 6)

set.seed(42)
x <- rbinom(4269, 1, 0.314)
n_perc(x)
n_perc(x, show_denom = "always")
n_perc(x, show_symbol = FALSE)

# n_perc0 examples
n_perc0(c(0, 1,1, 1, 0, 0))
n_perc0(mtcars$cyl == 6)

}
