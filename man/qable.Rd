% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qable.R
\name{qable}
\alias{qable}
\title{Qable: an extended version of knitr::kable}
\usage{
qable(
  x,
  rtitle,
  rgroup,
  rnames = rownames(x),
  cnames = colnames(x),
  markup = getOption("qwraps2_markup", "latex"),
  ...
)
}
\arguments{
\item{x}{\code{matrix} or \code{data.frame} to be turned into a qable}

\item{rtitle}{a row grouping title. See Details.}

\item{rgroup}{a named numeric vector with the name of the row group and the
number of rows within the group.  \code{sum(rowgroup) == nrow(x)}.}

\item{rnames}{a character vector of the row names}

\item{cnames}{column names}

\item{markup}{the markup language to use, passed to the \code{format}
argument of \code{knitr::kable}.}

\item{...}{additional arguments passed to \code{knitr::kable}}
}
\value{
a character vector of the formatted numbers
}
\description{
Create a simple table via kable with row groups and rownames
similar to those of \code{hmisc::latex} or \code{htmlTable::htmlTable}.
}
\details{
\code{qable} is used as the printing method for \code{qwraps2_summary_table}
objects.  Check the vignettes for examples on building data summary tables.

\code{rtitle} can be used to add a title to the column constructed by the
\code{rgroup} and \code{rnames}.  The basic layout of a table generated by
\code{qable} is:

\tabular{lcc}{
 rtitle    \tab cnames[1] \tab cnames[2] \cr
 rgroup[1] \tab           \tab           \cr
 rnames[1] \tab x[1, 1]   \tab x[1, 2]   \cr
 rnames[2] \tab x[2, 1]   \tab x[2, 2]   \cr
 rnames[3] \tab x[3, 1]   \tab x[3, 2]   \cr
 rgroup[2] \tab           \tab           \cr
 rnames[4] \tab x[4, 1]   \tab x[4, 1]   \cr
 rnames[5] \tab x[5, 1]   \tab x[5, 1]   \cr
}

It should be noted that \code{escape = !(markup == "latex")} is passed to
\code{\link[knitr]{kable}}.
}
\examples{
data(mtcars)
qable(mtcars)
qable(mtcars, markup = "markdown")

# by make
make <- sub("^(\\\\w+)\\\\s?(.*)$", "\\\\1", rownames(mtcars))
make <- c(table(make))

# A LaTeX table with a vertical bar between each column
qable(mtcars[sort(rownames(mtcars)), ], rgroup = make)

# A LaTeX table with no vertical bars between columns
qable(mtcars[sort(rownames(mtcars)), ], rgroup = make, vline = "")

# a markdown table
qable(mtcars[sort(rownames(mtcars)), ], rgroup = make, markup = "markdown")

# define your own column names
qable(mtcars[sort(rownames(mtcars)), ],
      rgroup = make,
      cnames = toupper(colnames(mtcars)),
      markup = "markdown")

# define your own column names and add a title
qable(mtcars[sort(rownames(mtcars)), ],
      rtitle = "Make & Model",
      rgroup = make,
      cnames = toupper(colnames(mtcars)),
      markup = "markdown")

}
\seealso{
\code{\link[knitr]{kable}}

\code{summary_table}, for an example of build a data summary table, i.e., a
\dQuote{Table 1}.

For more detail on arguments you can pass to \code{...} look at the
non-exported functions form the knitr package \code{knitr:::kable_latex},
\code{knitr:::kable_markdown}, or others.
}
