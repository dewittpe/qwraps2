#'
#' Adding a comparison row to each row group is not something that
#' `qwraps2::summary_table` will support directly.   This is because
#' the issue is related to the limitations of markdown and the complexity of
#' supporting all the different ways to implement spanning multicol of a table
#' in LaTeX.
#'
#' Using `qwraps2::summary_table` to generate the primary table is a good
#' starting point.  Building the output table itself will require some other
#' packages.
#'
#' 
#' With the release of qwraps2 version 0.5.0, the mtcars2 data is an exported
#' data set and does not need to be built explicitly.
library(qwraps2)
options(qwraps2_markup = "markdown")

summaries <- qsummary(mtcars2[, c("mpg", "wt", "gear_factor")])

by_cyl <-
  summary_table(mtcars2, summaries = summaries, by = "cyl_factor")

#'
#' Note that the output from `summary_table` is a character matrix.
str(by_cyl)

#'
#' Instead of Choen's D I'll report and F-statistic and p-value form an
#' analysis of variance.
mpg_comp <-
  paste(extract_fstat(lm(mpg ~ cyl_factor, data = mtcars2)),
        extract_fpvalue(lm(mpg ~ cyl_factor, data = mtcars2)),
        collapse = ", ")

wt_comp <-
  paste(extract_fstat(lm(wt ~ cyl_factor, data = mtcars2)),
        extract_fpvalue(lm(wt ~ cyl_factor, data = mtcars2)),
        collapse = ", ")

mpg_comp
wt_comp

#'
#' For building the table, there are many options.  Spanning multiple columns in
#' markdown is not trivial.  Different flavors of markdown will render the
#' tables differently.  Some will support multicolumn spanning, other flavors
#' will not.
#'
#' For a markdown table, I recommend having a new column with the comparison
#' reported.  For the `by_cyl` table I would put the F stat and p-value on the
#' rows where the mean is reported.  This puts the statistical test and results
#' on the line related to the summary statistic.
by_cyl2 <- cbind(by_cyl, "comparison" = "&nbsp;")
by_cyl2[grepl("mean", rownames(by_cyl2)), "comparison"] <- c(mpg_comp, wt_comp)

by_cyl2

#'
#' If, instead, the comparison is a new row, I like the idea of using the
#' summary to add a blank row then add the comparison to the blank row.
summaries[[1]] <- c(summaries[[1]], "comparison" = ~ qwraps2::frmt(""))
summaries[[2]] <- c(summaries[[2]], "comparison" = ~ qwraps2::frmt(""))

by_cyl3 <- summary_table(mtcars2, summaries, by = "cyl_factor")

by_cyl3[grepl("comparison", rownames(by_cyl3)), 1] <- c(mpg_comp, wt_comp)

by_cyl3

#'
#' This does not address spanning the multiple columns.  That issue is, at least
#' to my knowledge, non-trivial.  I would use different tools and methods
#' depending on the target file format.  If I'm going to build a .pdf I would be
#' working in LaTeX, not markdown, and use `\multicolumn{}{}{}` explicitly.  If
#' the target output was html I would build a html table explicitly.
#' [htmlTable](https://cran.r-project.org/package=htmlTable) is a great package
#' for that.  There are compatibility options with *might* help when building
#' .docx or other Office style outputs.
