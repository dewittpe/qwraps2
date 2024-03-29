#'---
#'title: "qwraps2: Data Sets"
#'author: "Peter E. DeWitt"
#'output:
#'  rmarkdown::html_vignette:
#'    toc: true
#'    number_sections: true
#'bibliography: references.bib
#'vignette: >
#'  %\VignetteIndexEntry{qwraps2: Data Sets}
#'  %\VignetteEngine{knitr::rmarkdown}
#'  %\VignetteEncoding{UTF-8}
#'---
#'
# /*
devtools::load_all()
# */
library(qwraps2)
packageVersion("qwraps2")
#'
#' # mtcars2
#'
#' The base R package datasets provides the mtcars data set.  The information in
#' mtcars is the fuel consumption and automobile characteristics of
{{ nrow(mtcars) }}
#' automobiles as reported in the March, April, June and July 1974 issues of
#' _Motor Trend_ magazine [@hocking1976biometrics].
#'
#' That dataset is modified and extended to provide support for examples within
#' the qwraps2 package documentation.  This vignette documents the construction
#' of mtcars2.
#'
#' # Construction of mtcars2
#'
#' Starting with the original mtcars:
mtcars2 <- mtcars
str(mtcars2)

#'
#' The cyl column provides the number of cylinders for in the engine of the
#' automobiles.  We will use two additional versions of this information, one as
#' character column and one as a factor.  Please note that the order of the
#' factor levels is intentionally set to be non-sequential.  This will help to
#' illustrate the ordering or results when using a factor or a character vector
#' as a grouping variable.
mtcars2$cyl_character <- paste(mtcars2$cyl, "cylinders")
mtcars2$cyl_factor    <- factor(mtcars2$cyl,
                                levels = c(6, 4, 8),
                                labels = paste( c(6, 4, 8), "cylinders"))

#'
#' Create other factor variables.
mtcars2$gear_factor <-
  factor(mtcars2$gear, levels = c(3, 4, 5), labels = paste(c(3, 4, 5), "forward gears"))

#'
#' Engine configuration: the
{{ backtick(vs) }}
#' column is an integer vector for indicating
#' V-shaped or straight.  The constructed column engine is a factor the same
#' information as a labeled factor.
mtcars2$engine <-
  factor(mtcars2$vs, levels = c(0, 1), labels = c("V-shaped", "straight"))

#'
#' Transmission: the
{{ backtick(am) }}
#' column is an integer vector indicating if the transmission is automatic or
#' manual.  We construct a
{{ backtick(transmission) }}
#' column to provide the same information as a factor.
mtcars2$transmission <-
  factor(mtcars2$am, levels = c(0, 1), labels = c("Automatic", "Manual"))

#'
#' The rownames of the mtcars2 data set provide the make and model of the
#' automobiles.  Here we will create columns for make and model and then omit
#' the rownames.
mtcars2$make  <- sub("^(\\w+)\\s(.+)", "\\1", rownames(mtcars2))
mtcars2$model <- sub("^(\\w+)\\s(.+)", "\\2", rownames(mtcars2))
rownames(mtcars2) <- NULL

#'
#' To have some dates to use in examples we are going to add an mostly
#' arbitrary date column to mtcars2.  Given that the data came from the March
#' through July issues of _Motor Trend_ in 1974, we will create a
{{ backtick(test_date) }}
#' column starting in January 1974 forward with one to three tests per week
#' through May 1974.  This assumes the data is in chronological order of the data.
set.seed(42)
mtcars2$test_date <-
  as.POSIXct("1974-01-03", tz = "GMT") +
  cumsum(sample(c(2, 3, 4, 7) * 3600 * 24, size = nrow(mtcars2), replace = TRUE))

#'
#' Lastly we will order the columns of mtcars2 so similar columns are next to
#' each other.
mtcars2 <-
  mtcars2[, c("make", "model", "mpg", "disp", "hp", "drat", "wt", "qsec",
              "cyl", "cyl_character", "cyl_factor",
              "vs", "engine",
              "am", "transmission",
              "gear", "gear_factor",
              "carb",
              "test_date")]

#'
#' # Summary of mtcars2
#'
#' mtcars2 is a data frame with
{{ nrow(mtcars2) }}
#' observations with
{{ ncol(mtcars2) }}
#' variables.  Some of the variables tell us the same information, but in
#' different formats.
str(mtcars2)

#'
#' | Element | Name          | Description                                                                        |
#' | ------: | :------------ | :---------------------------------------                                           |
#' | [,  1]  | make          | Vehicle Manufacturer                                                               |
#' | [,  2]  | model         | Vehicle model                                                                      |
#' | [,  3]  | mpg           | Miles/(US) gallon                                                                  |
#' | [,  4]  | disp          | Displacement (cu.in.)                                                              |
#' | [,  5]  | hp            | Gross horsepower                                                                   |
#' | [,  6]  | drat          | Rear axle ratio                                                                    |
#' | [,  7]  | wt            | Weight (1000 lbs)                                                                  |
#' | [,  8]  | qsec          | 1/4 mile time                                                                      |
#' | [,  9]  | cyl           | Number of cylinders                                                                |
#' | [, 10]  | cyl_character | Number of cylinders as a character string                                          |
#' | [, 11]  | cyl_factor    | Number of cylinders as a factor                                                    |
#' | [, 12]  | vs            | Engine (0 = V-shaped, 1 = straight)                                                |
#' | [, 13]  | engine        | same info as vs, but as a factor                                                   |
#' | [, 14]  | am            | Transmission (0 = automatic, 1 = manual)                                           |
#' | [, 15]  | transmission  | same info as am as a factor                                                        |
#' | [, 16]  | gear          | Number of forward gears                                                            |
#' | [, 17]  | gear_factor   | Number of forward gears as a factor                                                |
#' | [, 18]  | carb          | Number of carburetors                                                              |
#' | [, 19]  | test_date     | arbitrary date - created to approximate when the vehicle would have been assessed. |
#'
#'
#' # pefr
#'
#' Peak expiratory flow rate (pefr) data is used for examples within the qwraps2
#' package.  The data has been transcribed from [@bland1986statistical].
#'
#'> The sample comprised colleagues and family of J.M.B. chosen to give a wide
#'> range of PEFR but in no way representative of any defined population. Two
#'> measurements were made with a Wright peak flow meter and two with a mini
#'> Wright meter, in random order. All measurements were taken by J.M.B., using
#'> the same two instruments. (These data were collected to demonstrate the
#'> statistical method and provide no evidence on the comparability of these two
#'> instruments.) We did not repeat suspect readings and took a single reading as
#'> our measurement of PEFR. Only the first measurement by each method is used to
#'> illustrate the comparison of methods, the second measurements being used in
#'> the study of repeatability.
#'
#' The units of measure for the pefr are liters per minute (L/min).
#'
# copied text from the manuscript
pefr_table <-
  read.delim(
             header = FALSE,
             text = "
1	494	490	512	525
2	395	397	430	415
3	516	512	520	508
4	434	401	428	444
5	476	470	500	500
6	557	611	600	625
7	413	415	364	460
8	442	431	380	390
9	650	638	658	642
10	433	429	445	432
11	417	420	432	420
12	656	633	626	605
13	267	275	260	227
14	478	492	477	467
15	178	165	259	268
16	423	372	350	370
17	427	421	451	443")

#'
#' Build the data set
pefr <-
  expand.grid(subject = 1:17,
              measurement = 1:2,
              meter   = c("Wright peak flow meter", "Mini Wright peak flow meter"),
              KEEP.OUT.ATTRS = FALSE,
              stringsAsFactors = FALSE)
pefr$pefr <- do.call(c, pefr_table[, 2:5])

head(pefr)

#'
#' See
{{ backtick(vignette("qwraps2-graphics", package = "qwraps2")) }}
#' for examples using this data set, specifically in the construction and use of
#' Bland-Altman plots via
{{ backtick(qblandaltman) %s% "."}}
#'
#'
#' # Spambase
#'
#' Spambase [@spambase] is a useful data set for example needed a binary outcome
#' and several possible predictors.  The data set and documentation can be found
#' in this package in the directory on your machine at:
system.file("spambase", package = "qwraps2")

#'
#' The data set
{{ backtick(spambase) }}
#' was generated thusly:
nms <-
  scan(system.file("spambase", "spambase.names", package = "qwraps2")
       , what = character()
       , skip = 33
       , sep = "\n"
       , quiet = TRUE
  )
nms <- sapply(strsplit(nms, split = ":"), getElement, 1)
nms <- c(nms, "spam")

# clean up char_freq names
nms <-
  nms |>
  sub(";", "semicolon",         x = _, fixed = TRUE) |>
  sub("(", "parenthesis",       x = _, fixed = TRUE) |>
  sub("[", "square_bracket",    x = _, fixed = TRUE) |>
  sub("!", "exclamation_point", x = _, fixed = TRUE) |>
  sub("$", "dollar_sign",       x = _, fixed = TRUE) |>
  sub("#", "pound",             x = _, fixed = TRUE)

spambase <- read.csv(
    file = system.file("spambase", "spambase.data", package = "qwraps2")
    , header = FALSE
    , col.names = nms
)

#'
#' There are
{{ frmt(nrow(spambase)) }}
#' rows of data with
{{ frmt(ncol(spambase) - 1L)}}
#' predictors for the binary outcome
{{ backtick(spam) }}
#'
n_perc(spambase$spam) # count and percent of spam messages
#'
#'
#'
#' # References
#'
#'<div id="refs"></div>
#'
#' # Session Info
#+ label = "sessioninfo"
sessionInfo()

# /* ------------------------ Export the data set ------------------------------
save(mtcars2, file = "data/mtcars2.rda")
save(pefr, file = "data/pefr.rda")
save(spambase, file = "data/spambase.rda")
# */

# /* ---------------------------- END OF FILE ------------------------------- */
