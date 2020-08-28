#'---
#'title: "Construction of the pefr data set"
#'author: "Peter DeWitt"
#'output:
#'  rmarkdown::html_vignette:
#'    toc: true
#'    number_sections: true
#'bibliography: references.bib
#'vignette: >
#'  %\VignetteIndexEntry{pefr}
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
  expand.grid(subject = pefr[, 1],
              measurement = 1:2,
              meter   = c("Wright peak flow meter", "Mini Wright peak flow meter"),
              KEEP.OUT.ATTRS = FALSE,
              stringsAsFactors = FALSE)
pefr$pefr <- do.call(c, pefr_table[, 2:5])

pefr

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
save(pefr, file = "data/pefr.rda")
# */

# /* ---------------------------- END OF FILE ------------------------------- */

