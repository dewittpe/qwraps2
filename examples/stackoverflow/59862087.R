#'
#' IceCreamToucan’s answer is very good. I’m posting this answer to offer a
#' different way to present the information.

library(dplyr)
library(qwraps2)

dat <- data.frame("type" = c("B","B","A","B","A","A","B","A","A","B","A","A","A","B","B","B"),
                  "num"  = c(3,0,0,9,6,0,4,1,1,5,6,1,3,0,0,0))

#'
#' When building the `dplyr::summarize` call you can use the `qwraps2::frmtci`
#' call to format the output of `qwraps2::mean_ci` into a character string of
#' length one.
#'
#' I would also recommend using the data pronoun `.data` so you can be explicit
#' about the variables to summarize.
dat %>%
  dplyr::group_by(type) %>%
  dplyr::summarize(N = n(),
                   mean.ci = qwraps2::frmtci(qwraps2::mean_ci(.data$num)),
                   Percent = qwraps2::n_perc(.data$num > 0))
