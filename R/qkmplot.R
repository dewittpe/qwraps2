#' @title Kaplan-Meier Plot
#'
#' @description A ggplot2 version of a Kaplan-Meier Plot
#'
#' @details
#' Functions to build, explicitly or implicitly, data.frames and then creating a
#' ggplot2 KM plot.
#'
#' @param x object
#' @param conf_int logical if TRUE show the CI
#' @param ... Other arguments passed to survival::plot.survfit
#'
#' @seealso \code{vignette("qwraps2-graphics", package = "qwraps2")} for
#' additional examples.
#'
#' @return a ggplot.
#'
#' @examples
#' # create a survfit object
#' require(survival)
#' leukemia.surv <- survival::survfit(survival::Surv(time, status) ~ x, data = survival::aml)
#'
#' qkmplot(leukemia.surv, conf_int = TRUE)
#'
#' @export
#' @rdname qkmplot
qkmplot <- function(x, conf_int = FALSE, ...) {
  UseMethod("qkmplot")
}

#' @export
qkmplot.default <- function(x, conf_int = FALSE, ...) {
  qkmplot_ggplot(x, conf_int = conf_int, ...)
}

#' @export
qkmplot.survfit <- function(x, conf_int = FALSE, ...) {
  qkmplot_ggplot(qkmplot_bulid_data_frame(x), conf_int = conf_int, ...)
}

#' @export
qkmplot.qwraps2_generated <- function(x, conf_int = FALSE, ...) {
  qkmplot_ggplot(x, conf_int = conf_int, ...)
}

qkmplot_ggplot <- function(dat, conf_int = FALSE, ...) {
  layers <- list(ggplot2::aes_string(x = "time", y = "surv"),
                 if (!is.null(dat$strata)) {ggplot2::aes_string(colour = "strata", fill = "strata") } else {NULL},
                 ggplot2::geom_step(),
                 ggplot2::ylim(c(0, 1)),
                 ggplot2::ylab("Survivial"),
                 ggplot2::geom_point(data = dat[dat$n.censor > 0, ], shape = 3, alpha = 0.9))

  if (conf_int) {
    layers <- append(layers,
                     ggplot2::geom_ribbon(ggplot2::aes_string(ymin = "lower", ymax = "upper"), alpha = 0.2,
                                          stat = "stepribbon")
      )
  }

  ggplot2::ggplot(dat) + layers
}

#' @export
#' @rdname qkmplot
qkmplot_bulid_data_frame <- function(x) {
  plot_data <- data.frame(time = x[['time']],
                          n.risk = x[['n.risk']],
                          n.event = x[['n.event']],
                          n.censor = x[['n.censor']],
                          surv = x[['surv']],
                          # strata = rep(attr(x[['strata']], "names"), times = x[['strata']]),
                          upper = x[['upper']],
                          lower = x[['lower']],
                          stringsAsFactors = FALSE)
  if (!is.null(x$strata)) {
    plot_data$strata <- rep(attr(x[['strata']], "names"), times = x[['strata']])
    first_data <- plot_data[!duplicated(plot_data$strata), ]
  } else {
    first_data <- plot_data[1, ]
  }
  first_data$time <- 0
  first_data$surv <- 1
  first_data$n.risk <- NA
  first_data$n.event <- NA
  first_data$n.censor <- 0
  first_data$lower <- 1
  first_data$upper <- 1

  dat <- rbind(plot_data, first_data)
  class(dat) <- c("qwraps2_generated", class(dat))
  dat
}

