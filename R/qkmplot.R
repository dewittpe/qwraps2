#' @title Kaplan-Meier Plot
#'
#' @description A ggplot2 version of a Kaplan-Meier Plot
#'
#' @details
#' Functions to build, explicitly or implicitly, data.frames and then creating a
#' ggplot2 KM plot.
#'
#' More details and examples for graphics within qwraps2 are in the
#' vignette(\dQuote{qwraps2-graphics}, pacakge = {qwraps2})
#'
#' @param x object
#' @param conf_int logical if TRUE show the CI
#' @param ... Other arguments passed to survival::plot.survfit
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
#' qkmplot_bulid_data_frame(leukemia.surv)
#'
#' qrmst(leukemia.surv) # NaN for rmst.se in Nonmaintained strata as laste observation is an event
#' qrmst(leukemia.surv, 44)
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
  layers <- list(eval(substitute(ggplot2::aes(x = X, y = Y), list(X = as.name("time"), Y = as.name("surv")))),
                 if (!is.null(dat$strata)) {eval(substitute(ggplot2::aes(colour = X, fill = Y), list(X = as.name("strata"), Y = as.name("strata")))) } else {NULL},
                 ggplot2::geom_step(),
                 ggplot2::ylim(c(0, 1)),
                 ggplot2::ylab("Survivial"),
                 ggplot2::geom_point(data = dat[dat$n.censor > 0, ], shape = 3, alpha = 0.9))

  if (conf_int) {
    layers <- append(layers,
                     ggplot2::geom_ribbon(mapping = eval(substitute(ggplot2::aes(ymin = MN, ymax = MX), list(MN = as.name("lower"), MX = as.name("upper"))))
                                          , alpha = 0.2
                                          , stat = "stepribbon")
      )
  }

  ggplot2::ggplot(dat) + layers
}

#' @export
#' @rdname qkmplot
qkmplot_bulid_data_frame <- function(x) {
  UseMethod("qkmplot_bulid_data_frame")
}

#' @export
#' @rdname qkmplot
qkmplot_bulid_data_frame.survfit <- function(x) {
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

#' @param tau upper bound on time for restricted mean survival time estimate
#' @export
#' @rdname qkmplot
qrmst <- function(x, tau = Inf) {
  UseMethod("qrmst")
}

#' @export
#' @rdname qkmplot
qrmst.survfit <- function(x, tau = Inf) {
  d <- qkmplot_bulid_data_frame(x)
  d <- subset(d, d$time > 0)
  qrmst.qwraps2_generated(d, tau = tau)
}

#' @export
#' @rdname qkmplot
qrmst.qwraps2_generated <- function(x, tau = Inf) {
  d <- split(x, x$strata)
  for (i in seq_along(d)) {

    d[[i]] <- subset(d[[i]], d[[i]]$time <= tau)

    d[[i]]$timeL <-  c(0, d[[i]]$time[-length(d[[i]]$time)])
    d[[i]]$rsum <- d[[i]]$surv * (d[[i]]$time - d[[i]]$timeL)
    d[[i]]$rcsum <- rev(cumsum(rev(d[[i]]$rsum)))
    d[[i]]$rvsum <- (d[[i]]$rcsum^2 * d[[i]]$n.event) / (d[[i]]$n.risk * (d[[i]]$n.risk - d[[i]]$n.event))


    d[[i]] <-
        data.frame(strata = d[[i]]$strata[1],
                   tau    = max(d[[i]]$time),
                   rmst   = sum(d[[i]]$rsum),
                   rmst.se = sqrt(sum(d[[i]]$rvsum))
                   )
  }

  d <- do.call(rbind, d)
  d
}

