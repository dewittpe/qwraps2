#' @title Kaplan-Meier Plot
#'
#' @description A ggplot2 version of a Kaplan-Meier Plot
#'
#' @details
#' Functions to build, explicitly or implicitly, data.frames and then creating a
#' ggplot2 KM plot.
#'
#' More details and examples for graphics within qwraps2 are in the
#' vignette(\dQuote{qwraps2-graphics}, package = \dQuote{qwraps2})
#'
#' @param x object
#' @param conf_int logical if TRUE show the CI
#' @param ... Other arguments passed to survival::plot.survfit
#'
#' @return a ggplot.
#'
#' @examples
#' require(survival)
#'
#' leukemia.surv <- survival::survfit(survival::Surv(time, status) ~ x, data = survival::aml)
#'
#' qkmplot(leukemia.surv, conf_int = TRUE)
#'
#' qkmplot_bulid_data_frame(leukemia.surv)
#'
#' qrmst(leukemia.surv) # NaN for rmst.se in Nonmaintained strata as last observation is an event
#' qrmst(leukemia.surv, 44)
#'
#' # pbc examples
#' pbc_fit <-
#'   survival::survfit(
#'       formula = survival::Surv(time, status > 0) ~ trt
#'     , data = pbc
#'     , subset = !is.na(trt)
#'   )
#'
#' qkmplot(pbc_fit)
#' qkmplot(pbc_fit, conf_int = TRUE)
#'
#' qrmst(pbc_fit)
#' qrmst(pbc_fit)
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
qkmplot.qkmplot_data <- function(x, conf_int = FALSE, ...) {
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
                          upper = x[['upper']],
                          lower = x[['lower']],
                          stringsAsFactors = FALSE)

  if (!is.null(x$strata)) {
    plot_data$strata <- rep(attr(x[['strata']], "names"), times = x[['strata']])
    for( s in attr(x[['strata']], "names")) {
      if (min(plot_data$time[plot_data$strata == s]) > 0) {
        plot_data <- rbind(data.frame(time = 0,
                                      n.risk = max(plot_data$n.risk[plot_data$strata == s]),
                                      n.event = 0,
                                      n.censor = 0,
                                      surv = 1,
                                      upper = 1,
                                      lower = 1,
                                      strata = s)
                           , plot_data)
      }
    }
    plot_data <- plot_data[order(plot_data$strata, plot_data$time),]
  } else {
    if (min(plot_data$time > 0)) {
        plot_data <- rbind(data.frame(time = 0,
                                      n.risk = max(plot_data$n.risk),
                                      n.event = 0,
                                      n.censor = 0,
                                      surv = 1,
                                      upper = 1,
                                      lower = 1
                                      )
                           , plot_data)
    }
    plot_data <- plot_data[order(plot_data$time),]
  }

  class(plot_data) <- c("qkmplot_data", class(plot_data))
  plot_data
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
  # d <- subset(d, d$time > 0)
  qrmst.qkmplot_data(d, tau = tau)
}

#' @export
#' @rdname qkmplot
qrmst.qkmplot_data <- function(x, tau = Inf) {

  d <- split(x, x$strata)

  if (is.infinite(tau)) {
    tau <- min(sapply(d, function(x) {max(x$time)}))
  }

  for (i in seq_along(d)) {

    d[[i]] <- subset(d[[i]], d[[i]]$time <= tau)

    if(!isTRUE(all.equal(max(d[[i]][["time"]]), tau))) {
      taildf <- utils::tail(d[[i]], n = 1)
      taildf$time <- tau
      d[[i]] <- rbind(d[[i]], taildf)
    }

    d[[i]]$timeL <- c(0, d[[i]]$time[-length(d[[i]]$time)])
    d[[i]]$rsum  <- d[[i]]$surv * (d[[i]]$time - d[[i]]$timeL)
    d[[i]]$rcsum <- rev(cumsum(rev(d[[i]]$rsum)))
    d[[i]]$rvsum <- (d[[i]]$rcsum^2 * d[[i]]$n.event) / (d[[i]]$n.risk * (d[[i]]$n.risk - d[[i]]$n.event))

    d[[i]] <-
        data.frame(
          strata  = d[[i]]$strata[1]
        , rmst    = sum(d[[i]]$rsum)
        , rmtl    = tau - sum(d[[i]]$rsum)
        , rmst.se = sqrt(sum(d[[i]]$rvsum))
        , tau     = max(d[[i]]$time)
        )
  }

  d <- do.call(rbind, d)
  d
}
