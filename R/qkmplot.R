#' @title Kaplan-Meier Plot
#'
#' @description TO BE CONSTRUCTED
#'
#' @details
#' TO DO
#'
#' @param x object
#' @param ... Other arguments passed to survival::plot.survfit
#'
#' @return a ggplot.  
#' 
#' @examples

 library(survival)
 fit <- leukemia.surv <- survfit(Surv(time, status) ~ x, data = aml) 
 plot(leukemia.surv, conf.int = T, lty = 2:3, col = 1:2)

 qkmplot(leukemia.surv) 

#'
#'
#' 
#' @export   
#' @rdname qkmplot
qkmplot <- function(x, ...) { 
  UseMethod("qkmplot") 
}

#' @export
qkmplot.default <- function(x, ...) { 
  qkmplot_ggplot(x, ...)
}

#' @export
qkmplot.survfit <- function(x, ...) { 
  qkmplot_ggplot(qkmplot_bulid_data_frame(x), ...)
}

#' @export
qkmplot.qwraps2_generated <- function(x, ...) { 
  qkmplot_ggplot(x, ...)
}

qkmplot_ggplot <- function(.data, ...) { 
  ggplot2::ggplot(.data) + 
  ggplot2::aes_string(x = "time", y = "surv", colour = "strata", fill = "strata") + 
  ggplot2::geom_step() + 
  ggplot2::ylim(c(0, 1)) + 
  ggplot2::ylab("Survivial") + 
  ggplot2::geom_point(data = dplyr::filter(.data, n.censor > 0), shape = 3, alpha = 0.9) 
}

#' @export   
#' @rdname qkmplot
qkmplot_bulid_data_frame <- function(x) { 
  plot_data <- data.frame(time = x[['time']], 
                          n.risk = x[['n.risk']],
                          n.event = x[['n.event']],
                          n.censor = x[['n.censor']],
                          surv = x[['surv']],
                          strata = rep(attr(x[['strata']], "names"), times = x[['strata']]), 
                          upper = x[['upper']],
                          lower = x[['lower']], 
                          stringsAsFactors = FALSE) 
  first_data <- plot_data[!duplicated(plot_data$strata), ]
  first_data$time <- 0
  first_data$surv <- 1
  first_data$n.risk <- NA
  first_data$n.event <- NA
  first_data$n.censor <- 0
  first_data$lower <- 1
  first_data$upper <- 1

  rbind(plot_data, first_data)
} 

