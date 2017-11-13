#' @title Autocorrelation plot
#'
#' @description TO BE CONSTRUCTED
#'
#' @details
#' TO DO
#'
#' @param x object
#' @param conf_level confidence level for determining \sQuote{sigificant}
#' correlations.
#' @param show_sig logical, highlight significant correlations.
#' @param ... Other arguments passed to stats::acf
#'
#' @return a ggplot.  
#' 
#' @examples
#' # Generate a random data set
#' set.seed(42)
#' n <- 250
#' x1 <- x2 <- x3 <- x4 <- vector('numeric', length = n)
#' x1[1] <- runif(1)
#' x2[1] <- runif(1)
#' x3[1] <- runif(1)
#' x4[1] <- runif(1)
#' 
#' # white noise
#' Z.1 <- rnorm(n, 0, 1)
#' Z.2 <- rnorm(n, 0, 2)
#' Z.3 <- rnorm(n, 0, 5)
#' 
#' for(i in 2:n)
#' {
#' 	x1[i] <- x1[i-1] + Z.1[i] - Z.1[i-1] + x4[i-1] - x2[i-1]
#' 	x2[i] <- x2[i-1] - 2 * Z.2[i] + Z.2[i-1] - x4[i-1]
#' 	x3[i] <- x3[i-1] + x2[i-1] + 0.2 * Z.3[i] + Z.3[i-1]
#' 	x4[i] <- x4[i-1] + runif(1, 0.5, 1.5) * x4[i-1]
#' }
#' testdf <- data.frame(x1, x2, x3, x4)
#' 
#' # Base acf plot for one variable
#' acf(testdf$x1)
#' 
#' # qacf plot for one variable
#' qacf(testdf$x1)
#' qacf(testdf$x1, show_sig = TRUE)
#' 
#' # more than one variable
#' acf(testdf)
#' qacf(testdf)
#' qacf(testdf, show_sig = TRUE)
#' 
#' @export   
#' @rdname qacf
qacf <- function(x, conf_level = 0.95, show_sig = FALSE, ...) {
  UseMethod("qacf")
}

#' @export
qacf.default <- function(x, conf_level = 0.95, show_sig = FALSE, ...) {
  do.call(qacf.data.frame, list(x = data.frame(x), conf_level = conf_level, show_sig = show_sig, ...))
}

#' @export
#' @method qacf data.frame
qacf.data.frame <- function(x, conf_level = 0.95, show_sig = FALSE, ...) { 
  acf_data <- stats::acf(x, plot = FALSE, ...)
  ciline <- stats::qnorm((1 - conf_level) / 2) / sqrt(acf_data$n.used)

  lags <- dplyr::as_data_frame(acf_data$lag)
  acfs <- dplyr::as_data_frame(acf_data$acf)

  acf_df <- 
    dplyr::bind_cols(tidyr::gather(lags, key = 'key', value = 'lag'),
                     tidyr::gather(acfs, key = 'key', value = 'value')["value"])
  acf_df <-
    dplyr::mutate(acf_df, significant =  factor(abs(.data$value) > abs(ciline)))

  g <-
    ggplot2::ggplot() +
    ggplot2::aes_string(x = "lag", y = "value") +
    ggplot2::geom_bar(stat = "identity", position = "identity") +
    ggplot2::ylab(acf_data$type)

  if (ncol(x) > 1) {
    facets <- rep(apply(expand.grid(acf_data$snames, acf_data$snames),
                        1, function(x) {if(x[1] == x[2]) x[1] else paste(x, collapse = " & ")}),
                  each = dim(acf_data$acf)[1])
    facets_levels <-
      apply(expand.grid(acf_data$snames, acf_data$snames)[, 2:1],
            1, function(x) {if(x[1] == x[2]) x[1] else paste(x, collapse = " & ")})

    acf_df <-
      dplyr::mutate(acf_df, facets = factor(facets, levels = facets_levels))

    g <- g + ggplot2::facet_wrap( ~ facets, scales = "free_x")
  }

  if(show_sig) { 
    if (ncol(x) > 1) {
      g <- g +
        ggplot2::geom_hline(yintercept = ciline) + 
        ggplot2::geom_hline(yintercept = -ciline) + 
        ggplot2::aes_string(fill = "significant")
    } else {
      g <- g +
        ggplot2::geom_hline(yintercept = -ciline) + 
        ggplot2::aes_string(fill = "significant")
    }

  } 

  g <- ggplot2::`%+%`(g, acf_df)
  g
} 
