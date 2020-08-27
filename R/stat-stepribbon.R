#' Step ribbon statistic
#'
#' Provides stair step values for ribbon plots (Copied this from the
#' https://github.com/hrbrmstr/ggalt version 0.6.0, which is not yet on CRAN.
#' Some minor modifications to the file have been made).
#'
#' @inheritParams ggplot2::geom_ribbon
#' @param geom which geom to use; defaults to code{ribbon}
#' @param direction \code{hv} for horizontal-vertical steps, \code{vh} for
#'   vertical-horizontal steps
#' @references \url{https://groups.google.com/forum/?fromgroups=#!topic/ggplot2/9cFWHaH1CPs}
#' @examples
#' x <- 1:10
#' df <- data.frame(x=x, y=x+10, ymin=x+7, ymax=x+12)
#'
#' # horizontal-vertical steps (default)
#' gg <- ggplot2::ggplot(df, ggplot2::aes(x, y))
#' gg <- gg + ggplot2::geom_ribbon(ggplot2::aes(ymin=ymin, ymax=ymax),
#'                                 stat="stepribbon", fill="#b2b2b2",
#'                                 direction="hv")
#' gg <- gg + ggplot2::geom_step(color="#2b2b2b")
#' gg
#'
#' # vertical-horizontal steps (default)
#' gg <- ggplot2::ggplot(df, ggplot2::aes(x, y))
#' gg <- gg + ggplot2::geom_ribbon(ggplot2::aes(ymin=ymin, ymax=ymax),
#'                                 stat="stepribbon", fill="#b2b2b2",
#'                                 direction="vh")
#' gg <- gg + ggplot2::geom_step(color="#2b2b2b")
#' gg
#'
#' # The same plot calling stat_stepribbon directly
#' gg <- ggplot2::ggplot(df, ggplot2::aes(x, y))
#' gg <- gg + stat_stepribbon(mapping = ggplot2::aes(ymin=ymin, ymax=ymax),
#'                            fill="#b2b2b2", direction="vh")
#' gg <- gg + ggplot2::geom_step(color="#2b2b2b")
#' gg
#'
#' @export
stat_stepribbon <- function(mapping=NULL, data=NULL, geom="ribbon",
                            position="identity",
                            na.rm=FALSE, show.legend=NA, inherit.aes=TRUE,
                            direction="hv", ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatStepribbon,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      direction = direction,
      ...
    )
  )
}

#' @title Stat Step Ribbon
#' @description
#' Provides stair step values for ribbon plots (Copied this from the
#' https://github.com/hrbrmstr/ggalt version 0.6.0, which is not yet on CRAN.
#' Some minor modifications to the file have been made).
#' @format NULL
#' @usage NULL
#' @references \url{https://groups.google.com/forum/?fromgroups=#!topic/ggplot2/9cFWHaH1CPs}
#' @export
StatStepribbon <-
  ggplot2::ggproto(
    "StatStepRibbon", ggplot2::Stat,

    required_aes = c("x", "ymin", "ymax"),

    compute_group = function(data, scales, direction="hv",
                             yvars=c("ymin", "ymax"), ...) {
      stairstepn(data=data, direction=direction, yvars=yvars)
    }

  )

stairstepn <- function(data, direction="hv", yvars="y") {

  direction <- match.arg(direction, c("hv", "vh"))

  data <- as.data.frame(data)[order(data$x),]

  n <- nrow(data)

  if (direction == "vh") {
    xs <- rep(1:n, each=2)[-2*n]
    ys <- c(1, rep( 2:n, each=2))
  } else {
    ys <- rep(1:n, each=2)[-2*n]
    xs <- c(1, rep(2:n, each=2))
  }

  data.frame(
    x=data$x[xs],
    data[ys, yvars, drop=FALSE],
    data[xs, setdiff(names(data), c("x", yvars)), drop=FALSE]
  )

}
