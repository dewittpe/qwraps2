\dontrun{

# load ggplot2 and the diamonds data set
data(diamonds, package = "ggplot2")

# compare a simple regression to random noise
dat <- 
  data.frame(fitted(lm(price ~ poly(carat, 4), data = diamonds)),  # fitted values
             diamonds$price + rnorm(nrow(diamonds), sd = 0.2),     # observed with noise
             pi)                                                   # extra column
qblandaltman(dat)

# simple example
dat <- data.frame(eval1 = rpois(100, 3), eval2 = rpois(100, 3.4)) 
qblandaltman(dat)

ggplot2::last_plot() + ggplot2::theme_bw()

# Two plots in one ggplot object
set.seed(42)
dat1 <- data.frame(eval1 = rnorm(100), eval2 = rt(100, df = 1))
dat2 <- data.frame(eval1 = rpois(50, 3), eval2 = rpois(50, 4))

# individual plots
qblandaltman(dat1)
qblandaltman(dat2)

# combined plots
dat <- rbind(cbind(set = "rnorm", qblandaltman_build_data_frame(dat1)), 
             cbind(set = "rpois", qblandaltman_build_data_frame(dat2)))
qblandaltman(dat, generate_data = FALSE) + ggplot2::facet_wrap( ~ set)

}
