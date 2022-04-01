library(qwraps2)
qs <- qsummary(mtcars2[c("cyl", "cyl_factor", "cyl_character")])

stopifnot(identical(qs[["cyl"]],
                    list("minimum"      = ~ qwraps2::frmt(min(cyl)),
                         "median (IQR)" = ~ qwraps2::median_iqr(cyl),
                         "mean (sd)"    = ~ qwraps2::mean_sd(cyl),
                         "maximum"      = ~ qwraps2::frmt(max(cyl)))))

stopifnot(identical(qs[["cyl_factor"]],
                  list("6 cylinders" = ~ qwraps2::n_perc(cyl_factor == "6 cylinders", digits = 0, show_symbol = FALSE),
                       "4 cylinders" = ~ qwraps2::n_perc(cyl_factor == "4 cylinders", digits = 0, show_symbol = FALSE),
                       "8 cylinders" = ~ qwraps2::n_perc(cyl_factor == "8 cylinders", digits = 0, show_symbol = FALSE))))

stopifnot(identical(qs[["cyl_character"]],
                  list("4 cylinders" = ~ qwraps2::n_perc(cyl_character == "4 cylinders", digits = 0, show_symbol = FALSE),
                       "6 cylinders" = ~ qwraps2::n_perc(cyl_character == "6 cylinders", digits = 0, show_symbol = FALSE),
                       "8 cylinders" = ~ qwraps2::n_perc(cyl_character == "8 cylinders", digits = 0, show_symbol = FALSE))))


