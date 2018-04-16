data(mtcars)
mymtcars <- 
  dplyr::mutate(mtcars, cyl_factor = factor(cyl, levels = c(6, 4, 8)), cyl_char = as.character(cyl))

summarize_these <- 
  with(mymtcars,
       list("Cylinder (numeric)" = tab_summary(cyl),
            "Cylinder (factor)"  = tab_summary(cyl_factor),
            "Cylinder (character)" = tab_summary(cyl_char),
            "MGP" = tab_summary(mpg))
       ) 

test_that("tab_summary generates formulea for numeric vector", 
          {

            expect_equivalent(summarize_these[[1]], list(~ min(cyl), 
                                                         ~ qwraps2::median_iqr(cyl),
                                                         ~ qwraps2::mean_sd(cyl),
                                                         ~ max(cyl))) 
          })

test_that("tab_summary generates formulea for factor", 
          { 
            expect_equivalent(summarize_these[[2]], list(~ qwraps2::n_perc(cyl_factor == "6", digits = 0, show_symbol = FALSE), 
                                                         ~ qwraps2::n_perc(cyl_factor == "4", digits = 0, show_symbol = FALSE),
                                                         ~ qwraps2::n_perc(cyl_factor == "8", digits = 0, show_symbol = FALSE))) 
          })

test_that("tab_summary generates formulea for character", 
          { 
            expect_equivalent(summarize_these[[3]], list(~ qwraps2::n_perc(cyl_char == "4", digits = 0, show_symbol = FALSE), 
                                                         ~ qwraps2::n_perc(cyl_char == "6", digits = 0, show_symbol = FALSE),
                                                         ~ qwraps2::n_perc(cyl_char == "8", digits = 0, show_symbol = FALSE)))
          }) 
