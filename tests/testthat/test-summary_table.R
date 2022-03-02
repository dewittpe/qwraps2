test_that("qsummary generates expected formulea",
          {
            qs <- qsummary(mtcars2[c("cyl", "cyl_factor", "cyl_character")])

            expect_equivalent(qs[["cyl"]],
                              list(~ qwraps2::frmt(min(cyl)),
                                   ~ qwraps2::median_iqr(cyl),
                                   ~ qwraps2::mean_sd(cyl),
                                   ~ qwraps2::frmt(max(cyl))))

            expect_equivalent(qs[["cyl_factor"]],
                              list(~ qwraps2::n_perc(cyl_factor == "6 cylinders", digits = 0, show_symbol = FALSE),
                                   ~ qwraps2::n_perc(cyl_factor == "4 cylinders", digits = 0, show_symbol = FALSE),
                                   ~ qwraps2::n_perc(cyl_factor == "8 cylinders", digits = 0, show_symbol = FALSE)))

            expect_equivalent(qs[["cyl_character"]],
                              list(~ qwraps2::n_perc(cyl_character == "4 cylinders", digits = 0, show_symbol = FALSE),
                                   ~ qwraps2::n_perc(cyl_character == "6 cylinders", digits = 0, show_symbol = FALSE),
                                   ~ qwraps2::n_perc(cyl_character == "8 cylinders", digits = 0, show_symbol = FALSE)))

          })

