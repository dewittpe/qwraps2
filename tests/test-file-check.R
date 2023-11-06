library(qwraps2)
e <- new.env()
example("file_check", local = e)
ls(envir = e)

################################################################################
#                                    Test 1                                    #
stopifnot(isTRUE(e$test1))
stopifnot(identical(attr(e$test1, "checks")[, c("absolute_path")], c(FALSE, FALSE)))
stopifnot(identical(attr(e$test1, "checks")[, c("case_match")], c(TRUE, TRUE)))
stopifnot(identical(attr(e$test1, "checks")[, c("current_md5sum")], c("7a3409e17f9de067740e64448a86e708", "798e52b92e0ae0e60f3f3db1273235d0")))
stopifnot(identical(attr(e$test1, "checks")[, c("expected_md5sum")], c(NA_character_, NA_character_)))
stopifnot(identical(attr(e$test1, "checks")[, c("md5check")], c(NA,NA)))

################################################################################
#                                    Test 2                                    #

# stopifnot(
#   identical(
#     e$test2
#   ,
#   TRUE
#   )
# )

################################################################################
#                               test case match                                #
stopifnot(all.equal(unclass(e$test_case_match), FALSE, check.attributes = FALSE))

################################################################################
e$md5check3

################################################################################

# verify error is thrown when stop = TRUE
x <-
  tryCatch(file_check(c("UNLIKELYFILENAME", "QWRAPS2_EXAMPLE_1.txt", "QWRAPS2_EXAMPLE_2.txt"), stop = TRUE)
           , error = function(e) e )
stopifnot(!is.null(x))
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
