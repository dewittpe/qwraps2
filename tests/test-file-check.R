library(qwraps2)
e <- new.env()
example("file_check", local = e)
ls(envir = e)

stopifnot(isTRUE(e$test1))

stopifnot(
  identical(
    e$test2
  ,
    structure(FALSE, checks = structure(list(path = c("UNLIKELYFILENAME", "QWRAPS2_EXAMPLE_1.txt", "QWRAPS2_EXAMPLE_2.txt"), absolute_path = c(FALSE, FALSE, FALSE), accessible = c(FALSE, TRUE, TRUE), current_md5sum = c(NA, "7a3409e17f9de067740e64448a86e708", "798e52b92e0ae0e60f3f3db1273235d0"), expected_md5sum = c(NA_character_, NA_character_, NA_character_), md5check = c(NA, NA, NA)), class = "data.frame", row.names = c("UNLIKELYFILENAME", "QWRAPS2_EXAMPLE_1.txt", "QWRAPS2_EXAMPLE_2.txt")), class = "qwraps2_file_check")
  )
)

# verify error is thrown when stop = TRUE
x <-
  tryCatch(file_check(c("UNLIKELYFILENAME", "QWRAPS2_EXAMPLE_1.txt", "QWRAPS2_EXAMPLE_2.txt"), stop = TRUE)
           , error = function(e) e )
stopifnot(!is.null(x))
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
