library(qwraps2)

# it is very difficult to build good tests for plots as the rendered plot needs
# to viewed by a human.  These tests are just to make sure that the underlying
# code is generating the same basic thing.  A human inspection of the examples
# and vignettes is highly recommended.
#
# This testing script can be thought of as a sneaky way to get the examples for
# plots into the code coverage based on only tests.

################################################################################
#                                extract legend                                #
e <- new.env()
example("ggplot2_extract_legend", local = e, ask = FALSE)
ls(envir = e)

stopifnot(identical(names(e$temp), c("legend", "plot")))

################################################################################
e_qacf <- new.env()
example("qacf", local = e_qacf, ask = FALSE)

################################################################################
e_qroc <- new.env()
example("qroc", local = e_qroc, ask = FALSE)

################################################################################
e_qkmp <- new.env()
example("qkmplot", local = e_qkmp, ask = FALSE)

################################################################################
e_qba <- new.env()
example("qblandaltman", local = e_qba, ask = FALSE)

################################################################################
#                                 End of File                                  #
################################################################################
