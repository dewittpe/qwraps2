library(reprex)

reprex({

  # namespaces and options:
  library(qwraps2)
  library(magrittr)
  library(dplyr)
  options(qwraps2_markup = "markdown")

  # build an example data set:
  set.seed(42)

  # I AM GUESING AT THIS FORM IF IT IS NOT CORRECT THEN ALL THAT FOLLOWS WILL BE
  # WRONG TOO.
  dat <- tibble(Type1 = factor(sample(LETTERS[1:2], 100, replace = TRUE), levels = LETTERS[1:2]),
                Type2 = factor(sample(LETTERS[3:4], 100, replace = TRUE), levels = LETTERS[3:5]),
                Var1  = sample(c(0L, 1L, NA_integer_), 100, replace = TRUE),
                Var2  = sample(c(0L, 1L, NA_integer_), 100, replace = TRUE),
                Var3  = sample(c(0L, 1L, NA_integer_), 100, replace = TRUE))

  print(dat, n = 3)
  summary(dat)

  # gather the data such that the Var1, Var2, and Var3 columns become a
  # key/value pairing and filter to only the rows with a value of 1.  That
  # should match what you suggested for the summary above.
  dat2 <-
    dat %>%
    tidyr::gather(., key = "key", value = "value", Var1:Var3) %>%
    dplyr::filter(value == 1)

  dat2

  # The qsummary will quickly build the list-of-lists needed
  qsummary(dat2)$key

  # build the tables and bind together
  by_type1 <- summary_table(x = group_by(dat2, Type1), summaries = qsummary(dat2)['key'])
  by_type2 <- summary_table(x = group_by(dat2, Type2), summaries = qsummary(dat2)['key'])
  whole    <- cbind(by_type1, by_type2)

  # print the result
  print(whole, rtitle = "summary", booktabs = TRUE)
},
outfile = "issue-0075"
)
