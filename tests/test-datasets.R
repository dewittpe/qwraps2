library(qwraps2)

# verify the exported data sets
stopifnot(
 identical(
   data(package = "qwraps2")$results[, "Item"]
 , c("mtcars2", "pefr", "spambase")
 )
)

# the data sets should be lazyloaded, so the following should just work
mtcars2
pefr
spambase

################################################################################
##                               mtcars2 checks                               ##

# if these dim sizes fail and the dimensions have changed, make sure to update
# the R/datasets.R documentation too
stopifnot(inherits(mtcars2, "data.frame"))
stopifnot(identical(nrow(mtcars2), 32L))
stopifnot(identical(ncol(mtcars2), 19L))

stopifnot(identical(names(mtcars2)[1], "make"))
stopifnot(identical(names(mtcars2)[2], "model"))
stopifnot(identical(names(mtcars2)[3], "mpg"))
stopifnot(identical(names(mtcars2)[4], "disp"))
stopifnot(identical(names(mtcars2)[5], "hp"))
stopifnot(identical(names(mtcars2)[6], "drat"))
stopifnot(identical(names(mtcars2)[7], "wt"))
stopifnot(identical(names(mtcars2)[8], "qsec"))
stopifnot(identical(names(mtcars2)[9], "cyl"))
stopifnot(identical(names(mtcars2)[10], "cyl_character"))
stopifnot(identical(names(mtcars2)[11], "cyl_factor"))
stopifnot(identical(names(mtcars2)[12], "vs"))
stopifnot(identical(names(mtcars2)[13], "engine"))
stopifnot(identical(names(mtcars2)[14], "am"))
stopifnot(identical(names(mtcars2)[15], "transmission"))
stopifnot(identical(names(mtcars2)[16], "gear"))
stopifnot(identical(names(mtcars2)[17], "gear_factor"))
stopifnot(identical(names(mtcars2)[18], "carb"))
stopifnot(identical(names(mtcars2)[19], "test_date"))

# verify values are the same as in the mtcars data set
stopifnot(
  sapply(names(mtcars), function(nm) {identical(mtcars2[[nm]], mtcars[[nm]])})
    )

# verify the levels of the mtcars2$cyl_factor are as expected
stopifnot(identical(class(mtcars2[["cyl_factor"]]), "factor"))
stopifnot(
  identical(
    levels(mtcars2[["cyl_factor"]])
    ,
    c("6 cylinders", "4 cylinders", "8 cylinders")
    )
  )

# character version
stopifnot(identical(class(mtcars2[["cyl_character"]]), "character"))
stopifnot(
  identical(
    sort(unique(mtcars2[["cyl_character"]]))
    ,
    c("4 cylinders", "6 cylinders", "8 cylinders")
    )
  )

# gear_factor
stopifnot(identical(class(mtcars2[["gear_factor"]]), "factor"))
stopifnot(
  identical(
    levels(mtcars2[["gear_factor"]])
    ,
    c("3 forward gears", "4 forward gears", "5 forward gears")
    )
  )

# engine
stopifnot(identical(class(mtcars2[["engine"]]), "factor"))
stopifnot(
  identical(
    levels(mtcars2[["engine"]])
    ,
    c("V-shaped", "straight")
    )
  )

# transmission
stopifnot(identical(class(mtcars2[["transmission"]]), "factor"))
stopifnot(
  identical(
    levels(mtcars2[["transmission"]])
    ,
    c("Automatic", "Manual")
    )
  )

################################################################################
##                              spambase checks                               ##
# verify the names
stopifnot(
  identical(
    names(spambase)
  , c("word_freq_make", "word_freq_address", "word_freq_all", "word_freq_3d",
      "word_freq_our", "word_freq_over", "word_freq_remove",
      "word_freq_internet", "word_freq_order", "word_freq_mail",
      "word_freq_receive", "word_freq_will", "word_freq_people",
      "word_freq_report", "word_freq_addresses", "word_freq_free",
      "word_freq_business", "word_freq_email", "word_freq_you",
      "word_freq_credit", "word_freq_your", "word_freq_font", "word_freq_000",
      "word_freq_money", "word_freq_hp", "word_freq_hpl", "word_freq_george",
      "word_freq_650", "word_freq_lab", "word_freq_labs", "word_freq_telnet",
      "word_freq_857", "word_freq_data", "word_freq_415", "word_freq_85",
      "word_freq_technology", "word_freq_1999", "word_freq_parts",
      "word_freq_pm", "word_freq_direct", "word_freq_cs", "word_freq_meeting",
      "word_freq_original", "word_freq_project", "word_freq_re",
      "word_freq_edu", "word_freq_table", "word_freq_conference",
      "char_freq_semicolon", "char_freq_parenthesis",
      "char_freq_square_bracket", "char_freq_exclamation_point",
      "char_freq_dollar_sign", "char_freq_pound", "capital_run_length_average",
      "capital_run_length_longest", "capital_run_length_total", "spam")
  )
)

################################################################################
##                                End of File                                 ##
################################################################################
