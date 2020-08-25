# A list-of-lists for the summaries arg.  This object is of the basic form:
# It is recommended that you use the .data pronoun in the functions, see
# help(topic = ".data", package = "rlang") for details on this pronoun.
# list("row group A" =
#      list("row 1A" = ~ <summary function>,
#           "row 2A" = ~ <summary function>),
#      "row group B" =
#      list("row 1B" = ~ <summary function>,
#           "row 2B" = ~ <summary function>,
#           "row 3B" = ~ <summary function>))

our_summaries <-
  list("Miles Per Gallon" =
         list("min"  = ~ min(mpg),
              "mean" = ~ mean(mpg),
              "mean &plusmn; sd" = ~ qwraps2::mean_sd(mpg),
              "max"  = ~ max(mpg)),
       "Weight" =
         list("median" = ~ median(wt)),
       "Cylinders" =
         list("4 cyl: n (%)" = ~ qwraps2::n_perc0(cyl == 4),
              "6 cyl: n (%)" = ~ qwraps2::n_perc0(cyl == 6),
              "8 cyl: n (%)" = ~ qwraps2::n_perc0(cyl == 8)))

# Going to use markdown for the markup language in this example,  the original
# option will be reset at the end of the example.
orig_opt <- options()$qwraps2_markup
options(qwraps2_markup = "markdown")

# The summary table for the whole mtcars data set
# whole_table <- summary_table_042(mtcars, our_summaries)
whole_table <- summary_table(mtcars, our_summaries)
whole_table

# The summary table for mtcars grouped by am (automatic or manual transmission)
# This will generate one column for each level of mtcars$am
grouped_by_table <-
  summary_table(mtcars, our_summaries, by = "am")
grouped_by_table

# an equivalent call if you are using the tidyverse:
summary_table(dplyr::group_by(mtcars, am), our_summaries)

# To build a table with a column for the whole data set and each of the am
# levels
cbind(whole_table, grouped_by_table)

# Adding a caption for a LaTeX table
print(whole_table, caption = "Hello world", markup = "latex")

# A **warning** about grouped_df objects.
# If you use dplyr::group_by or
# dplyr::rowwise to manipulate a data set and fail to use dplyr::ungroup you
# might find a table that takes a long time to create and does not summarize the
# data as expected.  For example, let's build a data set with twenty subjects
# and injury severity scores for head and face injuries.  We'll clean the data
# by finding the max ISS score for each subject and then reporting summary
# statistics there of.
set.seed(42)
dat <- data.frame(id = letters[1:20],
                  head_iss = sample(1:6, 20, replace = TRUE, prob = 10 * (6:1)),
                  face_iss = sample(1:6, 20, replace = TRUE, prob = 10 * (6:1)))
dat <- dplyr::group_by(dat, id)
dat <- dplyr::mutate(dat, iss = max(head_iss, face_iss))

iss_summary <-
  list("Head ISS" =
       list("min"    = ~ min(head_iss),
            "median" = ~ median(head_iss),
            "max"    = ~ max(head_iss)),
       "Face ISS" =
       list("min"    = ~ min(face_iss),
            "median" = ~ median(face_iss),
            "max"    = ~ max(face_iss)),
       "Max ISS" =
       list("min"    = ~ min(iss),
            "median" = ~ median(iss),
            "max"    = ~ max(iss)))

# Want: a table with one column for all subjects with nine rows divided up into
# three row groups.  However, the following call will create a table with 20
# columns, one for each subject because dat is a grouped_df
summary_table(dat, iss_summary)

# Ungroup the data.frame to get the correct output
summary_table(dplyr::ungroup(dat), iss_summary)


################################################################################
# The Default call will work with non-syntactically valid names and will
# generate a table with statistics defined by the qsummary call.
summary_table(mtcars, by = "cyl")

# Another example from the diamonds data
data("diamonds", package = "ggplot2")
diamonds["The Price"] <- diamonds$price
diamonds["A Logical"] <- sample(c(TRUE, FALSE), size = nrow(diamonds), replace = TRUE)

# the next two lines are equivalent.
summary_table(diamonds)
summary_table(diamonds, qsummary(diamonds))

summary_table(diamonds, by = "cut")

summary_table(diamonds,
              summaries =
              list("My Summary of Price" =
                   list("min price" = ~ min(price),
                        "IQR"       = ~ stats::IQR(price))),
              by = "cut")

################################################################################
# Data sets with missing values
temp <- mtcars
temp$cyl[5] <- NA
temp$am[c(1, 5, 10)] <- NA
temp$am <- factor(temp$am, levels = 0:1, labels = c("Automatic", "Manual"))
temp$vs <- as.logical(temp$vs)
temp$vs[c(2, 6)] <- NA
qsummary(dplyr::select(temp, cyl, am, vs))
summary_table(dplyr::select(temp, cyl, am, vs))

################################################################################
# binding tables together.  The original design and expected use of
# summary_table did not require a rbind, as all rows are defined in the
# summaries argument.  That said, here are examples of using cbind and rbind to
# build several different tables.
our_summary1 <-
  list("Miles Per Gallon" =
       list("min" = ~ min(mpg),
            "max" = ~ max(mpg),
            "mean (sd)" = ~ qwraps2::mean_sd(mpg)),
       "Displacement" =
       list("min" = ~ min(disp),
            "max" = ~ max(disp),
            "mean (sd)" = ~ qwraps2::mean_sd(disp)))

our_summary2 <-
  list(
       "Weight (1000 lbs)" =
       list("min" = ~ min(wt),
            "max" = ~ max(wt),
            "mean (sd)" = ~ qwraps2::mean_sd(wt)),
       "Forward Gears" =
       list("Three" = ~ qwraps2::n_perc0(gear == 3),
            "Four"  = ~ qwraps2::n_perc0(gear == 4),
            "Five"  = ~ qwraps2::n_perc0(gear == 5))
       )

tab1 <- summary_table(mtcars, our_summary1)
tab2 <- summary_table(dplyr::group_by(mtcars, am), our_summary1)
tab3 <- summary_table(dplyr::group_by(mtcars, vs), our_summary1)

tab4 <- summary_table(mtcars, our_summary2)
tab5 <- summary_table(dplyr::group_by(mtcars, am), our_summary2)
tab6 <- summary_table(dplyr::group_by(mtcars, vs), our_summary2)

cbind(tab1, tab2, tab3)
cbind(tab4, tab5, tab6)

# row bind is possible, but it is recommended to extend the summary instead.
rbind(tab1, tab4)
summary_table(mtcars, summaries = c(our_summary1, our_summary2))

\dontrun{
  cbind(tab1, tab4) # error because rows are not the same
  rbind(tab1, tab2) # error because columns are not the same
}

################################################################################
# reset the original markup option that was used before this example was
# evaluated.
options(qwraps2_markup = orig_opt)

# Detailed examples in the vignette
# vignette("summary-statistics", package = "qwraps2")
