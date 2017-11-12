# A list-of-lists for the summaries arg.  This object is of the basic form:
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

# Going to use markdow for the markup language in this example,  the original
# option will be reset at the end of the example.
orig_opt <- options()$qwraps2_markup
options(qwraps2_markup = "markdown")

# The summary table for the whole mtcars data set
whole_table <- summary_table(mtcars, our_summaries)
whole_table

# The summary table for mtcars grouped by am (automatic or manual transmission)
# This will generate one column for each level of mtcars$am
grouped_by_table <- summary_table(dplyr::group_by(mtcars, am), our_summaries)
grouped_by_table

# To build a table with a column for the whole data set and each of the am
# levels
cbind(whole_table, grouped_by_table)

# Adding a caption for a LaTeX table
print(whole_table, caption = "Hellow world", markup = "latex")

# A **warning** about grouped_df objects.  The attr
# If you use dplyr::group_by or
# dplyr::rowwise to manipulate a data set and fail to use dplyr::ungroup you
# might find a table that takes a long time to create and does not summarize the
# data as exapected.  For example, let's build a data set with twenty subjects
# and injury severity scores for head and face injuries.  We'll clean the data
# by finding the max ISS score for each subject and then reporting summary
# statistics there of.
set.seed(42)
library(dplyr)
dat <- dplyr::data_frame(id = letters[1:20],
                         head_iss = sample(1:6, 20, replace = TRUE, prob = 10 * (6:1)),
                         face_iss = sample(1:6, 20, replace = TRUE, prob = 10 * (6:1)))

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


subject_level_dat <-
  dat %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(iss = max(head_iss, face_iss))

# Want: a table with one column for all subjects with nine rows divided up into
# three row groups.  However, this will create a table with 20 columns, one for
# each subject
summary_table(subject_level_dat, iss_summary)

# Ungroup the data.frame to get the correct output
subject_level_dat %>%
  dplyr::ungroup() %>%
  summary_table(iss_summary)

# reset the original markup option that was used before this example was
# evaluated.
options(qwraps2_markup = orig_opt)

# Detailed examples in the vignette
# vignette("summary-statistics", package = "qwraps2")
