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
         list("min"  = ~ min(.data$mpg),
              "mean" = ~ mean(.data$mpg),
              "mean &plusmn; sd" = ~ qwraps2::mean_sd(.data$mpg),
              "max"  = ~ max(.data$mpg)),
       "Weight" = 
         list("median" = ~ median(.data$wt)),
       "Cylinders" = 
         list("4 cyl: n (%)" = ~ qwraps2::n_perc0(.data$cyl == 4),
              "6 cyl: n (%)" = ~ qwraps2::n_perc0(.data$cyl == 6),
              "8 cyl: n (%)" = ~ qwraps2::n_perc0(.data$cyl == 8)))

# Going to use markdown for the markup language in this example,  the original
# option will be reset at the end of the example.
orig_opt <- options()$qwraps2_markup
options(qwraps2_markup = "markdown")

# The summary table for the whole mtcars data set
whole_table <- summary_table(mtcars, our_summaries)
whole_table

# The summary table for mtcars grouped by am (automatic or manual transmission)
# This will generate one column for each level of mtcars$am
grouped_by_table <-
  summary_table(dplyr::group_by(mtcars, .data$am), our_summaries)
grouped_by_table

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
library(magrittr)
dat <- dplyr::data_frame(id = letters[1:20],
                         head_iss = sample(1:6, 20, replace = TRUE, prob = 10 * (6:1)),
                         face_iss = sample(1:6, 20, replace = TRUE, prob = 10 * (6:1)))

iss_summary <-
  list("Head ISS" = 
       list("min"    = ~ min(.data$head_iss),
            "median" = ~ median(.data$head_iss),
            "max"    = ~ max(.data$head_iss)),
       "Face ISS" = 
       list("min"    = ~ min(.data$face_iss),
            "median" = ~ median(.data$face_iss),
            "max"    = ~ max(.data$face_iss)),
       "Max ISS" = 
       list("min"    = ~ min(.data$iss),
            "median" = ~ median(.data$iss),
            "max"    = ~ max(.data$iss)))


subject_level_dat <-
  dat %>%
    dplyr::group_by(.data$id) %>%
    dplyr::mutate(iss = max(head_iss, face_iss))

# Want: a table with one column for all subjects with nine rows divided up into
# three row groups.  However, this will create a table with 20 columns, one for
# each subject
summary_table(subject_level_dat, iss_summary)

# Ungroup the data.frame to get the correct output
subject_level_dat %>%
  dplyr::ungroup() %>%
  summary_table(iss_summary)


################################################################################
# The Default call will work with non-syntactically valid names and will
# generate a table with statistics defined by the qsummary call.
mtcars %>%
  dplyr::group_by(.data$cyl) %>%
  summary_table(.)

# Another example from the diamonds data
data("diamonds", package = "ggplot2")
diamonds["The Price"] <- diamonds$price
diamonds["A Logical"] <- sample(c(TRUE, FALSE), size = nrow(diamonds), replace = TRUE)
diamonds[["badcol"]] <- replicate(expr = list(c(1:34)), n = nrow(diamonds))

summary_table(diamonds)
summary_table(diamonds, qsummary(diamonds))

summary_table(dplyr::group_by(diamonds, .data$cut))

summary_table(dplyr::group_by(diamonds, .data$cut),
              list("My Summary of Price" =
                   list("min price" = ~ min(.data$price),
                        "IQR"       = ~ stats::IQR(.data$price))))

################################################################################
# Data sets with missing values
temp <- mtcars
temp$cyl[5] <- NA
temp$am[c(1, 5, 10)] <- NA
temp$am <- factor(temp$am, levels = 0:1, labels = c("Automatic", "Manual"))
temp$vs <- as.logical(temp$vs)
temp$vs[c(2, 6)] <- NA
qsummary(dplyr::select(temp, .data$cyl, .data$am, .data$vs))
summary_table(dplyr::select(temp, .data$cyl, .data$am, .data$vs))

################################################################################
# reset the original markup option that was used before this example was
# evaluated.
options(qwraps2_markup = orig_opt)

# Detailed examples in the vignette
# vignette("summary-statistics", package = "qwraps2")
