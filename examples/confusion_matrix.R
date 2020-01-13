################################################################################
## Example 1
test  <- c(rep(1, 53), rep(0, 47))
truth <- c(rep(1, 20), rep(0, 33), rep(1, 10), rep(0, 37))
con_mat <- confusion_matrix(x = test, y = truth, positive = "1")
str(con_mat)

con_mat

con_mat$cells$true_positives  # 20
con_mat$cells$true_negatives  # 37
con_mat$cells$false_positives # 33
con_mat$cells$false_negatives # 10

con_mat_with_boot <- confusion_matrix(test, truth, positive = "1", boot = TRUE)
con_mat_with_boot



################################################################################
## Example 2: based on an example from the wikipedia page:
# https://en.wikipedia.org/wiki/Confusion_matrix

animals <-
  data.frame(Predicted = c(rep("Cat",    5 + 2 +  0),
                           rep("Dog",    3 + 3 +  2),
                           rep("Rabbit", 0 + 1 + 11)),
             Actual    = c(rep(c("Cat", "Dog", "Rabbit"), times = c(5, 2,  0)),
                           rep(c("Cat", "Dog", "Rabbit"), times = c(3, 3,  2)),
                           rep(c("Cat", "Dog", "Rabbit"), times = c(0, 1, 11))),
             stringsAsFactors = FALSE)

table(animals)

cats <- apply(animals, 1:2, function(x) ifelse(x == "Cat", "Cat", "Non-Cat"))

# Default calls, note the difference based on what is set as the 'positive'
# value.
confusion_matrix(cats[, "Predicted"], cats[, "Actual"], positive = "Cat")
confusion_matrix(cats[, "Predicted"], cats[, "Actual"], positive = "Non-Cat")

# Using a Formula
confusion_matrix(formula = I(Actual == "Cat") ~ I(Predicted == "Cat"),
                 data = animals,
                 positive = "TRUE")

confusion_matrix(formula = I(Actual == "Cat") ~ I(Predicted == "Cat"),
                 data = animals,
                 positive = "TRUE",
                 boot = TRUE)

################################################################################
## Example 3
russell <-
  data.frame(Pred  = c(rep(0, 2295), rep(0, 118), rep(1, 1529), rep(1, 229)),
             Truth = c(rep(0, 2295), rep(1, 118), rep(0, 1529), rep(1, 229)))

# The values for Sensitivity, Specificity, PPV, and NPV are dependent on the
# "positive" level.  By default, the first level of y is used.
confusion_matrix(x = russell$Pred, y = russell$Truth, positive = "0")
confusion_matrix(x = russell$Pred, y = russell$Truth, positive = "1")

confusion_matrix(Truth ~ Pred, data = russell, positive = "0")
confusion_matrix(Truth ~ Pred, data = russell, positive = "1")
