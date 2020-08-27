library(magrittr)
library(dplyr)  
library(qwraps2)
options(qwraps2_markup="markdown")

## Creating dummy data
group_1    <- rep(c("a", "b"), 5)
group_2    <- rep(1:2, each=5)
response_1 <- c(1, 1, 0, 0, 0, 0, 0, 0, 1, 0)
response_2 <- c(0, 1, 1, 1, 1, 0, 0, 0, 1, 0)
response_3 <- c(0, 1, 0, 1, 1, 1, 1, 1, 1, 0)
response_4 <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0)

data <- data.frame(group_1, group_2, response_1, response_2, response_3, response_4)

our_summary1 <- list(
  "Responses 1:2" = list(
                         "Response 1 and not Response 2"  = ~ qwraps2::n_perc0(response_1 == 1 & response_2 == 0, na_rm = TRUE), 
                         "Response 2 and not Response 1"  = ~ qwraps2::n_perc0(response_1 == 0 & response_2 == 1, na_rm = TRUE),
                         "!Response 1 and  Response 2"    = ~ qwraps2::n_perc0(response_1 == 1 & response_2 == 1, na_rm = TRUE),
                         "Neither Response 1 nor Response 2"    = ~ qwraps2::n_perc0(response_1 == 0 & response_2 == 0, na_rm = TRUE)
                         ),
  "Responses 3:4" = list("Response 3"=~ qwraps2::n_perc0(response_3 == 1,na_rm = TRUE),
                         "Response 4"=~ qwraps2::n_perc0(response_4 == 1,na_rm = TRUE))
)

all <- summary_table(dplyr::group_by(data, group_1), our_summary1)
sev <- summary_table(dplyr::group_by(data, group_2), our_summary1)
whole <- cbind(sev, all)
print(whole, rtitle="summary", booktabs = TRUE)

