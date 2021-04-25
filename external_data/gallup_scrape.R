
library(tidyverse)
library(rvest)

g <- "https://news.gallup.com/poll/247016/conservatives-greatly-outnumber-liberals-states.aspx"

gallup <- read_html(g) %>%
  html_node(".table-overflow") %>%
  html_table() %>%
  .[3:52, 1:6]

names(gallup) <- c("state", "conservative", "moderate", "liberal",
                   "conservative_average", "n")

write.csv(gallup, "external_data/gallup.csv", row.names = FALSE)
