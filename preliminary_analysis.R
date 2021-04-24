us18 <- read.csv("US/us_18Q1.csv")
library(tidyverse)
biglist <- list()
length(biglist) <- 26 * 17
k <- 1
for (i in c(3:28, 153:158)) {
  for (j in c(seq(from = 30, by = 2, to = 62), 122:131, 159)) {
    biglist[[k]] <- tapply(us18[, j], us18[, i], mean, na.rm = TRUE)
    names(biglist)[[k]] <- paste(names(us18)[c(i, j)], collapse = ", ")
    k <- k + 1
  }
}