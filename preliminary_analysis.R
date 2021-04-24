us18 <- read.csv("US/us_18Q1.csv")
biglist <- list()
length(biglist) <- 26 * 17
k <- 1
for (i in 3:28) {
  for (j in seq(from = 30, by = 2, to = 62)) {
    biglist[[k]] <- tapply(us18[, j], us18[, i], mean, na.rm = TRUE)
    names(biglist)[[k]] <- paste(names(us18)[c(i, j)], collapse = ", ")
    k <- k + 1
  }
}