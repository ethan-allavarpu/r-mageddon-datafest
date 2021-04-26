library(googleway)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(gtrendsR)
library(usmap)

us18 <- read.csv("US/us_18Q1.csv") %>%
  select(-DATE, -QTIME, -START_DATE, -STATUS)
us18 <- mutate(us18, )



nmus <- str_subset(names(us18), "NMU\\b")
nmu_vals <- us18[, nmus] %>%
  mutate_all(~replace(., is.na(.), 0))
tapply(as.numeric(as.matrix(nmu_vals)), rep(nmus, each = nrow(nmu_vals)), weighted.mean, w = us18$WT)

# By location
nmus <- str_subset(names(us18), "NMU\\b")
nmu_aggregate <- us18[, nmus] %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  rowSums()

any_nmu <- us18[, nmus] %>%
  apply(1, sum, na.rm = TRUE) %>% as.logical()

two_var_relationship <- function(x, y) {
  race_age <- list()
  length(race_age) <- 1
  names(race_age) <- "DAST"
  us18$var1 <- us18[[x]]
  us18$var2 <- us18[[y]]
  ra <- us18 %>%
    mutate(NMU = nmu_aggregate, ANY = any_nmu) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    group_by(var1, var2) %>%
    summarize(dast = weighted.mean(DAST_SUM, WT, na.rm = TRUE))
  for (i in seq_len(ncol(ra) - 2)) {
    ra[is.na(ra)] <- 0
    ra_new <- as.data.frame(ra[, c(1:2, i + 2)])
    colnames(ra_new) <- c(x, y, "DAST")
    race_age[[i]] <- ra_new
  }
  par(mfrow = c(1, 3))
  for (i in seq_along(race_age)) {
    cor_heatmap <- ggplot(data = race_age[[i]], aes(x = race_age[[i]][, 1], y = race_age[[i]][, 2], fill = race_age[[i]][, 3])) +
      geom_tile()
    cor_heatmap <- cor_heatmap +
      scale_fill_gradient2(low = "white", high = "darkred",
                           name = "DAST") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
      ggtitle("DAST Scores") + xlab(x) + ylab(y)
    print(cor_heatmap)
  }
}

