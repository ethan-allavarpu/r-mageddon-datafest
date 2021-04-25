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

two_var_relationship <- function(x, y) {
  race_age <- list()
  length(race_age) <- 18
  names(race_age) <- c(nmus, "TOTAL")
  us18$var1 <- us18[[x]]
  us18$var2 <- us18[[y]]
  ra <- us18 %>%
    mutate(NMU = nmu_aggregate) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    group_by(var1, var2) %>%
    summarize(fent = weighted.mean(FENT_NMU, WT), bup = weighted.mean(BUP_NMU, WT), meth = weighted.mean(METH_NMU, WT),
              morph = weighted.mean(MORPH_NMU, WT), oxy = weighted.mean(OXY_NMU, WT), oxym = weighted.mean(OXYM_NMU, WT),
              tram = weighted.mean(TRAM_NMU, WT), tap = weighted.mean(TAP_NMU, WT), hyd = weighted.mean(HYD_NMU, WT),
              hydm = weighted.mean(HYDM_NMU, WT), suf = weighted.mean(SUF_NMU, WT), cod = weighted.mean(COD_NMU, WT),
              dihy = weighted.mean(DIHY_NMU, WT), benz = weighted.mean(BENZ_NMU, WT), stim = weighted.mean(STIM_NMU, WT),
              thc = weighted.mean(THC_NMU, WT), ktm = weighted.mean(KTM_NMU, WT),
              total = sum(fent, bup, meth, morph, oxy, oxym, tram, tap, hyd, hydm, suf, cod, dihy, benz, stim, thc, ktm, na.rm = TRUE))
  for (i in seq_len(ncol(ra) - 2)) {
    ra[is.na(ra)] <- 0
    ra_new <- as.data.frame(ra[, c(1:2, i + 2)])
    colnames(ra_new) <- c(x, y, "NMU")
    race_age[[i]] <- ra_new
  }
  nmu <- numeric(0)
  for (i in seq_len(length(race_age) - 1)) {
    nmu <- c(nmu, race_age[[i]][["NMU"]])
  }
  par(mfrow = c(1, 2))
  for (i in seq_along(race_age)) {
    cor_heatmap <- ggplot(data = race_age[[i]], aes(x = race_age[[i]][, 1], y = race_age[[i]][, 2], fill = NMU)) +
      geom_tile()
    cor_heatmap <- cor_heatmap +
      scale_fill_gradient2(low = "white", high = "darkred", 
                           limits = if (i < 18) {range(nmu, na.rm = TRUE)} else{NULL}) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
      ggtitle(names(race_age)[i]) + xlab(x) + ylab(y)
    print(cor_heatmap)
  }
}

