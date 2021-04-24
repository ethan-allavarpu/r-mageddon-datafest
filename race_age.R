
library(googleway)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(gtrendsR)
library(usmap)

us18 <- read.csv("US/us_18Q1.csv") %>%
  select(-DATE, -QTIME, -START_DATE, -STATUS)



# By location
nmus <- str_subset(names(us18), "NMU\\b")
nmu_aggregate <- us18[, nmus] %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  rowSums()

race_age <- list()
length(race_age) <- 17
names(race_age) <- nmus
for (i in seq_len(17)) {
  ra <- us18 %>%
    mutate(NMU = nmu_aggregate) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    group_by("DEM_AGE", "DEM_RACE") %>%
    summarize(fent = mean(FENT_NMU), bup = mean(BUP_NMU), meth = mean(METH_NMU),
              morph = mean(MORPH_NMU), oxy = mean(OXY_NMU), oxym = mean(OXYM_NMU),
              tram = mean(TRAM_NMU), tap = mean(TAP_NMU), hyd = mean(HYD_NMU),
              hydm = mean(HYDM_NMU), suf = mean(SUF_NMU), cod = mean(COD_NMU),
              dihy = mean(DIHY_NMU), benz = mean(BENZ_NMU), stim = mean(STIM_NMU),
              thc = mean(THC_NMU), ktm = mean(KTM_NMU))
  ra[is.na(ra)] <- 0
  ra <- as.data.frame(ra[, c(1:2, i + 2)])
  colnames(ra)[3] <- "NMU"
  race_age[[i]] <- ra
}