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

states <- us18 %>%
  mutate(NMU = nmu_aggregate) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  group_by(DEM_STATE) %>%
  summarize(fent = weighted.mean(FENT_NMU, WT), bup = weighted.mean(BUP_NMU, WT), meth = weighted.mean(METH_NMU, WT),
            morph = weighted.mean(MORPH_NMU, WT), oxy = weighted.mean(OXY_NMU, WT), oxym = weighted.mean(OXYM_NMU, WT),
            tram = weighted.mean(TRAM_NMU, WT), tap = weighted.mean(TAP_NMU, WT), hyd = weighted.mean(HYD_NMU, WT),
            hydm = weighted.mean(HYDM_NMU, WT), suf = weighted.mean(SUF_NMU, WT), cod = weighted.mean(COD_NMU, WT),
            dihy = weighted.mean(DIHY_NMU, WT), benz = weighted.mean(BENZ_NMU, WT), stim = weighted.mean(STIM_NMU, WT),
            thc = weighted.mean(THC_NMU, WT), ktm = weighted.mean(KTM_NMU, WT))
names(states)[1] <- "state"
