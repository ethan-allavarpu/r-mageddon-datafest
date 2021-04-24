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

state_nmu <- us18 %>%
  mutate(NMU = nmu_aggregate) %>%
  group_by(DEM_STATE) %>%
  summarize(fent = weighted.mean(FENT_NMU, WT, na.rm = TRUE), bup = weighted.mean(BUP_NMU, WT, na.rm = TRUE), meth = weighted.mean(METH_NMU, WT, na.rm = TRUE),
            morph = weighted.mean(MORPH_NMU, WT, na.rm = TRUE), oxy = weighted.mean(OXY_NMU, WT, na.rm = TRUE), oxym = weighted.mean(OXYM_NMU, WT, na.rm = TRUE),
            tram = weighted.mean(TRAM_NMU, WT, na.rm = TRUE), tap = weighted.mean(TAP_NMU, WT, na.rm = TRUE), hyd = weighted.mean(HYD_NMU, WT, na.rm = TRUE),
            hydm = weighted.mean(HYDM_NMU, WT, na.rm = TRUE), suf = weighted.mean(SUF_NMU, WT, na.rm = TRUE), cod = weighted.mean(COD_NMU, WT, na.rm = TRUE),
            dihy = weighted.mean(DIHY_NMU, WT, na.rm = TRUE), benz = weighted.mean(BENZ_NMU, WT, na.rm = TRUE), stim = weighted.mean(STIM_NMU, WT, na.rm = TRUE),
            thc = weighted.mean(THC_NMU, WT, na.rm = TRUE), ktm = weighted.mean(KTM_NMU, WT, na.rm = TRUE),
            total = sum(fent, bup, meth, morph, oxy, oxym, tram, tap, hyd, hydm, suf, cod, dihy, benz, stim, thc, ktm, na.rm = TRUE))
names(state_nmu)[1] <- "state"