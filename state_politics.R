
library(googleway)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(gtrendsR)
library(usmap)

  # using MIT data ----
    # primary = senate, secondary = state
get_ratio <- function(df, state_name) {
  democrats <- c("democrat", "democratic-farmer-labor", "democratic-npl")
  state_data <- df %>%
    filter(state == state_name, !is.na(party)) %>%
    # take into account affiliated parties
    mutate(party = ifelse(party %in% democrats, "democrat",
                          ifelse(party == "republican", party, "other"))) %>%
    group_by(party) %>%
    summarize(party_votes = sum(candidatevotes))
  # democrat-to-republican ratio
  state_data$party_votes[state_data$party == "democrat"] /
    state_data$party_votes[state_data$party == "republican"]
}
get_election_politics <- function(state_name) {
  if (state_name %in% unique(senate_election$state)) {
    ratio <- get_ratio(senate_election, state_name)
    if (length(ratio) != 1) {
      ratio <- get_ratio(state_election, state_name)
    }
  } else {
    ratio <- get_ratio(state_election, state_name)
  }
  ratio
}
senate_election <- read.csv("external_data/mit_senate.csv")
state_election <- read.csv("external_data/mit_state.csv")
election_politics <- map_dbl(unique(state_election$state), get_election_politics)

  # using Gallup data ----
gallup <- read.csv("external_data/gallup.csv")

  # combining with main data ----
us18 <- read.csv("US/us_18Q1.csv") %>%
  select(-DATE, -QTIME, -START_DATE, -STATUS)
nmus <- str_subset(names(us18), "NMU\\b")

nmu_politics <- us18 %>%
  filter(DEM_STATE != "DC") %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  group_by(DEM_STATE) %>%
  summarize(fent = weighted.mean(FENT_NMU, WT),
            bup = weighted.mean(BUP_NMU, WT),
            meth = weighted.mean(METH_NMU, WT),
            morph = weighted.mean(MORPH_NMU, WT),
            oxy = weighted.mean(OXY_NMU, WT),
            oxym = weighted.mean(OXYM_NMU, WT),
            tram = weighted.mean(TRAM_NMU, WT),
            tap = weighted.mean(TAP_NMU, WT),
            hyd = weighted.mean(HYD_NMU, WT),
            hydm = weighted.mean(HYDM_NMU, WT),
            suf = weighted.mean(SUF_NMU, WT),
            cod = weighted.mean(COD_NMU, WT),
            dihy = weighted.mean(DIHY_NMU, WT),
            benz = weighted.mean(BENZ_NMU, WT),
            stim = weighted.mean(STIM_NMU, WT),
            thc = weighted.mean(THC_NMU, WT),
            ktm = weighted.mean(KTM_NMU, WT),
            total = sum(fent, bup, meth, morph, oxy, oxym, tram, tap, hyd,
                        hydm, suf, cod, dihy, benz, stim, thc, ktm)) %>%
  mutate(election = election_politics,
         ideological = gallup$liberal / gallup$conservative) %>%
  rename(state = DEM_STATE)
