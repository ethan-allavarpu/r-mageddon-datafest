
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
  (state_data$party_votes[state_data$party == "democrat"] - state_data$party_votes[state_data$party == "republican"])/
    ((state_data$party_votes[state_data$party == "democrat"] + state_data$party_votes[state_data$party == "republican"] + state_data$party_votes[state_data$party == "other"] ))
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
nmu_aggregate <- us18[, nmus] %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  rowSums()

any_nmu <- us18[, nmus] %>%
  apply(1, sum, na.rm = TRUE) %>% as.logical()
  

nmu_politics <- us18 %>%
  mutate(NMU = nmu_aggregate, ANY = any_nmu) %>%
  filter(DEM_STATE != "DC") %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  group_by(DEM_STATE) %>%
  summarize(FENT = weighted.mean(FENT_NMU, WT),
            BUP = weighted.mean(BUP_NMU, WT),
            METH = weighted.mean(METH_NMU, WT),
            MORPH = weighted.mean(MORPH_NMU, WT),
            OXY = weighted.mean(OXY_NMU, WT),
            OXYM = weighted.mean(OXYM_NMU, WT),
            TRAM = weighted.mean(TRAM_NMU, WT),
            TAP = weighted.mean(TAP_NMU, WT),
            HYD = weighted.mean(HYD_NMU, WT),
            HYDM = weighted.mean(HYDM_NMU, WT),
            SUF = weighted.mean(SUF_NMU, WT),
            COD = weighted.mean(COD_NMU, WT),
            DIHY = weighted.mean(DIHY_NMU, WT),
            BENZ = weighted.mean(BENZ_NMU, WT),
            STIM = weighted.mean(STIM_NMU, WT),
            THC = weighted.mean(THC_NMU, WT),
            KTM = weighted.mean(KTM_NMU, WT),
            dast = weighted.mean(DAST_SUM, WT),
            aggregate = weighted.mean(NMU, WT, na.rm = TRUE),
            any = weighted.mean(ANY, WT)) %>%
  mutate(election = election_politics,
         ideological = gallup$liberal / gallup$conservative) %>%
  rename(state = DEM_STATE)
