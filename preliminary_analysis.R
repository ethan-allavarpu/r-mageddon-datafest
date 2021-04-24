
library(tidyverse)
us18 <- read.csv("US/us_18Q1.csv") %>%
  select(-DATE, -QTIME, -START_DATE, -STATUS)

# INFLUENCE OF SECTION ORDER ----
  # potentially insightful for questionnaire design
  # could run a bootstrap hypothesis test

# by section ----
sections <- c("FENT", "BUP", "METH", "MORPH", "OXY", "OXYM", "TRAM", "TAP",
              "HYD", "HYDM", "SUF", "COD", "DIHY")

order_section <- data.frame(matrix(nrow = 13))
for (i in seq_along(sections)) {
  section <- sections[i]
  proportion_table <- us18 %>%
    select(ORDER = str_c("ORDER_", section), USE = str_c(section, "_USE")) %>%
    group_by(ORDER) %>%
    summarize(PROPORTION = mean(USE)) %>%
    arrange(desc(PROPORTION)) %>%
    as.data.frame()
  names(proportion_table) <- c(str_c(section, "_ord"), str_c(section, "_prop"))
  order_section <- order_section %>%
    cbind(proportion_table)
}
order_section <- order_section[, -1]
rm(list = c("i", "section", "proportion_table"))

order_section %>%
  select(contains("ord")) %>%
  apply(MARGIN = 1, FUN = mean)

# by respondent ----
order_respondent <- integer(13)
for (i in seq_len(nrow(us18))) {
  order_vector <- as.integer(unname(us18[i, ] %>%
                                      select(str_c("ORDER_", sections))))
  use_vector <- as.integer(unname(us18[i, ] %>%
                                    select(str_c(sections, "_USE"))))
  order_respondent <- order_respondent + use_vector[order(order_vector)]
}
rm(list = c("i", "order_vector", "use_vector"))

# DEMOGRAPHICS AND NMU ----
nmus <- str_subset(names(us18), "NMU\\b")
nmu_props <- us18[, nmus] %>%
  mutate_all(~replace(., is.na(.), 0)) %>% # missing = no lifetime use
  colMeans()

# broad categories ----
dems <- c("DEM_GENDER", "DEM_AGE10", "DEM_STDNT", "DEM_VET", "DEM_HEALTH",
          "DEM_STATE", "DEM_HISPANIC", "DEM_RACE", "DEM_INCOME", "DEM_MARITAL",
          "DEM_EDU", "DEM_PREG") # missing for DEM_PREG = males
dem_nmu_summary <- function(dem_column_name) {
  dem_variable <- us18[[dem_column_name]]
  if (dem_column_name == "DEM_PREG") {
    dem_variable[is.na(dem_variable)] <- 0
  }
  dem_totals <- summary(as.factor(dem_variable))
  dem_nmu_df <- us18 %>%
    select(dem_column = dem_column_name, nmus) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    group_by(dem_column) %>%
    summarize(fent = sum(FENT_NMU), bup = sum(BUP_NMU), meth = sum(METH_NMU),
              morph = sum(MORPH_NMU), oxy = sum(OXY_NMU), oxym = sum(OXYM_NMU),
              tram = sum(TRAM_NMU), tap = sum(TAP_NMU), hyd = sum(HYD_NMU),
              hydm = sum(HYDM_NMU), suf = sum(SUF_NMU), cod = sum(COD_NMU),
              dihy = sum(DIHY_NMU), benz = sum(BENZ_NMU), stim = sum(STIM_NMU),
              thc = sum(THC_NMU), ktm = sum(KTM_NMU)) %>%
    select(-dem_column) %>%
    apply(2, function(x) {x / dem_totals}) %>%
    apply(1, function(y) {y / nmu_props}) %>% t()
  rownames(dem_nmu_df) <- levels(as.factor(dem_variable))
  dem_nmu_df
}
dem_nmu <- map(dems, dem_nmu_summary)
names(dem_nmu) <- dems
  # eye-balling observations:
    # GENDER: 1 (male) is ~3 times of 2 (female)
    # AGE10: 6 (oldest category) < 5 < 4 < 1 (youngest) < 3 < 2 for all drugs
    # STDNT: 1 (yes) is ~2-6 times larger than 0 (no) for all drugs
    # VET: 1 (yes) tends to be larger than 0 (no), but generally comparable
    # HEALTH: 1 (yes) larger than 0 (no) for all drugs, up to >7 times as large
    # STATE: top and bottom states vary by drug
    # HISPANIC: 1 (yes) largest for all drugs, up to >5 as large as next
    # RACE: order varies by drug; generally 5 & 6 = smallest; 4 & 7 = largest
    # INCOME: order varies by drug; 11 always smallest; 9 usually largest
    # MARITAL: order varies by drug; 2 (widowed) always smallest;
      # largest is usually 4 (never married) and occasionally 1 (married)
    # EDU: order varies by drug; 8 (doctorate) is largest for all except THC
    # PREG: 0 (no) is larger than 1 (yes) for all drugs, up to >8 as large

# college students ----
nmu_aggregate <- us18[, nmus] %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  rowSums()
college_nmu <- as_tibble(us18) %>%
  bind_cols(nmu = nmu_aggregate) %>%
  filter(DEM_COLLEGE == 1)

  # Q13: similar between 0 (public) and 1 (private) for both mean and variance
college_nmu %>%
  group_by(COLLEGE_TYPE) %>%
  summarize(mean(nmu), sd(nmu))

  # Q14: mean of 4 (online university) is only a bit smaller than 1 (two-year)
    # or 2 (four-year) at ~0.7-0.9; 5 (trade school) is clearly largest at ~2.5
    # 4 also has largest variance -> likely not all students engage in NMU,
    # but those who do, consume multiple drugs
college_nmu %>%
  group_by(COLLEGE_LENGTH) %>%
  summarize(mean(nmu), sd(nmu))

  # Q15: 0 (<5000) and 4 (not sure) are noticeably smaller; 1-3 comparable
    # for both mean and variance
college_nmu %>%
  group_by(COLLEGE_SIZE) %>%
  summarize(mean(nmu), sd(nmu))

  # Q16: 1 (yes) is substantially larger than 0 (no) for both mean and variance
college_nmu %>%
  group_by(COLLEGE_HOUSING) %>%
  summarize(mean(nmu), sd(nmu))

  # Q17: 1 (yes) is substantially larger than 0 (no) for both mean and variance
    # mean and variance of 0 is similar to that for 0 of Q16 -> could be a
    # baseline for college students without influence of large college
    # institutions e.g. campus housing and fraternities/sororities?
college_nmu %>%
  group_by(COLLEGE_GREEK) %>%
  summarize(mean(nmu), sd(nmu))

# MENTAL ILLNESS AND NMU ----
ments <- str_subset(names(us18), "MENT_")
ments <- ments[1:(length(ments) - 2)] # drop OTH (other) and NONE
ment_nmu_summary <- function(ment_column_name) {
  ment_totals <- summary(as.factor(us18[[ment_column_name]]))
  ment_nmu_df <- us18 %>%
    select(ment_column = ment_column_name, nmus) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    group_by(ment_column) %>%
    summarize(fent = sum(FENT_NMU), bup = sum(BUP_NMU), meth = sum(METH_NMU),
              morph = sum(MORPH_NMU), oxy = sum(OXY_NMU), oxym = sum(OXYM_NMU),
              tram = sum(TRAM_NMU), tap = sum(TAP_NMU), hyd = sum(HYD_NMU),
              hydm = sum(HYDM_NMU), suf = sum(SUF_NMU), cod = sum(COD_NMU),
              dihy = sum(DIHY_NMU), benz = sum(BENZ_NMU), stim = sum(STIM_NMU),
              thc = sum(THC_NMU), ktm = sum(KTM_NMU)) %>%
    select(-ment_column) %>%
    apply(2, function(x) {x / ment_totals}) %>%
    apply(1, function(y) {y / nmu_props}) %>% t()
  rownames(ment_nmu_df) <- names(ment_totals)
  ment_nmu_df
}
ment_nmu <- map(ments, ment_nmu_summary)
names(ment_nmu) <- ments
  # eye-balling observations:
    # ANX: 1 > 0 for all drugs; by up to >3 times
    # ADHD: 1 > 0 for all drugs; by ~2-5 times each
    # AUT: 1 > 0 for all drugs; by >2 times each and up to >16 times
    # BIP: 1 > 0 for all drugs; by ~2-5 times each
    # BPD: 1 > 0 for all drugs; by >3 times each and up to >9 times
    # DEP: 1 > 0 for all drugs except TAP; by ~2 times mostly
    # EAT: 1 > 0 for all drugs; by >2 times each and up to >6 times
    # OCD: 1 > 0 for all drugs; by ~2-3+ times each
    # PANIC: 1 > 0 for all drugs; by up to ~3 times
    # PPD: 1 > 0 for all drugs; by ~2-4+ times each
    # PTSD: 1 > 0 for all drugs except SUF and TAP; by ~2+ times mostly
    # SCH: 1 > 0 for all drugs; by up to >3 times

# ALCOHOL/TOBACCO USE ----
  # clear jump from 1 (0 drinks/week) and 2 (1-7) to 3 (8-14) and 4 (15-21)
  # 5 (22 and above) above 3 and 4 for some drugs but below for others
us18 %>%
  select(ALC_USE, nmus) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  group_by(ALC_USE) %>%
  summarize(fent = mean(FENT_NMU), bup = mean(BUP_NMU), meth = mean(METH_NMU),
            morph = mean(MORPH_NMU), oxy = mean(OXY_NMU),
            oxym = mean(OXYM_NMU), tram = mean(TRAM_NMU), tap = mean(TAP_NMU),
            hyd = mean(HYD_NMU), hydm = mean(HYDM_NMU), suf = mean(SUF_NMU),
            cod = mean(COD_NMU), dihy = mean(DIHY_NMU), benz = mean(BENZ_NMU),
            stim = mean(STIM_NMU), thc = mean(THC_NMU), ktm = mean(KTM_NMU))

  # 1 (currently use) is higher than 2 (never) and 3 (previously used) for all
  # drugs; 3 is up to ~2 times of 2 for some drugs but comparable on others
us18 %>%
  select(TOB_USE, nmus) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  group_by(TOB_USE) %>%
  summarize(fent = mean(FENT_NMU), bup = mean(BUP_NMU), meth = mean(METH_NMU),
            morph = mean(MORPH_NMU), oxy = mean(OXY_NMU),
            oxym = mean(OXYM_NMU), tram = mean(TRAM_NMU), tap = mean(TAP_NMU),
            hyd = mean(HYD_NMU), hydm = mean(HYDM_NMU), suf = mean(SUF_NMU),
            cod = mean(COD_NMU), dihy = mean(DIHY_NMU), benz = mean(BENZ_NMU),
            stim = mean(STIM_NMU), thc = mean(THC_NMU), ktm = mean(KTM_NMU)) %>% View()

# RELATIONSHIP BETWEEN NMU AND DAST-10 ----
dasts <- str_c("DAST_", 1:10)
dast_props <- us18 %>%
  select(dasts) %>%
  mutate(DAST_3 = ifelse(DAST_3 == 1, 0, 1)) %>% # 1 is desirable for DAST_3
  colMeans()
nmu_dast_summary <- function(nmu_column_name) {
  nmu_column <- us18[[nmu_column_name]]
  nmu_column[is.na(nmu_column)] <- 0
  nmu_dast_sums <- vapply(dasts, function(x) {
    sum(us18[[x]] & nmu_column, na.rm = TRUE)
  }, numeric(1))
  nmu_dast <- (nmu_dast_sums / sum(nmu_column, na.rm = TRUE)) / dast_props
  as.data.frame(as.list(nmu_dast))
}
nmu_dast <- map_dfr(nmus, nmu_dast_summary)
rownames(nmu_dast) <- nmus
  # eye-balling observations:
    # does not appear to be consistent ordering across DAST questions, but
    # 12 (COD) is lowest in all but DAST_1 and DAST_3
    # 3 (METH) is highest most frequently -> five out of ten questions
    # suggests that DAST is better at identifying NMU of some drugs than others

# NMU VERSUS USE + DEMOGRAPHICS ----
dem_use_nmu_summary <- function(dem_column_name) {
  us18 %>%
    select(dem_column = dem_column_name, nmus,
           str_replace(nmus, "_NMU", "_USE")) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    group_by(dem_column) %>%
    summarize(fent = sum(FENT_NMU) / sum(FENT_USE),
              bup = sum(BUP_NMU) / sum(BUP_USE),
              meth = sum(METH_NMU) / sum(METH_USE),
              morph = sum(MORPH_NMU) / sum(MORPH_USE),
              oxy = sum(OXY_NMU) / sum(OXY_USE),
              oxym = sum(OXYM_NMU) / sum(OXYM_USE),
              tram = sum(TRAM_NMU) / sum(TRAM_USE),
              tap = sum(TAP_NMU) / sum(TAP_USE),
              hyd = sum(HYD_NMU) / sum(HYD_USE),
              hydm = sum(HYDM_NMU) / sum(HYDM_USE),
              suf = sum(SUF_NMU) / sum(SUF_USE),
              cod = sum(COD_NMU) / sum(COD_USE),
              dihy = sum(DIHY_NMU) / sum(DIHY_USE),
              benz = sum(BENZ_NMU) / sum(BENZ_USE),
              stim = sum(STIM_NMU) / sum(STIM_USE),
              thc = sum(THC_NMU) / sum(THC_USE),
              ktm = sum(KTM_NMU) / sum(KTM_USE))
}
dem_use_nmu <- map(dems, dem_use_nmu_summary)
names(dem_use_nmu) <- dems
  # eye-balling observations:
    # GENDER: 1 (male) higher than 2 (female) except STIM and THC
    # AGE10: order varies; mostly increasing trend with age
    # STDNT: 1 (yes) larger than (no) for all drugs
    # VET: 1 (yes) larger than 0 (no) for about half of the drugs;
      # 0 is larger for MORPH, OXY, HYD, COD, BENZ, STIM, THC, KTM
    # HEALTH: 1 (yes) higher than 0 (no) except STIM and THC
    # STATE: order varies but VT is largest for 7 drugs
    # HISPANIC: 1 (yes) largest for all drugs except THC
    # RACE: order varies; 5 smallest for 9 drugs
    # INCOME: order varies; 11 smallest for 11 drugs; 9 largest for 9 drugs
    # MARITAL: order varies by drug; 2 (widowed) is smallest most often
    # EDU: order varies by drug; 8 (doctorate) largest for 12 drugs
    # PREG: 1 (yes) is larger than 0 (no) for all drugs
      # note this is a flip of purely demographic against NMU
