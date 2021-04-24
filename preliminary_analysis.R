library(tidyverse)
us18 <- read.csv("US/us_18Q1.csv") %>%
  select(-DATE, -QTIME, -START_DATE, -STATUS)

# QUESTION: DOES SECTION ORDER MATTER? ----
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
    apply(2, function(x) { x / dem_totals}) %>%
    apply(1, function(y) {y / nmu_props}) %>% t()
  rownames(dem_nmu_df) <- levels(as.factor(dem_variable))
  dem_nmu_df
}

# broad categories ----
dems <- c("DEM_GENDER", "DEM_AGE10", "DEM_STDNT", "DEM_VET", "DEM_HEALTH",
          "DEM_STATE", "DEM_HISPANIC", "DEM_RACE", "DEM_INCOME", "DEM_MARITAL",
          "DEM_EDU", "DEM_PREG") # missing for DEM_PREG = males
dem_nmu <- map(dems, dem_nmu_summary)
names(dem_nmu) <- dems

  # eye-balling observations:
  # GENDER: 1 (male) is ~3 times of 2 (female)
  # AGE10: 6 (oldest category) < 5 < 4 < 1 (youngest) < 3 < 2 for all drugs
  # STDNT: 1 (yes) is ~2-6 times larger than 0 (no) for all drugs
  # VET: 1 (yes) tends to be larger than 0 (no), but generally comparable
  # HEALTH: 1 (yes) larger than 0 (no) for all drugs, up to >7 times as large
  # STATE: top and bottom states vary by drug
  # HISPANIC: 1 (yes) largest for all drugs, up to >5 as large as next largest
  # RACE: order varies by drug; generally 5 & 6 = smallest; 4 & 7 = largest
  # INCOME: order varies by drug; 11 always smallest; 9 usually largest
  # MARITAL: order varies by drug; 2 (widowed) always smallest;
    # largest is usually 4 (never married) and occasionally 1 (married)
  # EDU: order varies by drug; 8 (doctorate) is largest for all except THC
  # PREG: 0 (no) is larger than 1 (yes) for all drugs, up to >8 as large


