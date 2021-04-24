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
nmu_sums <- us18[, nmus] %>%
  mutate_all(~replace(., is.na(.), 0)) %>% # missing = no lifetime use
  colSums()
dem_nmu_summary <- function(dem_column_name) {
  dem_variable <- us18[[dem_column_name]]
  if (dem_column_name == "DEM_PREG") {
    dem_variable[is.na(dem_variable)] <- 0
  }
  dem_sums <- summary(as.factor(dem_variable))
  dem_nmu_df <-
    data.frame(matrix(nrow = length(dem_sums), ncol = length(nmu_sums)))
  for (i in seq_along(dem_sums)) {
    for (j in seq_along(nmu_sums)) {
      dem_nmu_df[i, j] <- nmu_sums[j] / dem_sums[i]
    }
  }
  names(dem_nmu_df) <- nmus
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
  # GENDER: very similar between 1 (male) and 2 (female)
  # AGE10: descending order for all drugs = 1 > 6 > 3 > 2 > 4 > 5
  # STDNT: 1 (yes) is ~10 times larger than 0 (no) for all drugs
  # VET: 1 (yes) is ~8-10 times larger than 0 (no) for all drugs except KTM
  # HEALTH: 1 (yes) is ~20 times larger than 0 (no) for all drugs
  # STATE: top three = 1 (AK) > 47 (VT) > 29 (ND);
    # bottom three = 5 (CA) < 10 (FL) < 44 (TX) for all drugs
  # HISPANIC: 0 << 1 << 9 (did not say); each << is ~ one order of magnitude
  # RACE: 4 > 5 > 7 > 6 > 3 > 2 > 1 for all drugs
  # INCOME: 10 < 1 < 3 < 2 < 5 < 4 < 7 < 6 < 11 < 9 < 8 for all drugs
  # MARITAL: 1 < 4 < 3 < 2 for all drugs; 2 (widowed) is much higher than all
    # others for some drugs e.g. OXY (>3 times higher than next highest)
  # EDU: 6 < 3 < 2 < 5 < 7 < 4 < 8 < 1 for all drugs; 1 (below high school) and
    # 8 (doctorate) are pretty comparable, 4 (trade school) is about half that
  # PREG: 1 (yes) is two orders of magnitude larger than 0 (no) for all drugs
