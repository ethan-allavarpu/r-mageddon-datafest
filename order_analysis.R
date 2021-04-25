
library(tidyverse)
us18 <- read.csv("US/us_18Q1.csv") %>%
  select(-DATE, -QTIME, -START_DATE, -STATUS)

  # demographics: proportion of section order v. population proportion ----

sections <- c("FENT", "BUP", "METH", "MORPH", "OXY", "OXYM", "TRAM", "TAP",
              "HYD", "HYDM", "SUF", "COD", "DIHY")
dems <- c("DEM_GENDER", "DEM_AGE10", "DEM_STDNT", "DEM_VET", "DEM_HEALTH",
          "DEM_STATE", "DEM_HISPANIC", "DEM_RACE", "DEM_INCOME", "DEM_MARITAL",
          "DEM_EDU", "DEM_PREG") # missing for DEM_PREG = males

check_dem_order <- function(dem_name) {
  df <- us18 %>%
    select(DEM = dem_name, str_c("ORDER_", sections)) %>%
    filter(!is.na(DEM)) # remove males from DEM_PREG
  by_dem_range <- df %>%
    group_by(DEM) %>%
    summarize(FENT = mean(ORDER_FENT), BUP = mean(ORDER_BUP),
              METH = mean(ORDER_METH), MORPH = mean(ORDER_MORPH),
              OXY = mean(ORDER_OXY), OXYM = mean(ORDER_OXYM),
              TRAM = mean(ORDER_TRAM), TAP = mean(ORDER_TAP),
              HYD = mean(ORDER_HYD), HYDM = mean(ORDER_HYDM),
              SUF = mean(ORDER_SUF), COD = mean(ORDER_COD),
              DIHY = mean(ORDER_DIHY), .groups = "drop") %>%
    select(-DEM)
  by_dem_range_min <- apply(by_dem_range, 2, min)
  by_dem_range_max <- apply(by_dem_range, 2, max)
  by_dem_range_output <-
    data.frame(MIN = by_dem_range_min, MAX = by_dem_range_max)
  by_order_range <- map_df(str_c("ORDER_", sections), function(section) {
    all_drugs <- tapply(df$DEM, df[[section]], mean, na.rm = TRUE)
    data.frame(MIN = min(all_drugs), MAX = max(all_drugs))
  })
  list(by_dem = by_dem_range_output, by_order = by_order_range,
       pop_dem_mean = mean(as.integer(as.factor(df$DEM))))
}
dem_order <- map(dems, check_dem_order)
names(dem_order) <- dems

  # order by section calculations ----

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

  # order by section bootstrap ----

set.seed(1)
randomized_section <- replicate(n = 1000, {
  randomized_data <- map(sections, function(section) {
    section_order <- sample(us18[[str_c("ORDER_", section)]], 30007)
    section_use <- as.logical(us18[[str_c(section, "_USE")]])
    data.frame(order = section_order, use = section_use)
  })
  order_means <- sapply(seq_len(13), function(i) {
    drug <- randomized_data[[i]]
    (drug %>%
        group_by(order) %>%
        summarize(prop = mean(use)) %>%
        arrange(desc(prop)))$order
  }) %>%
    rowMeans()
  sum((order_means - 7)^2)
})

observed_section <- order_section %>%
  select(contains("ord")) %>%
  apply(MARGIN = 1, FUN = mean)

mean(randomized_section >= sum((observed_section - 7)^2)) # p-value = 0
