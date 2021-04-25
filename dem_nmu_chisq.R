
library(tidyverse)
us18 <- read.csv("US/us_18Q1.csv") %>%
  select(-DATE, -QTIME, -START_DATE, -STATUS)

dem_nmu_chisq_test <- function(dem_name, nmu_name) {
  dem_nmu <- us18 %>%
    select(dem = dem_name, nmu = nmu_name, WT) %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    group_by(dem) %>%
    summarize(nmu_count = sum(nmu), dem_count = n())
  chisq.test(dem_nmu$nmu_count, p = dem_nmu$dem_count / 30007,
             simulate.p.value = TRUE)$p.value
}

dems <- c("DEM_GENDER", "DEM_AGE10", "DEM_STDNT", "DEM_VET", "DEM_HEALTH",
          "DEM_STATE", "DEM_HISPANIC", "DEM_RACE", "DEM_INCOME", "DEM_MARITAL",
          "DEM_EDU", "DEM_PREG") # missing for DEM_PREG = males
nmus <- str_subset(names(us18), "NMU\\b")

dem_nmu_chisq_pval <-
  data.frame(matrix(nrow = length(dems), ncol = length(nmus)))
for (i in seq_along(dems)) {
  for (j in seq_along(nmus)) {
    dem_nmu_chisq_pval[i, j] <- dem_nmu_chisq_test(dems[i], nmus[j])
  }
}
names(dem_nmu_chisq_pval) <- str_replace(nmus, "_NMU", "")
dem_nmu_chisq_pval <- cbind(DEM = str_replace(dems, "DEM_", ""),
                            dem_nmu_chisq_pval)

dem_nmu_chisq_pval %>%
  pivot_longer(!contains("DEM"), names_to = "NMU", values_to = "PVAL") %>%
  ggplot() + geom_tile(aes(DEM, NMU, fill = PVAL)) +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "seagreen",
                       midpoint = 0.1, name = "p-value")

# sum(dem_nmu_chisq_pval[, -1] < (0.05 / (12 * 17))) returns 0
# min(dem_nmu_chisq_pval[, -1]) returns 0.0004997501 -> * (17 * 12) = 0.101949
