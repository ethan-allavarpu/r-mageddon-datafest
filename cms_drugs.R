
library(tidyverse)
library(readxl)

us18 <- read.csv("US/us_18Q1.csv") %>%
  select(-DATE, -QTIME, -START_DATE, -STATUS)
nmus <- str_subset(names(us18), "NMU\\b")

cms18 <- read_xlsx("external_data/cms_drugs.xlsx", sheet = 2) %>%
  .[-(1:3), c(1:3, 28:35)]
colnames(cms18) <- # (w)asp = (weighted) average spending per
  c("brand_name", "generic_name", "manufacturer_count", "total_spending",
    "total_dosage_units", "total_claims", "total_beneficiaries",
    "wasp_dosage_unit", "asp_claim", "asp_beneficiary", "outlier_flag")
dc <- str_replace(nmus, "_NMU", "")
ugn <- unique(cms18$generic_name)
drug_type_conversion <-
  tibble(drug_code = dc[c(rep(1, 3), rep(2, 3), 3, rep(4, 3), rep(5, 5), 6,
                          rep(7, 2), 8, rep(9, 5), rep(10, 2), 11, rep(12, 7),
                          13, rep(14, 4), rep(15, 5), rep(16, 3), 17)],
         generic_name = # missing: sufentanil
           c(str_subset(ugn, "Fentanyl"), str_subset(ugn, "Buprenorphine"),
             "Methadone HCl", str_subset(ugn, "Morphine"),
             str_subset(ugn, "Oxycodone"), "Oxymorphone HCl",
             str_subset(ugn, "Tramadol"), "Tapentadol HCl",
             str_subset(ugn, "Hydrocodone"), str_subset(ugn, "Hydromorphone"),
             NA, str_subset(ugn, "Codeine"), "Acetaminophen/Caff/Dihydrocod",
             "Lorazepam", "Diazepam", "Temazepam", "Alprazolam", # benzodiazepines
             "Amphetamine", "Lisdexamfetamine Dimesylate", "Methylphenidate HCl",
             "Atomoxetine HCl", "Dextroamphetamine Sulfate", # stimulants
             "Nabilone", "Dronabinol", "Cannabidiol (Cbd)", # cannbinoids
             "Esketamine HCl"))
cms18 <- cms18 %>%
  left_join(drug_type_conversion, by = "generic_name")
