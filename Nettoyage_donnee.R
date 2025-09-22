library(readxl)
library(dplyr)
dta_insee <- read_xlsx("indic-stat-circonscriptions-legislatives-2022.xlsx",sheet = 1, skip = 7)
summary(dta_insee)

dta_insee[, -c(1,2)] <- lapply(dta_insee[, -c(1,2)], function(x) {
  as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", x))) 
})

composition <- read_xlsx("circo_composition.xlsx",sheet = "table")

donnees <- composition %>%
  left_join(dta_insee, by = "circo")

summary(donnees)
