library(readxl)
library(dplyr)
dta_insee <- read_xlsx("indic-stat-circonscriptions-legislatives-2022.xlsx",sheet = 1, skip = 7)
summary(dta_insee)

dta_insee[, -c(1,2)] <- lapply(dta_insee[, -c(1,2)], function(x) {
  as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", x))) 
})

dta_insee[,c(1,2)] <- lapply(dta_insee[,c(1,2)],as.factor)

# Selection des variables d'intérêts
dta_insee<-dta_insee[,c(1:4,7:13,19:21,37:50,54:55,59:63,91:98,118:123)]

# Selection des observations : France Métropolitaine
dta_insee <- dta_insee[1:540,]

summary(dta_insee)

