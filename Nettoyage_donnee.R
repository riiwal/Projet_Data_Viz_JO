library(readxl)
library(dplyr)

dta_insee <- read_xlsx("indic-stat-circonscriptions-legislatives-2022.xlsx",sheet = 1, skip = 7)
summary(dta_insee)

#On passe les données en numérique car l'importation ne s'est pas bien faites 
dta_insee[, -c(1,2)] <- lapply(dta_insee[, -c(1,2)], function(x) {
  as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", x))) 
})

dta_insee[,c(1,2)] <- lapply(dta_insee[,c(1,2)],as.factor)

# Selection des variables d'intérêts
dta_insee<-dta_insee[,c(1:4,7:13,19:21,37:50,54:55,59:63,91:98,118:123)]

# Selection des observations : France Métropolitaine
dta_insee <- dta_insee[2:540,]

# on retire les espaces pour permettre la jointure des tables
dta_insee$`Nom de la circonscription` <- gsub(" ", "", dta_insee$`Nom de la circonscription`)

# on importe les données resultats
result_2022 <- read_xlsx("resultats-par-niveau-cirlg-t1-france-entiere.xlsx",sheet = 1)

# selection de la france métropolitaine
result_2022 <- result_2022[1:539,] 
result_2022$`Libellé du département` <- gsub(" ", "", result_2022$`Libellé du département`)
# on créer la colonne qui servira pour faire la jointure
result_2022 %>% 
  mutate(`Nom de la circonscription` = ifelse(`Code de la circonscription`=="01",
                                            paste0(`Libellé du département`, "-", "1recirconscription"),
                                            paste0(paste0(`Libellé du département`, "-", as.numeric(`Code de la circonscription`),"ecirconscription"))))-> result_2022


# on crée le jeu de données d'analyses

donnees <- dta_insee %>%
  full_join(result_2022, by = "Nom de la circonscription")
summary(donnees)
