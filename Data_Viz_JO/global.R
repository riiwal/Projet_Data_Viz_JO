#packages utilisés ---
library(readxl)
library(dplyr)
library(stringr)
library(sf)
library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)
library(GGally)
library(corrplot)
library(RColorBrewer)
library(ggthemes)
library(factoextra)
library(patchwork)
library(scales)
library(forcats)

# nettoyage des données ---- 
dta_insee <- read_xlsx("indic-stat-circonscriptions-legislatives-2022.xlsx",sheet = 1, skip = 7)

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
# summary(donnees)


#Ajout des données pour faire la carte 
circo <- st_read("https://static.data.gouv.fr/resources/contours-geographiques-des-circonscriptions-legislatives/20240613-191506/circonscriptions-legislatives-p20.geojson")
circo <- circo[1:539,] # On prend que la métropole 

# Création d'une clé commune 

donnees$circo <- as.character(donnees$circo) # code de nos données repassé en charactères 


#On enlève le 0 en trop dans donnes pour faire matcher les codes 
donnees <- donnees %>%
  mutate(
    circo = paste0(
      str_sub(circo, 1, 2),   # 2 premiers caractères (département)
      str_sub(circo, 4)       # tout sauf le 3e caractère (le zéro en trop)
    )
  )

dtaf <- circo %>%
  left_join(donnees, by = c("codeCirconscription" = "circo"))


#On ajuste les variables Voix et Nom 
dtaf <- dtaf %>%
  mutate(across(starts_with("Voix"), ~ as.numeric(as.character(.))))
dtaf <- dtaf %>%
  mutate(across(starts_with("%"), ~ as.numeric(as.character(.))))
dtaf <- dtaf %>%
  mutate(across(starts_with("Nom"), ~ as.factor(as.character(.))))

#Ajout de la colonne gagnant 
ajouter_gagnant <- function(df) {
  # Colonnes des % Voix/Exp
  pct_cols <- paste0("% Voix/Exp", c("", 2:12))
  
  # Sélectionner les colonnes et convertir en numérique en gérant les NA
  pct_mat <- as.data.frame(lapply(df[pct_cols], function(col) suppressWarnings(as.numeric(as.character(col)))))
  
  # Trouver l'indice du max par ligne (en ignorant les NA)
  gagnant_index <- apply(pct_mat, 1, function(x) {
    if (all(is.na(x))) {
      return(NA_integer_)
    } else {
      return(which.max(x))
    }
  })
  
  # Colonnes des noms
  nom_cols <- paste0("Nom", c("", 2:12))
  nom_mat <- as.data.frame(lapply(df[nom_cols], as.character), stringsAsFactors = FALSE)
  
  # Récupérer le nom gagnant pour chaque ligne
  nom_gagnant <- mapply(function(i, row) {
    if (is.na(i)) return(NA_character_)
    return(row[[i]])
  }, gagnant_index, split(nom_mat, seq_len(nrow(nom_mat))), USE.NAMES = FALSE)
  
  # Ajouter colonne gagnant
  df$Gagnant <- nom_gagnant
  
  return(df)
}

dtaf <- ajouter_gagnant(dtaf)

#Renommer les % Voix/Exp

dtaf <- dtaf %>% 
  rename(Arthaud_exp = `% Voix/Exp`,
         Roussel_exp = `% Voix/Exp2`,
         Macron_exp = `% Voix/Exp3`,
         Lassalle_exp = `% Voix/Exp4`,
         LePen_exp = `% Voix/Exp5`,
         Zemmour_exp = `% Voix/Exp6`,
         Melenchon_exp = `% Voix/Exp7`,
         Hidalgo_exp = `% Voix/Exp8`,
         Jadot_exp = `% Voix/Exp9`,
         Pecresse_exp = `% Voix/Exp10`,
         Poutou_exp = `% Voix/Exp11`,
         DupontAignan_exp = `% Voix/Exp12`,
         
         
         
  )


dtaf<- dtaf %>% 
  rename(Abs_insc = `% Abs/Ins`,
         Vot_insc = `% Vot/Ins`,
         Blanc_vote =`% Blancs/Vot`,
         Nul_vote = `% Nuls/Vot`)
dtaf <- dtaf[,-c(5,53:59,61,63:64,66:67,69:77,79:84,86:91,93:98,100:105,107:112,114:119,121:126,128:133,135:140,142:147,149:154)]

summary(dtaf)

dtaf <- dtaf %>%
  mutate(across(c(1, 3, 68), as.factor))

