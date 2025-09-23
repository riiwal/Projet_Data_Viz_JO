#Exploration by Riwal

#Test de carte --------
library(sf)
library(ggplot2)
library(stringr)
circo <- st_read("https://static.data.gouv.fr/resources/contours-geographiques-des-circonscriptions-legislatives/20240613-191506/circonscriptions-legislatives-p20.geojson")
circo <- circo[1:539,] # On prend que la métropole 

unique(nchar(circo$codeCirconscription))   # codes du geojson
donnees$circo <- as.character(donnees$circo)
unique(nchar(donnees$circo)) # codes de ton CSV
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

## ---------------
class(dtaf$Nom)
levels(dtaf$Nom)

dtaf$Nom<-as.factor(dtaf$Nom)

ggplot(dtaf) +
  geom_sf(aes(fill=Nom), color = "white") +
  coord_sf(xlim = c(-6, 10), ylim = c(41, 52)) +
  theme_minimal() +
  labs(title = "Circonscriptions par numéro (France métropolitaine)")

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


mon_dataframe <- ajouter_gagnant(dtaf)
ggplot(mon_dataframe) +
  geom_sf(aes(fill=Gagnant), color = "white") +
  coord_sf(xlim = c(-6, 10), ylim = c(41, 52)) +
  theme_minimal() +
  labs(title = "Circonscriptions par numéro (France métropolitaine)")

names(mon_dataframe)

#Taux d'absentéisme 
ggplot(mon_dataframe) +
  geom_sf(aes(fill=`% Abs/Ins`), color = "white") +
  coord_sf(xlim = c(-6, 10), ylim = c(41, 52)) +
  theme_minimal() +
  labs(title = "Taux d'absentéisme")+
  scale_fill_continuous(low="#bcacd4",high="#18047e")


#
ggplot(mon_dataframe)+
  geom_point()+
  aes(x=modtrans_voit,y=`% Voix/Exp7`)+
  ggtitle("Nb de gens votant Ecologie en fonction du nb de gens utilisant la voiture")

ggplot(mon_dataframe)+
  geom_point()+
  aes(x=modtrans_voit,y=`% Voix/Exp5`)+
  ggtitle("Nb de gens votant Le Pen en fonction du nb de gens utilisant la voiture")

ggplot(mon_dataframe)+
  geom_point()+
  aes(x=pop_rur_non_periu,y=`% Voix/Exp5`)+
  ggtitle("Nb de gens votant Le Pen en fonction de la part de pop étant population rurale ")

ggplot(mon_dataframe)+
  geom_point()+
  aes(x=pop_rur_non_periu,y=`% Voix/Exp7`)+
  ggtitle("Nb de gens votant Jadot en fonction de la part de pop étant population rurale ")




