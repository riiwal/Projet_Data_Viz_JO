#Exploration by Riwal

#Test de carte 
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

ggplot(circo) +
  geom_sf(aes(), color = "white") +
  coord_sf(xlim = c(-6, 10), ylim = c(41, 52)) +
  theme_minimal() +
  labs(title = "Circonscriptions par numéro (France métropolitaine)")
