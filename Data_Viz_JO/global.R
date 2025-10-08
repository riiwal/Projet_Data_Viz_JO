#packages utilisés ---
library(readxl) # lecture de fichier 
library(dplyr) # manipulation de données
library(stringr) # manipulation de texte
library(sf) # donnée géométriques
library(shiny)
library(bslib) # theme shiny
library(ggplot2) # graphiques
library(tidyverse) # travail dans l'environnement tidyverse
library(RColorBrewer) # palettes de couleurs
library(ggthemes) # theme graphiques
library(FactoMineR) # ACP
library(factoextra) # graphiques ACP
library(scales) # gradient
library(leaflet) # carte
library(leaflet.extras) # carte
library(htmltools) # objet html pour la carte
library(visNetwork) # arbre
library(heatmaply) # heatmap
library(png) # importation d'image
library(DT) # datatable
library(rpart) # arbre
library(visNetwork) # arbre
library(rpart.plot) # arbre
#library(sparkline)
#library(shinyWidgets)


dtaf_loaded <- readRDS("data/dtaf.RDS")

# Base géo UNIQUE et propre (circonscriptions)
dtaf_base <- dtaf_loaded %>%
  st_make_valid() %>%
  st_transform(2154) %>%
  mutate(codeCirconscription = as.character(codeCirconscription))


theme_presidentielles <- bs_theme(
  version = 5,                
  bootswatch = "flatly",
  bg = "white",             # gris très clair pour les fonds
  fg = "#1B1F3B",             # bleu nuit pour le texte principal
  primary = "#1B1F3B",        # bleu République
  secondary = "#EF4135",      # rouge République
  info = "#1B1F3B",           # bleu plus vif pour éléments interactifs
  success = "#EF4135",        # vert clair pour indicateurs positifs
  warning = "#F1C40F",        # jaune doré
  danger = "#E74C3C",         # rouge plus doux pour alertes
  
  border_radius = "0.5rem",   # coins légèrement arrondis
  "navbar-bg" = "#0B3D91",    # bleu foncé républicain pour la barre du haut
  "navbar-fg" = "white",
  "navbar-brand-color" = "white",
  "navbar-light-color" = "white",
  "navbar-light-hover-color" = "#FCC780" # accent doré au survol
  )