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


dtaf_loaded <- readRDS("data/dtaf.RDS")

# Base géo 
dtaf_base <- dtaf_loaded %>%
  st_make_valid() %>%
  st_transform(2154) %>%
  mutate(codeCirconscription = as.character(codeCirconscription))

# Thème de l'application
theme_presidentielles <- bs_theme(
  version = 5,                
  bootswatch = "flatly",    # Base visuelle 
  bg = "white",             # Couleur de fond globale de l'app
  fg = "#1B1F3B",           # texte principal
  primary = "#1B1F3B",      # couleur primaire
  secondary = "#EF4135",    # couleur secondaire
  info = "#1B1F3B",         # éléments interactifs
  success = "#EF4135",      
  warning = "#F1C40F",        
  danger = "#E74C3C",       
  border_radius = "0.5rem",   # coins légèrement arrondis
  "navbar-bg" = "#0B3D91",    # fond de la navbar
  "navbar-fg" = "white",      # couleur du texte de la navbar
  "navbar-brand-color" = "white", # Couleur du titre
  "navbar-light-color" = "white", # couleur des liens
  "navbar-light-hover-color" = "#FCC780" #survol
  )