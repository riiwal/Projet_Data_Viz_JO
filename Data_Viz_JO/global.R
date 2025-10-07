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
library(FactoMineR)
library(factoextra)
library(patchwork)
library(scales)
library(forcats)
library("ggpubr")
library("ggrepel")
#library(ggiraph)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(visNetwork)
library(heatmaply)
library(png)
library(DT)


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