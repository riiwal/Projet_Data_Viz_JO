library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)
library(ggrepel)
library(ggiraph)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(htmltools)

# Préparation des données top3 (une seule fois, en dehors)
top3 <- dtaf_loaded[, c(3, 56:67)] %>% 
  pivot_longer(cols = 2:13,
               names_to = "candidat",
               values_to = "pourcentage")

top3$candidat <- as.factor(top3$candidat)

# Garder les 3 premiers candidats par circonscription
top3 <- top3 %>% 
  group_by(codeCirconscription) %>% 
  slice_max(pourcentage, n = 3, with_ties = FALSE)

# Renommer les candidats
top3 <- top3 %>%
  mutate(candidat = recode(candidat,
                           "Arthaud_exp" = "Arthaud",
                           "Roussel_exp" = "Roussel",
                           "Macron_exp" = "Macron",
                           "Lassalle_exp" = "Lasalle",
                           "LePen_exp" = "Le Pen",
                           "Zemmour_exp" = "Zemmour",
                           "Melenchon_exp" = "Mélenchon",
                           "Hidalgo_exp" = "Hidalgo",
                           "Jadot_exp" = "Jadot",
                           "Pecresse_exp" = "Pécresse",
                           "Poutou_exp" = "Poutou",
                           "DupontAignan_exp" = "Dupont-Aignan"))

# Créer les labels top3 (une seule fois)
labels_top3 <- top3 %>%
  group_by(codeCirconscription) %>%
  summarise(top3_label = paste0(candidat, " : ", round(pourcentage, 1), "%", 
                                collapse = "<br>"),
            .groups = "drop")

# Render de la carte
output$mymap <- renderLeaflet({
  
  # Joindre les données top3
  dtaf2 <- dtaf_loaded %>%
    left_join(st_drop_geometry(labels_top3), by = "codeCirconscription")
  
  # Créer la palette selon la sélection
  pal <- colorNumeric("Oranges", domain = dtaf2[[input$select]])
  
  # Créer les labels HTML
  labels <- lapply(
    paste0(
      "<b>", dtaf2$nomDepartement, " - ", dtaf2$nomCirconscription, "</b><br>",
      "<b>", names(dtaf2)[which(names(dtaf2) == input$select)], " : </b>",
      round(dtaf2[[input$select]], 2), "%<br>",
      ifelse(!is.na(dtaf2$top3_label), 
             paste0("<br><b>Top 3 :</b><br>", dtaf2$top3_label), 
             "")
    ),
    HTML
  )
  
  # Créer la carte Leaflet
  leaflet(dtaf2, options = leafletOptions(maxZoom = 11, minZoom = 5, zoom=5)) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(dtaf2[[input$select]]),
      fillOpacity = 0.7,
      color = "darkgray",
      weight = 1,
      opacity = 1,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#333",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      pal = pal, 
      values = ~dtaf2[[input$select]],
      title = "Pourcentage (%)",
      position = "bottomright",
      opacity = 0.7
    ) %>%
    setView(lng = 2.5, lat = 46.5, zoom = 6) %>%
    clearTiles() %>% 
    setMaxBounds( lng1 = -5.142222,  
                  lat1 = 41.333740,  
                  lng2 = 9.560000,    
                  lat2 = 51.089062)
 
     })