library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library("ggpubr")
library("ggrepel")
library(ggiraph)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(htmltools) 



#On créer un subset avec seulement les votes et les circonscription
top3<-dtaf[,c(3,56:67)] %>% 
  pivot_longer(cols=2:13,
               names_to = "candidat",
               values_to = "pourcentage")

top3$candidat <- as.factor(top3$candidat)

summary(top3)

#la j'ai les 3 premiers 
top3<-top3 %>% 
  group_by(codeCirconscription) %>% 
  slice_max(pourcentage, n = 3, with_ties = FALSE)

top3 <- top3 %>%
  mutate(candidat = recode(candidat,
                           "Arthaud_exp" = "Arthaud",
                           "Roussel_exp" = "Roussel",
                           "Macron_exp" = "Macron",
                           "Lassalle_exp" = "Lasalle",
                           "LePen_exp" = "LePen",
                           "Zemmour_exp" = "Zemmour",
                           "Melenchon_exp" = "Melenchon",
                           "Hidalgo_exp" = "Hidalgo",
                           "Jadot_exp" = "Jadot",
                           "Pecresse_exp" = "Pecresse",
                           "Poutou_exp" = "Poutou",
                           "DupontAignan_exp" = "DupontAignan"))


pal <- colorNumeric("Oranges", domain = dtaf$act_cho)


labels_top3 <- top3[,c(1,3,4)] %>%
  group_by(codeCirconscription) %>%
  summarise(top3_label = paste0(candidat, " : ", round(pourcentage, 1), "%", collapse = "<br>"))
labels_top3 <- as_tibble(labels_top3)

dtaf2 <- dtaf %>%
  left_join(labels_top3, by = "codeCirconscription")


######################################################

output$mymap <- renderLeaflet({
  leaflet(dtaf2) %>%
    addPolygons(
      fillColor = ~pal(act_cho),
      fillOpacity = 1,
      color = "darkgray",
      weight = 1,
      # Tooltip avec HTML
      label = lapply(
        paste0(
          dtaf2$nomDepartement, " ", dtaf2$nomCirconscription, ": ", round(dtaf2$act_cho, 2), "%<br>",
          ifelse(!is.na(dtaf2$top3_label), paste0("Top 3 :<br>", dtaf2$top3_label), "")
        ),
        HTML
      )
    ) %>% 
    addTiles(
      urlTemplate = "", 
      options = providerTileOptions(minZoom = 5, maxZoom = 11)
    ) %>% 
    setMapWidgetStyle(style = list(background = "transparent"))
  
})
#########################################################

# Joindre avec dtaf



leaflet(dtaf2) %>%
    addPolygons(
    fillColor = ~pal(act_cho),
    fillOpacity = 1,
    color = "darkgray",
    weight = 1,
    
    label = ~paste0(nomDepartement, " ", nomCirconscription, ": ", round(act_cho, 2))
  ) %>% 
  addTiles(urlTemplate="",options = providerTileOptions(minZoom=5,maxZoom=11)) %>% 
  setMapWidgetStyle(style = list(background="transparent")) #avoir un fond blanc



# Garder geometry.x et renommer en geometry
dtaf2 <- dtaf2 %>%
  select(-geometry.y) %>%
  rename(geometry = geometry.x) %>%
  st_as_sf()  # s'assurer que c'est bien un objet sf

