#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)
library(forcats)
library(tidyverse)

shinyUI(tagList(
  # CSS + bandeau global
  tags$head(tags$style(HTML("
    .flagbar {
      position: fixed;
      top: 0; left: 0;
      width: 100%;
      height: 10px;                     /* épaisseur du bandeau */
      z-index: 1000;
      background: linear-gradient(
        90deg,
        #0055A4 0%,    #0055A4 33.333%,
        #FFFFFF 33.333%, #FFFFFF 66.666%,
        #EF4135 66.666%, #EF4135 100%
      );
      box-shadow: 0 1px 3px rgba(0,0,0,.18);
    }
    /* décale le contenu pour ne pas passer sous le bandeau */
    body { padding-top: 12px; }
  "))),
  div(class = "flagbar"),
  
  navbarPage(title = "Présidentielles 2022", theme = theme_presidentielles, inverse=TRUE,fluid = TRUE,
             tabPanel(
               title = "Accueil",
               plotOutput("image_fond_accueil", width = "100%", height = "700px")
             ),
             tabPanel(title = "Données",  source("UI/UI_data_summ.R", local = TRUE)$value),
             tabPanel(title = "Analyse Descriptive", source("ui/ui_descriptif.R", local = TRUE)$value),
             tabPanel(title = "Carte",   source("UI/Ui_carte.R", local = TRUE)$value),
             tabPanel(title = "ACP",     source("UI/Ui_ACP.R",   local = TRUE)$value),
             tabPanel(title = "Arbre",   source("UI/Ui_arbre.R", local = TRUE)$value),
             tabPanel(title = "Heatmap", source("ui/ui_heatmap.R", local = TRUE)$value)
  )
))
