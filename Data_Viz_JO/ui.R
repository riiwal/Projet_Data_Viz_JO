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



shinyUI(navbarPage(title = "Présidentielles 2022",
  tabPanel(
  title = "Accueil",
  plotOutput("image_fond_accueil", width = "100%", height = "700px")),
  
  tabPanel(title = "Données",source("UI/UI_data_summ.R",local = TRUE)$value),
  # tabPanel(title = "Analyse Descriptive", source("ui/ui_descriptif.R", local = TRUE)$value),
  # tabPanel(title = "Carte", source("UI/Ui_carte.R", local=TRUE)$value),
  # tabPanel(title = "ACP", source("UI/Ui_ACP.R", local=TRUE)$value),
  tabPanel(title = "Arbre", source("UI/Ui_arbre.R", local=TRUE)$value),
  tabPanel(title = "Heatmap", source("ui/ui_heatmap.R",local = TRUE)$value)
))
