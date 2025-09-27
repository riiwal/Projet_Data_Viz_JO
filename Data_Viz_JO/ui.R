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



shinyUI(
  navbarPage(
    title = "Présidentielles 2022",
    tabPanel(
      title = "Données",
      navlistPanel(
        tabPanel(title="plot"),
        tabPanel(title = "Table"),
        tabPanel(title = "Résultats — Shiny"),
        tabPanel(title = "Penguin's life")
      )
    ),
    tabPanel(title = "Analyse Descriptive", source("ui/ui_descriptif.R", local = TRUE)$value),
    tabPanel(title = "Riwal", source("UI/Ui_carte.R", local=TRUE)$value)
  )
)

