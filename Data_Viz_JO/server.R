#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)
library(forcats)
library(tidyverse)



shinyServer(function(input, output, session) {
  output$image <- renderImage( 
    { 
      list(src = "image_accueil_urne.png", height = "100%") 
    }, 
    deleteFile = FALSE 
  ) 
  source("server/server_descriptive.R", local = TRUE)
  source('Server/Server_carte.R',local=TRUE)
  source("Server/Server_ACP.R",local = TRUE)
  source("Server/Server_arbre.R",local = TRUE)
  source("server/server_heatmap.R",local = TRUE)
})
