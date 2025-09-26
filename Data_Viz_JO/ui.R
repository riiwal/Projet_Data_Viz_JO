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

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  title = "Présidentielles 2022",
  tabPanel(
    title = "Données",
    navlistPanel(tabPanel(title = "Plot"),
    tabPanel(title = "Table", ),
    tabPanel(title = "History"))
    ,
    tabPanel(title = "Penguin's life")
  ),
  
  tabPanel(title = "Analyse Descriptive", "some charts"),
  tabPanel(title = "Visualisations", "some tables")
))
