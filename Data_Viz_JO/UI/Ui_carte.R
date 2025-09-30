library(shiny)
tagList(fluidPage(
  fluidRow(column(
    width = 12,
    offset = 1,
    h1("Carte de Riwal", align = "center")),
  fluidRow(column(3,selectInput( 
    "select", 
    "Choissisez une variable à afficher sur la carte:", 
    list("Chomâge" = "actcho", "% de propriétaire" = "proprio", "% agriculteur" = "act_agr") 
  )), 
  column(9,leafletOutput("mymap", height = 550),
  ))
 
)))
