library(shiny)
tagList(fluidPage(
  selectInput( 
    "select", 
    "Choissisez une variable à afficher sur la carte:", 
    list("Chomâge" = "actcho", "% de propriétaire" = "proprio", "% agriculteur" = "act_agr") 
  ), 
  leafletOutput("mymap"),
  p(),
 
))
