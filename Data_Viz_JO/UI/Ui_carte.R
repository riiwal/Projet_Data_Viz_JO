library(shiny)
tagList(fluidPage(
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points")
))
