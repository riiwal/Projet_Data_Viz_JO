library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("Exploration du dataset dtaf"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("theme", "Choisissez un thème :", 
                  choices = c("Démographie", "Activité","Education","CSP","Menages", "Logement", "Transport", "Accès", "Economie", "Votes"),
                  selected = "Démographie")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tableau", DTOutput("table")),
        tabPanel("Résumé", htmlOutput("summary"))
      )
    )
  )
)
