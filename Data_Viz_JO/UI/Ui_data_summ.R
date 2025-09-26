library(shiny)
library(DT)

ui_data <- fluidPage(
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
library(shiny)


shinyApp(ui = ui_data, server = server_data)
