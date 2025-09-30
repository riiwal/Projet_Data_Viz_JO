library(shiny)
library(DT)

# navlistPanel(
#   tabPanel(title = "plot"),
#   tabPanel(title = "Table"),
#   tabPanel(title = "Résultats — Shiny"),
#   tabPanel(title = "Penguin's life")
# )

fluidPage(
  titlePanel("Exploration du dataset dtaf"),

  sidebarLayout(
    sidebarPanel(
      selectInput("theme", "Choisissez un thème pour le résumé :",
                  choices = c("Démographie", "Activité","Education","CSP","Menages", "Logement", "Transport", "Accès", "Economie", "Votes"),
                  selected = "Démographie")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Tableau", DTOutput("table")),
        tabPanel("Résumé", tableOutput("summary"))
      )
    )
  )
)
#library(shiny)


#shinyApp(ui = ui_data, server = server_data)
